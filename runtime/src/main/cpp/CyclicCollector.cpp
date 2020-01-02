/*
 * Copyright 2010-2019 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#ifndef KONAN_NO_THREADS
#define WITH_WORKERS 1
#endif

#include "Alloc.h"
#include "Atomic.h"
#include "KAssert.h"
#include "Memory.h"
#include "Natives.h"
#include "Porting.h"
#include "Types.h"

#if WITH_WORKERS
#include <pthread.h>
#endif

#if WITH_WORKERS
/**
 * Theory of operations:
 *
 * Kotlin/Native runtime allows incremental cyclic garbage collection for the shared objects,
 * such as `AtomicReference` and `FreezableAtomicReference` instances (further known as the atomic rootset).
 * We perform such analysis by iterating over the transitive closure of the atomic rootset, and computing
 * aggregated inner reference counter for rootset elements over this transitive closure.
 * Atomic rootset is built by maintaining the linked list of all atomic and freezable atomic references objects.
 * Elements whose transitive closure inner reference count matches the actual reference count are ones
 * belonging to the garbage cycles and thus can be discarded.
 * If during computations of the aggregated RC there were modifications in the reference counts of
 * elements of the atomic rootset:
 *   - if it is being increased, then someone already got an external reference to this element, thus we may not
 *    end up matching the inner reference count anyway
 *   - if it is being decreased and object become garbage, it will be collected next time
 * If transitive closure of the atomic rootset mutates, it could only happen via changing the atomics references,
 * as all others elements of this closure is frozen.
 * To avoid that we leave all locks associated with atomic references taken during the transitive closure walking.
 * All locks are released once we finish the transitive closure walking.
 * TODO: can we do better than that?
 * There are some complications in this algorithm due to delayed reference counting: namely we have to execute
 * callback on each worker which will take into account reference counts coming from the stack references of such
 * a worker.
 * It means, we could perform actual collection only after all registered workers completed rendevouz which performs
 * such accounting.
 */
namespace {

typedef KStdDeque<ObjHeader*> ObjHeaderDeque;

class Locker {
  pthread_mutex_t* lock_;
 public:
  Locker(pthread_mutex_t* alock): lock_(alock) {
    pthread_mutex_lock(lock_);
  }
  ~Locker() {
    pthread_mutex_unlock(lock_);
  }
};

template <typename func>
inline void traverseObjectFields(ObjHeader* obj, func process) {
  const TypeInfo* typeInfo = obj->type_info();
  if (typeInfo != theArrayTypeInfo) {
    for (int index = 0; index < typeInfo->objOffsetsCount_; index++) {
      ObjHeader** location = reinterpret_cast<ObjHeader**>(
          reinterpret_cast<uintptr_t>(obj) + typeInfo->objOffsets_[index]);
      process(location);
    }
  } else {
    ArrayHeader* array = obj->array();
    for (int index = 0; index < array->count_; index++) {
      process(ArrayAddressOfElementAt(array, index));
    }
  }
}

class CyclicCollector {
  pthread_mutex_t lock_;
  pthread_cond_t cond_;
  pthread_t gcThread_;

  int currentAliveWorkers_;
  bool shallCollectGarbage_;
  bool shallRunCollector_;
  volatile bool terminateCollector_;
  void* firstWorker_;
  KStdUnorderedMap<ObjHeader*, int> rootsRefCounts_;
  KStdUnorderedSet<void*> alreadySeenWorkers_;
  KStdVector<ObjHeader*> rootset_;
  KStdVector<ObjHeader**> toRelease_;

  int gcRunning_;

  int32_t currentTick_;
  int32_t lastTick_;
  int64_t lastTimestampUs_;

 public:
  CyclicCollector() {
     pthread_mutex_init(&lock_, nullptr);
     pthread_cond_init(&cond_, nullptr);
     pthread_create(&gcThread_, nullptr, gcWorkerRoutine, this);
  }

  ~CyclicCollector() {
    {
      Locker locker(&lock_);
      terminateCollector_ = true;
      pthread_cond_signal(&cond_);
    }
    // TODO: improve.
    while (atomicGet(&terminateCollector_)) {}
    for (auto* it: toRelease_) {
      konan::consolePrintf("deinit %p:%p\n",it, *it);
      ZeroHeapRef(it);
    }
    pthread_cond_destroy(&cond_);
    pthread_mutex_destroy(&lock_);
  }

  static void* gcWorkerRoutine(void* argument) {
    CyclicCollector* thiz = reinterpret_cast<CyclicCollector*>(argument);
    thiz->gcProcessor();
    return nullptr;
  }

  static void addAtomicRootCallback(void* argument, ObjHeader* obj) {
      CyclicCollector* thiz = reinterpret_cast<CyclicCollector*>(argument);
      thiz->addAtomicRoot(obj);
  }

  void addAtomicRoot(ObjHeader* root) {
    RuntimeAssert(isAtomicReference(root), "Must be in the atomic rootset");
    rootset_.push_back(root);
  }

  bool isAtomicReference(ObjHeader* obj) {
    return (obj->type_info()->flags_ & TF_LEAK_DETECTOR_CANDIDATE) != 0;
  }

  void gcProcessor() {
     {
       Locker locker(&lock_);
       KStdDeque<ObjHeader*> toVisit;
       KStdUnorderedSet<ObjHeader*> visited;
       while (!terminateCollector_) {
         pthread_cond_wait(&cond_, &lock_);
         if (shallRunCollector_) {
           atomicSet(&gcRunning_, 1);
           alreadySeenWorkers_.clear();
           GC_AtomicRootsWalk(addAtomicRootCallback, this);
           for (auto* root: rootset_) {
             traverseObjectFields(root, [&toVisit, &visited](ObjHeader** location) {
               ObjHeader* ref = *location;
               if (ref != nullptr && visited.count(ref) == 0) {
                 toVisit.push_back(ref);
               }
             });
             while (toVisit.size() > 0)  {
               auto* obj = toVisit.front();
               toVisit.pop_front();
               if (isAtomicReference(obj)) {
                 rootsRefCounts_[obj]++;
               }
               visited.insert(obj);
               traverseObjectFields(obj, [&toVisit, &visited](ObjHeader** location) {
                 ObjHeader* ref = *location;
                 if (ref != nullptr && visited.count(ref) == 0) {
                   toVisit.push_back(ref);
                 }
               });
             }
           }
           for (auto it: rootsRefCounts_) {
             konan::consolePrintf("for %p inner %d actual %d\n", it.first, it.second,
               it.first->container()->refCount());
             // All references are inner. Actually we compare number of counted
             // inner references - number of stack references with number of non-stack references.
             if (it.second == it.first->container()->refCount()) {
               traverseObjectFields(it.first, [this](ObjHeader** location) {
                  toRelease_.push_back(location);
               });
             }
           }
           rootsRefCounts_.clear();
           rootset_.clear();
           visited.clear();
           RuntimeAssert(toVisit.size() == 0, "Must be clear");
           atomicSet(&gcRunning_, 0);
           shallRunCollector_ = false;
         }
       }
       terminateCollector_ = false;
     }
     konan::consolePrintf("GC finished\n");
  }

  void addWorker(void* worker) {
    Locker lock(&lock_);
    // We need to identify the main thread to avoid calling longer running code
    // on the first worker, as we assume it being the UI thread.
    if (firstWorker_ == nullptr) firstWorker_ = worker;
    currentAliveWorkers_++;
  }

  void removeWorker(void* worker) {
    Locker lock(&lock_);
    // When exiting the worker - we shall collect the cyclic garbage here.
    shallCollectGarbage_ = true;
    rendezvouzLocked(worker);
    currentAliveWorkers_--;
  }

  // TODO: this mechanism doesn't allow proper handling of references passed from one stack
  // to another between rendezvouz points.
  void addRoot(ObjHeader* obj) {
    Locker lock(&lock_);
    rootsRefCounts_[obj] = 0;
  }

  void removeRoot(ObjHeader* obj) {
    Locker lock(&lock_);
    rootsRefCounts_.erase(obj);
  }

  bool checkIfShallCollect() {
    auto tick = atomicAdd(&currentTick_, 1);
    if (shallCollectGarbage_) return true;
    auto delta = tick - atomicGet(&lastTick_);
    if (delta > 10 || delta < 0) {
       auto currentTimestampUs = konan::getTimeMicros();
       if (currentTimestampUs - atomicGet(&lastTimestampUs_) > 10000) {
         Locker locker(&lock_);
         lastTick_ = currentTick_;
         lastTimestampUs_ = currentTimestampUs;
         shallCollectGarbage_ = true;
         return true;
       }
    }
    return false;
  }

  static void heapCounterCallback(void* argument, ObjHeader* obj) {
    CyclicCollector* collector = reinterpret_cast<CyclicCollector*>(argument);
    collector->countLocked(obj, 1);
  }

  static void stackCounterCallback(void* argument, ObjHeader* obj) {
      CyclicCollector* collector = reinterpret_cast<CyclicCollector*>(argument);
      collector->countLocked(obj, -1);
    }

  void countLocked(ObjHeader* obj, int delta) {
    if (isAtomicReference(obj)) {
      rootsRefCounts_[obj] += delta;
    }
  }

  void rendezvouzLocked(void* worker) {
    if (toRelease_.size() > 0) {
      for (auto* it: toRelease_) {
        ZeroHeapRef(it);
      }
      toRelease_.clear();
    }
    if (alreadySeenWorkers_.count(worker) > 0) {
      return;
    }
    GC_StackWalk(stackCounterCallback, this);
    alreadySeenWorkers_.insert(worker);
    if (alreadySeenWorkers_.size() == currentAliveWorkers_) {
       // All workers processed, initiate GC.
       shallRunCollector_ = true;
       pthread_cond_signal(&cond_);
     }
  }

  void rendezvouz(void* worker) {
    if (atomicGet(&gcRunning_) != 0 || !checkIfShallCollect()) return;
    Locker lock(&lock_);
    rendezvouzLocked(worker);
  }

  void scheduleGarbageCollect() {
    Locker lock(&lock_);
    shallCollectGarbage_ = true;
  }
};

CyclicCollector* cyclicCollector = nullptr;

}  // namespace

#endif  // WITH_WORKERS

void cyclicInit() {
#if WITH_WORKERS
  RuntimeAssert(cyclicCollector == nullptr, "Must be not yet inited");
  cyclicCollector = konanConstructInstance<CyclicCollector>();
#endif
}

void cyclicDeinit() {
#if WITH_WORKERS
  RuntimeAssert(cyclicCollector != nullptr, "Must be inited");
  konanDestructInstance(cyclicCollector);
#endif  // WITH_WORKERS
}

void cyclicAddWorker(void* worker) {
#if WITH_WORKERS
  cyclicCollector->addWorker(worker);
#endif  // WITH_WORKERS
}

void cyclicRemoveWorker(void* worker) {
#if WITH_WORKERS
  cyclicCollector->removeWorker(worker);
#endif  // WITH_WORKERS
}

void cyclicRendezvouz(void* worker) {
#if WITH_WORKERS
  cyclicCollector->rendezvouz(worker);
#endif  // WITH_WORKERS
}

void cyclicScheduleGarbageCollect() {
#if WITH_WORKERS
  cyclicCollector->scheduleGarbageCollect();
#endif  // WITH_WORKERS
}

void cyclicAddAtomicRoot(ObjHeader* obj) {
#if WITH_WORKERS
  cyclicCollector->addRoot(obj);
#endif  // WITH_WORKERS
}

void cyclicRemoveAtomicRoot(ObjHeader* obj) {
#if WITH_WORKERS
  cyclicCollector->removeRoot(obj);
#endif  // WITH_WORKERS
}
package org.jetbrains.kotlin.backend.konan.ir

import org.jetbrains.kotlin.backend.common.ir.addFakeOverrides
import org.jetbrains.kotlin.backend.common.ir.createParameterDeclarations
import org.jetbrains.kotlin.backend.konan.InteropBuiltIns
import org.jetbrains.kotlin.backend.konan.descriptors.enumEntries
import org.jetbrains.kotlin.backend.konan.descriptors.findPackage
import org.jetbrains.kotlin.backend.konan.descriptors.isFromInteropLibrary
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.descriptors.PackageFragmentDescriptor
import org.jetbrains.kotlin.descriptors.PropertyDescriptor
import org.jetbrains.kotlin.ir.builders.declarations.addField
import org.jetbrains.kotlin.ir.builders.declarations.addGetter
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.declarations.impl.IrFileImpl
import org.jetbrains.kotlin.ir.expressions.IrExpression
import org.jetbrains.kotlin.ir.expressions.impl.IrBlockBodyImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrEnumConstructorCallImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrInstanceInitializerCallImpl
import org.jetbrains.kotlin.ir.symbols.IrClassSymbol
import org.jetbrains.kotlin.ir.symbols.IrEnumEntrySymbol
import org.jetbrains.kotlin.ir.symbols.IrSymbol
import org.jetbrains.kotlin.ir.util.*
import org.jetbrains.kotlin.ir.util.NaiveSourceBasedFileEntryImpl
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.psi2ir.generators.DeclarationGenerator
import org.jetbrains.kotlin.psi2ir.generators.EnumClassMembersGenerator
import org.jetbrains.kotlin.psi2ir.generators.GeneratorContext
import org.jetbrains.kotlin.resolve.descriptorUtil.getAllSuperClassifiers
import org.jetbrains.kotlin.resolve.descriptorUtil.module
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.utils.addToStdlib.firstNotNullResult

// TODO: Consider unification with FunctionIrProvider
internal class IrProviderForCEnumStubs(
        private val symbolTable: SymbolTable,
        private val context: GeneratorContext,
        private val interopBuiltIns: InteropBuiltIns
) : IrProvider {

    private val typeTranslator = context.typeTranslator
    private fun KotlinType.toIrType() = typeTranslator.translateType(this)

    private val enumClassMembersGenerator =
            EnumClassMembersGenerator(DeclarationGenerator(context))

    private val filesMap = mutableMapOf<PackageFragmentDescriptor, IrFile>()
    private val enumClasses = mutableListOf<IrClass>()

    var module: IrModuleFragment? = null
        set(value) {
            if (value == null)
                error("Provide a valid non-null module")
            if (field != null)
                error("Module has already been set")
            field = value
            value.files += filesMap.values
            enumClasses.forEach { it.addFakeOverrides() }
        }

    override fun getDeclaration(symbol: IrSymbol): IrDeclaration? = when {
        !symbol.descriptor.module.isFromInteropLibrary() ->
            null
        symbol is IrEnumEntrySymbol ->
            provideIrEnumEntry(symbol)
        symbol is IrClassSymbol && symbol.isCEnumSymbol(interopBuiltIns) ->
            provideIrClassForCEnum(symbol.descriptor)
        else ->
            null
    }

    // TODO: Do we need to declare parental IrClass?
    private fun provideIrEnumEntry(symbol: IrEnumEntrySymbol): IrEnumEntry {
        val enumClassDescriptor = symbol.descriptor.containingDeclaration as ClassDescriptor
        val irClassSymbol = symbolTable.referenceClass(enumClassDescriptor)
        val enumClassIr = if (!irClassSymbol.isBound) {
            provideIrClassForCEnum(enumClassDescriptor)
        } else {
            irClassSymbol.owner
        }
        return enumClassIr.findDeclaration { symbol.descriptor.name == it.name }
                ?: error("${symbol.descriptor.name} is not found in enum!")
    }

    private fun provideIrClassForCEnum(descriptor: ClassDescriptor): IrClass =
            symbolTable.declareClass(
                    startOffset = SYNTHETIC_OFFSET,
                    endOffset = SYNTHETIC_OFFSET,
                    origin = IrDeclarationOrigin.IR_EXTERNAL_DECLARATION_STUB,
                    descriptor = descriptor
            ).also { enumIrClass ->
                context.symbolTable.withScope(descriptor) {
                    descriptor.typeConstructor.supertypes.mapTo(enumIrClass.superTypes) {
                        it.toIrType()
                    }
                    enumIrClass.createParameterDeclarations()
                    enumClassMembersGenerator.generateSpecialMembers(enumIrClass)
                    enumIrClass.addMember(createPrimaryConstructor(descriptor).also {
                        it.parent = enumIrClass
                    })
                    val valuePropertyDescriptor: PropertyDescriptor = descriptor.defaultType.memberScope
                            .getContributedDescriptors()
                            .filterIsInstance<PropertyDescriptor>()
                            .filter { it.name.identifier == "value" }
                            .firstOrNull()
                            ?: error("No `CEnum.value` property!")
                    val valueIrField = enumIrClass.addField("value", valuePropertyDescriptor.type.toIrType()).also {
                        it.parent = enumIrClass
                    }
                    enumIrClass.addMember(createValueProperty(enumIrClass, valuePropertyDescriptor, valueIrField))
                    descriptor.defaultType.memberScope.getContributedDescriptors()
                    descriptor.enumEntries.mapTo(enumIrClass.declarations) { entryDescriptor ->
                        createEnumEntry(descriptor, entryDescriptor).also { it.parent = enumIrClass }
                    }
                }

                val packageFragmentDescriptor = descriptor.findPackage()
                val file = filesMap.getOrPut(packageFragmentDescriptor) {
                    IrFileImpl(NaiveSourceBasedFileEntryImpl("CEnums"), packageFragmentDescriptor).also {
                        this@IrProviderForCEnumStubs.module?.files?.add(it)
                    }
                }
                enumIrClass.parent = file
                file.declarations += enumIrClass
            }

    private fun createValueProperty(irClass: IrClass, propertyDescriptor: PropertyDescriptor, valueIrField: IrField): IrProperty {
        val irProperty = symbolTable.declareProperty(
                startOffset = SYNTHETIC_OFFSET,
                endOffset = SYNTHETIC_OFFSET,
                origin = IrDeclarationOrigin.IR_EXTERNAL_DECLARATION_STUB,
                descriptor = propertyDescriptor
        )
        irProperty.parent = irClass
        irProperty.backingField = valueIrField
        irProperty.addGetter {
            returnType = propertyDescriptor.type.toIrType()
        }
        return irProperty
    }

    private fun createEnumEntry(enumDescriptor: ClassDescriptor, entryDescriptor: ClassDescriptor): IrEnumEntry {
        return symbolTable.declareEnumEntry(
                startOffset = SYNTHETIC_OFFSET,
                endOffset = SYNTHETIC_OFFSET,
                origin = IrDeclarationOrigin.IR_EXTERNAL_DECLARATION_STUB,
                descriptor = entryDescriptor
        ).also { enumEntry ->
            enumEntry.initializerExpression = IrEnumConstructorCallImpl(
                    startOffset = SYNTHETIC_OFFSET,
                    endOffset = SYNTHETIC_OFFSET,
                    type = context.irBuiltIns.unitType,
                    symbol = context.symbolTable.referenceConstructor(enumDescriptor.unsubstitutedPrimaryConstructor!!),
                    typeArgumentsCount = 0 // enums can't be generic
            ).apply {
                putValueArgument(0, extractEnumEntryValue(entryDescriptor))
            }
        }
    }

    private fun extractEnumEntryValue(entryDescriptor: ClassDescriptor): IrExpression {
        val base = FqName("kotlinx.cinterop.internal.CEnumEntryValue")
        val types = setOf(
                "Byte", "Short", "Int", "Long",
                "UByte", "UShort", "UInt", "ULong"
        )
        fun extractValue(type: String): IrExpression? {
            val value = entryDescriptor.annotations
                    .findAnnotation(base.child(Name.identifier(type)))
                    ?.allValueArguments
                    ?.getValue(Name.identifier("value"))
                    ?: return null
            return context.constantValueGenerator.generateConstantValueAsExpression(SYNTHETIC_OFFSET, SYNTHETIC_OFFSET, value)
        }

        return types.firstNotNullResult(::extractValue) ?: error("TODO")
    }

    private fun createPrimaryConstructor(descriptor: ClassDescriptor): IrConstructor {
        val irBlockBody = IrBlockBodyImpl(
                startOffset = SYNTHETIC_OFFSET,
                endOffset = SYNTHETIC_OFFSET
        )
        irBlockBody.statements += generateEnumSuperConstructorCall(descriptor)
        irBlockBody.statements += IrInstanceInitializerCallImpl(
                startOffset = SYNTHETIC_OFFSET,
                endOffset = SYNTHETIC_OFFSET,
                classSymbol = context.symbolTable.referenceClass(descriptor),
                type = context.irBuiltIns.unitType
        )
        val irConstructor = context.symbolTable.declareConstructor(
                startOffset = SYNTHETIC_OFFSET,
                endOffset = SYNTHETIC_OFFSET,
                origin = IrDeclarationOrigin.IR_EXTERNAL_DECLARATION_STUB,
                descriptor = descriptor.unsubstitutedPrimaryConstructor!!
        )
        irConstructor.body = irBlockBody
        irConstructor.returnType = irConstructor.descriptor.returnType.toIrType()
        return irConstructor
    }

    private fun generateEnumSuperConstructorCall(
            classDescriptor: ClassDescriptor
    ): IrEnumConstructorCallImpl {
        val enumConstructor = context.builtIns.enum.constructors.single()
        return IrEnumConstructorCallImpl(
                startOffset = SYNTHETIC_OFFSET,
                endOffset = SYNTHETIC_OFFSET,
                type = context.irBuiltIns.unitType,
                symbol = context.symbolTable.referenceConstructor(enumConstructor),
                typeArgumentsCount = 1 // kotlin.Enum<T> has a single type parameter
        ).apply {
            putTypeArgument(0, classDescriptor.defaultType.toIrType())
        }
    }
}

private fun IrSymbol.isCEnumSymbol(interopBuiltIns: InteropBuiltIns): Boolean {
    if (this !is IrClassSymbol) return false
    return interopBuiltIns.cEnum in this.descriptor.getAllSuperClassifiers()
}
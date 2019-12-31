package kotlinx.cinterop.internal

@Target(AnnotationTarget.CLASS)
@Retention(AnnotationRetention.BINARY)
annotation class CStruct(val spelling: String)

@Target(
        AnnotationTarget.FUNCTION,
        AnnotationTarget.PROPERTY_GETTER,
        AnnotationTarget.PROPERTY_SETTER
)
@Retention(AnnotationRetention.BINARY)
public annotation class CCall(val id: String) {
    @Target(AnnotationTarget.VALUE_PARAMETER)
    @Retention(AnnotationRetention.BINARY)
    annotation class CString

    @Target(AnnotationTarget.VALUE_PARAMETER)
    @Retention(AnnotationRetention.BINARY)
    annotation class WCString

    @Target(AnnotationTarget.FUNCTION)
    @Retention(AnnotationRetention.BINARY)
    annotation class ReturnsRetained

    @Target(AnnotationTarget.FUNCTION)
    @Retention(AnnotationRetention.BINARY)
    annotation class ConsumesReceiver

    @Target(AnnotationTarget.VALUE_PARAMETER)
    @Retention(AnnotationRetention.BINARY)
    annotation class Consumed
}

// TODO: What is its target?
public class CEnumEntryValue {
    @Retention(AnnotationRetention.BINARY)
    annotation class Byte(val value: kotlin.Byte)
    @Retention(AnnotationRetention.BINARY)
    annotation class Short(val value: kotlin.Short)
    @Retention(AnnotationRetention.BINARY)
    annotation class Int(val value: kotlin.Int)
    @Retention(AnnotationRetention.BINARY)
    annotation class Long(val value: kotlin.Long)
    @Retention(AnnotationRetention.BINARY)
    annotation class UByte(val value: kotlin.UByte)
    @Retention(AnnotationRetention.BINARY)
    annotation class UShort(val value: kotlin.UShort)
    @Retention(AnnotationRetention.BINARY)
    annotation class UInt(val value: kotlin.UInt)
    @Retention(AnnotationRetention.BINARY)
    annotation class ULong(val value: kotlin.ULong)

}
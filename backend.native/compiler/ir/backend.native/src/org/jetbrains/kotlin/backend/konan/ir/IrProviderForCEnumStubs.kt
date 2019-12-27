package org.jetbrains.kotlin.backend.konan.ir

import org.jetbrains.kotlin.backend.konan.InteropBuiltIns
import org.jetbrains.kotlin.backend.konan.descriptors.enumEntries
import org.jetbrains.kotlin.backend.konan.descriptors.isFromInteropLibrary
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.expressions.impl.IrBlockBodyImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrEnumConstructorCallImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrInstanceInitializerCallImpl
import org.jetbrains.kotlin.ir.symbols.IrClassSymbol
import org.jetbrains.kotlin.ir.symbols.IrEnumEntrySymbol
import org.jetbrains.kotlin.ir.symbols.IrSymbol
import org.jetbrains.kotlin.ir.util.*
import org.jetbrains.kotlin.psi2ir.generators.DeclarationGenerator
import org.jetbrains.kotlin.psi2ir.generators.EnumClassMembersGenerator
import org.jetbrains.kotlin.psi2ir.generators.GeneratorContext
import org.jetbrains.kotlin.resolve.descriptorUtil.getAllSuperClassifiers
import org.jetbrains.kotlin.resolve.descriptorUtil.module
import org.jetbrains.kotlin.types.KotlinType

internal class IrProviderForCEnumStubs(
        private val symbolTable: SymbolTable,
        private val context: GeneratorContext,
        private val interopBuiltIns: InteropBuiltIns
) : IrProvider {

    private val typeTranslator = context.typeTranslator
    private fun KotlinType.toIrType() = typeTranslator.translateType(this)

    private val enumClassMembersGenerator =
            EnumClassMembersGenerator(DeclarationGenerator(context))

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

    // Do we need to declare parental IrClass?
    private fun provideIrEnumEntry(symbol: IrEnumEntrySymbol): IrEnumEntry {
        val enumClassDescriptor = symbol.descriptor.containingDeclaration as ClassDescriptor
        val irClassSymbol = symbolTable.referenceClass(enumClassDescriptor)
        val enumClassIr = if (!irClassSymbol.isBound) {
            provideIrClassForCEnum(enumClassDescriptor)
        } else {
            irClassSymbol.owner
        }
        return enumClassIr.findDeclaration { symbol.descriptor.name == it.name }
                ?: error("${symbol.descriptor.name } is not found in enum!")
    }

    private fun provideIrClassForCEnum(descriptor: ClassDescriptor): IrClass =
        symbolTable.declareClass(
                startOffset = SYNTHETIC_OFFSET,
                endOffset = SYNTHETIC_OFFSET,
                origin = IrDeclarationOrigin.IR_EXTERNAL_DECLARATION_STUB,
                descriptor = descriptor
        ).also { enumIrClass ->
            context.symbolTable.withScope(descriptor) {
                enumIrClass.thisReceiver = context.symbolTable.declareValueParameter(
                        startOffset = SYNTHETIC_OFFSET,
                        endOffset = SYNTHETIC_OFFSET,
                        origin = IrDeclarationOrigin.INSTANCE_RECEIVER,
                        descriptor = descriptor.thisAsReceiverParameter,
                        type = descriptor.thisAsReceiverParameter.type.toIrType()
                )
                enumClassMembersGenerator.generateSpecialMembers(enumIrClass)
                enumIrClass.addMember(createPrimaryConstructor(descriptor).also {
                    it.parent = enumIrClass
                })
                descriptor.enumEntries.mapTo(enumIrClass.declarations) { entryDescriptor ->
                    createEnumEntry(entryDescriptor).also { it.parent = enumIrClass }
                }
            }
        }

    private fun createEnumEntry(entryDescriptor: ClassDescriptor): IrEnumEntry {
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
                    symbol = context.symbolTable.referenceConstructor(enumEntryConstructor),
                    typeArgumentsCount = 0 // enums can't be generic
            )
        }
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
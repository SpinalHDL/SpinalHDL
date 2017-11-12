package spinal.core.internals

import spinal.core._

class AnalogDriverBool extends AnalogDriver{
  override def opName = "AnalogDriver(Bool, Bool)"

  override def getTypeObject = TypeBool
}

abstract class AnalogDriverBitVector extends AnalogDriver with Widthable{
  override type T = Expression with WidthProvider
  override private[core] def calcWidth = data.getWidth
}


class AnalogDriverBits extends AnalogDriverBitVector{
  override def opName = "AnalogDriver(Bits, Bool)"
  override def getTypeObject = TypeBits
}

class AnalogDriverUInt extends AnalogDriverBitVector{
  override def opName = "AnalogDriver(UInt, Bool)"
  override def getTypeObject = TypeUInt
}

class AnalogDriverSInt extends AnalogDriverBitVector{
  override def opName = "AnalogDriver(SInt, Bool)"
  override def getTypeObject = TypeSInt
}

class AnalogDriverEnum(enumDef : SpinalEnum) extends AnalogDriver with InferableEnumEncodingImpl{
  override def opName = "AnalogDriver(Enum, Bool)"
  override def getTypeObject = TypeEnum

  override def normalizeInputs: Unit = {InputNormalize.enumImpl(this)}
  override type T = Expression with EnumEncoded
  override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
  override def getDefinition: SpinalEnum = enumDef
}


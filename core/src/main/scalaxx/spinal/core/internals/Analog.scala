/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.core.internals

import spinal.core._


class AnalogDriverBool extends AnalogDriver {

  override def opName = "AnalogDriver(Bool, Bool)"

  override def getTypeObject = TypeBool
}


abstract class AnalogDriverBitVector extends AnalogDriver with Widthable {

  override type T = Expression with WidthProvider

  override private[core] def calcWidth = data.getWidth
}


class AnalogDriverBits extends AnalogDriverBitVector {

  override def opName = "AnalogDriver(Bits, Bool)"

  override def getTypeObject = TypeBits
}


class AnalogDriverUInt extends AnalogDriverBitVector {

  override def opName = "AnalogDriver(UInt, Bool)"

  override def getTypeObject = TypeUInt
}


class AnalogDriverSInt extends AnalogDriverBitVector {

  override def opName = "AnalogDriver(SInt, Bool)"

  override def getTypeObject = TypeSInt
}


class AnalogDriverEnum(enumDef: SpinalEnum) extends AnalogDriver with InferableEnumEncodingImpl {

  override def opName = "AnalogDriver(Enum, Bool)"

  override def getTypeObject = TypeEnum

  override def normalizeInputs: Unit = {InputNormalize.enumImpl(this)}

  override type T = Expression with EnumEncoded

  override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding

  override def getDefinition: SpinalEnum = enumDef
}


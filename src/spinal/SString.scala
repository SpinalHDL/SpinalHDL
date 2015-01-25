/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package spinal

/**
 * Created by PIC18F on 24.01.2015.
 */
object SString{
  def apply(value : String): SString ={
    val ret = new SString;
    ret.inputs(0) = SStringLiteral(value)
    ret
  }
}

//ONLY FOR BLACK BOX   generic parameters
class SString extends BaseType{
  override def calcWidth = 0

  def :=(that : SString)={
    this.assignFrom(that)
  }

  override def toBits: Bits = {
    throw new Exception("Illegal")
  }

  override def newMultiplexor(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer =  throw new Exception("Illegal")
}

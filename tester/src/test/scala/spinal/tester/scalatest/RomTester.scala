

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

package spinal.tester.scalatest

import spinal.core._
object RomTester {

  object MyEnum extends SpinalEnum{
    val a,b,c = newElement()
  }

  class DataStruct extends Bundle{
    val bool = Bool
    val bits = Bits(9 bits)
    val uint = UInt(10 bits)
    val sint = SInt(11 bits)
    val enumeration = MyEnum()
  }

  class RomTester extends Component {
    def lit(bool : Boolean,bits : Int,uint : Int,sint : Int,enumeration : MyEnum.E) = {
      val data = new DataStruct
      data.bool := Bool(bool)
      data.bits := B(bits)
      data.uint := U(uint)
      data.sint := S(sint)
      data.enumeration := enumeration
      data
    }
    def initValues = List(
      lit(false,0,0,0,MyEnum.a),
      lit(true,0,0,0,MyEnum.a),
      lit(false,0x1FF,0,0,MyEnum.a),
      lit(false,0,0x3FF,0,MyEnum.a),
      lit(false,0,0,-1,MyEnum.a),
      lit(false,0,0,0,MyEnum.c),
      lit(false,43,74,88,MyEnum.b)
    )
    val rom = Mem(new DataStruct,initValues)

    val address = in UInt(3 bits)
    val data = out(rom(address))
  }
}

class RomTesterGhdlBoot extends SpinalTesterGhdlBase {
  override def getName: String = "RomTester"
  override def createToplevel: Component = new RomTester.RomTester
}

class RomTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "RomTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/RomTester"
  override def createToplevel: Component = new RomTester.RomTester
}
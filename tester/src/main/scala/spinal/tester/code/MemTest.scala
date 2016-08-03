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

package spinal.tester.code

import spinal.core._

/**
 * Created by PIC18F on 22.08.2014.
 */
object MemTest {

  class BundleAA extends BundleA {
    val a = Bool
    val d = Bool
    val e = MyEnum()
  }

  class BundleA extends Bundle {
    val b = Bool
    val c = spinal.core.UInt(8 bit)
  }

  object MyEnum extends SpinalEnum {
    val s0, s1, s2 = newElement()
  }

  object MyEnum2 extends SpinalEnum {
    val e0, e1, e2 = newElement()
  }

  class ComponentAA extends Component {
    val io = new Bundle {
      val input = spinal.core.in UInt (5 bit)
      val output = spinal.core.out UInt (8 bit)
    }
    val temp = spinal.core.in UInt (7 bit)
    temp := io.input
    io.output := temp
  }

  class ComponentA extends Component {
    val io = new Bundle {
      val cond0 = spinal.core.in.Bool
      val cond1 = spinal.core.in.Bool
      val cond2 = spinal.core.in.Bool
      val cond3 = spinal.core.in.Bool

      val wrEnable = spinal.core.in.Bool
      val wrAddr = spinal.core.in UInt (4 bit)
      val wrData = spinal.core.in(new BundleA)

      val rdEnable = spinal.core.in.Bool
      val rdAddr = spinal.core.in UInt (4 bit)
      val rdData = spinal.core.out(new BundleA)

      val input = spinal.core.in UInt (4 bit)
      val output = spinal.core.out UInt (9 bit)
    }
    val componentAA = new ComponentAA
    componentAA.io.input := io.input
    io.output := RegNext(io.output)

    val mem = new Mem(io.wrData, 1 << io.wrAddr.getWidth).generateAsBlackBox

    val commonAddress = io.wrAddr + U(3)
    when(io.cond0 && io.cond1) {
   //   mem.write(io.wrAddr + spinal.core.UInt(1), io.wrData)
     // mem.write(commonAddress, io.wrData)
    }
   // val tmp = mem.readSync(io.rdAddr + spinal.core.UInt(2),io.cond2,readFirst)
    //val tmp = mem.readSync(commonAddress,io.cond2,readFirst)
    val tmp = mem.readWriteSync(commonAddress,io.wrData,io.cond0,io.cond1)
    io.rdData := tmp
    tmp.addAttribute(new AttributeString("myAttribut", "hallo"))
    tmp.addAttribute(new AttributeFlag("yolo"))



   // io.output := Mux(io.cond3,SInt(2),UInt(4))

  }


  def main(args: Array[String]) {
    println("START")
    var comp: ComponentA = null

    spinal.core.SpinalVhdl({
      comp = new ComponentA
      comp
    })

    println("DONE")


  }

}


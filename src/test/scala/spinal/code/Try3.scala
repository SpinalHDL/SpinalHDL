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

package spinal.code

import spinal._
import spinal.importMe._

/**
 * Created by PIC18F on 22.08.2014.
 */
object Try3 {

  class BundleAA extends BundleA {
    val a = new Bool()
    val d = new Bool()
    val e = MyEnum.craft()
  }

  class BundleA extends Bundle {
    val b = new Bool()
    val c = UInt(8 bit)
  }

  object MyEnum extends SpinalEnum {
    val s0, s1, s2 = Value
  }

  object MyEnum2 extends SpinalEnum {
    val e0, e1, e2 = Value
  }

  class ComponentA extends Component {
    val io = new Bundle {
      val inRegBundle = in(new BundleAA())
      val outRegBundle = out(new BundleAA())

      val slaveFlow = slave(new Flow(new BundleA))
      val masterFlow = master(new Flow(new BundleA))

      val slaveHandshake = slave(new Handshake(new BundleA))
      val masterHandshake = master(new Handshake(new BundleA))
    }
    {
      var regBundleInit = io.inRegBundle.clone()
      regBundleInit.a := Bool(true)
      regBundleInit.e := MyEnum.s1

      val regBundle = RegInit(regBundleInit)
      regBundle := io.inRegBundle
      io.outRegBundle := regBundle
    }


    io.masterFlow <-< io.slaveFlow
    io.masterHandshake.m2sPipeFrom(io.slaveHandshake)
//    val uC = u
//    val uCf = uC.getDeclaredFields
//    println("**")
  //  regBundle.flatten.foreach(_._2.dontSimplifyIt)
//    regBundleInit = null
//    regBundle = null
  }


  def main(args: Array[String]) {
    println("START")
    var comp: ComponentA = null

    SpinalVhdl({
      comp = new ComponentA
      comp
    })


    println("DONE")


  }

}


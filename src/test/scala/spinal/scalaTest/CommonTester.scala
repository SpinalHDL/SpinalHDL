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

package spinal.scalaTest

import spinal._
import spinal.importMe._

object CommonTester {


  class BundleA extends Bundle {
    val bod = new Bundle {
      val gggg = Bool()
      val aosi = UInt(3 bit)
    }
    val ahe = Bool()
    val zwg = Bool()
  }

  class BundleAA extends BundleA {
    val vsw = Bool()
    val lwee = UInt(5 bit)
  }

  class CommonTester extends Component {
    val io = new Bundle {
      val conds = in Vec(8, Bool())

      val inUIntA = in UInt (8 bit)
      val inUIntB = in UInt (8 bit)
      val outUIntAdder = out UInt()

      val inAA = in(new BundleAA)
      val inAABits = in Bits (new BundleAA().getBitsWidth bit)
      val outAA = out(new BundleAA)
      val outAABits = out Bits (new BundleAA().getBitsWidth bit)


      val assign = new Bundle{
        val sel = in Vec(4,UInt(4 bit))
        val bitDemux = out Bits(16 bit)


        def doit: Unit ={
          bitDemux := Bits(0)
          bitDemux(sel(0)) := conds(0)
          when(conds(1)){
            bitDemux(sel(1)) := conds(2)
          }.elsewhen(conds(3)){
            bitDemux(sel(0)) := conds(4)
          }
          when(conds(5)){
            bitDemux(sel(1)) := conds(6)
          }
          bitDemux(5) := Bool(true)
        }
      }
      def doit: Unit ={
        assign.doit
      }
    }


    io.doit

    io.outAA.fromBits(io.inAABits)
    io.outAABits := io.inAA.toBits


    val combAdder = new ComponentPart {
      val size = io.inUIntA.getWidth
      var out = Vec(size, Bool())

      var c = Bool(false)
      for (i <- 0 until size) {
        val a = io.inUIntA(i)
        val b = io.inUIntB(i)
        out(i) := a ^ b ^ c
        c = (a & b) | (a & c) | (b & c);
      }
      io.outUIntAdder := out.toBits.toUInt
    }


  }

}

class CommonTesterBoot extends SpinalTesterBase {
  override def getName: String = "CommonTester"

  override def createToplevel: Component = new CommonTester.CommonTester
}

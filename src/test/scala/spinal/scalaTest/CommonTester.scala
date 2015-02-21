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
    val bod = new Bundle{
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

      val inAA = in(new BundleAA)
      val inAABits = in Bits(new BundleAA().getBitsWidth bit)
      val outAA = out(new BundleAA)
      val outAABits = out Bits(new BundleAA().getBitsWidth bit)

    }


    io.outAA.fromBits(io.inAABits)
    io.outAABits  := io.inAA.toBits

  }

}

class CommonTesterBoot extends SpinalTesterBase {
  override def getName: String = "CommonTester"
  override def createToplevel: Component = new CommonTester.CommonTester
}

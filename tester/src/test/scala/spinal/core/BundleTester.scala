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

package spinal.core

import spinal.tester.SpinalTesterCocotbBase

object BundleTester {
  class BundleAA extends BundleA(True,3) {
    val b = Bool()
    val d = Bool()
  }

  case class BundleA(param : Bool,parm2 : Int) extends Bundle {
    val a = Bool()
    val c = Bool()
  }
}
import BundleTester._

class BundleTester extends Component {
  val io = new Bundle {
    val conds = in Vec(Bool(),8)

    val inAA = in Vec(new BundleAA,3)
    val inA = in Vec(new BundleA(False,6),1)

    val outAA = out(new BundleAA)
  }

  val zero = new BundleAA
  zero.flatten.foreach(_ := False)

  when(io.conds(0)) {
    io.outAA := io.inAA(0)
    io.outAA.allowOverride
    io.outAA assignSomeByName io.inA(0)
  } otherwise {
    io.outAA := zero
  }

  when(io.conds(1)) {
    io.outAA := io.inAA(1)
  }
}


//class BundleTesterGhdlBoot extends SpinalTesterGhdlBase {
//  override def getName: String = "BundleTester"
//  override def createToplevel: Component = new BundleTester
//}

class BundleTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "BundleTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/BundleTester"
  override def createToplevel: Component = new BundleTester
}
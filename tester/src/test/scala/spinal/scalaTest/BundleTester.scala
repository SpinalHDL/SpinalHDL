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

import spinal.core
import spinal.core._

object BundleTester {
  class BundleAA extends BundleA {
    val b = Bool()
    val d = core.Bool()
  }

  class BundleA extends Bundle {
    val a = core.Bool()
    val c = core.Bool()
  }
}

import BundleTester._

class BundleTester extends Component {
  val io = new Bundle {
    val conds = in Vec(8, core.Bool())

    val inAA = core.in Vec(3, new BundleAA)
    val inA = core.in Vec(1, new BundleA)

    val outAA = out(new BundleAA)
  }

  val zero = new BundleAA
  zero.flatten.foreach(_._2 := core.Bool(false))

  when(io.conds(0)) {
    io.outAA := io.inAA(0)
    io.outAA := io.inA(0)
  } otherwise {
    io.outAA := zero
  }

  core.when(io.conds(1)) {
    io.outAA := io.inAA(1)
  }
}


class BundleTesterBoot extends SpinalTesterBase {
  override def getName: String = "BundleTester"
  override def createToplevel: Component = new BundleTester
}
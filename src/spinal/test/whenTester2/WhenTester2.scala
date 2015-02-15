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

package spinal.test.whenTester

import spinal._
import spinal.importMe._
import spinal.test.SpinalJUnit
import org.junit._

class WhenTester extends Component {
  val io = new Bundle {
    val conds = in Vec(5, Bool())
    val data = in Vec(5, UInt(8 bit))
    val outDefault = out UInt (8 bit)
  }

  io.outDefault := io.data(0)
  when(io.conds(0)) {
    io.outDefault := io.data(1)
  }
}

class WhenTesterJUnit  extends SpinalJUnit{
  override def getName: String = "WhenTester"
  override def createToplevel: Component = new WhenTester
}

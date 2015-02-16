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

package spinal.test.junit

import org.junit.Test
import spinal.{Component, SpinalVhdl}

import scala.sys.process._

abstract class SpinalJUnit {
  @Test def testIt: Unit = {
    elaborate
    checkHDL
    simulateHDL
  }

  def elaborate: Unit = {
    SpinalVhdl(createToplevel)
  }

  def elaborateWithFail: Unit = {
    try {
      elaborate
      assert(false, "Spinal elaboration has not fail :(")
    } catch {
      case e: Throwable => return;
    }
  }


  def checkHDL(mustSuccess: Boolean): Unit = {
    println("GHDL compilation")
    assert(((s"ghdl -a $getName.vhd ${getName}_tb.vhd" !) == 0) == mustSuccess, if (mustSuccess) "compilation fail" else "Compilation has not fail :(")

    println("GHDL elaborate")
    assert(((s"ghdl -e ${getName}_tb" !) == 0) == mustSuccess, if (mustSuccess) "compilation fail" else "Compilation has not fail :(")
  }

  def checkHDL: Unit = checkHDL(true)
  def checkHDLWithFail: Unit = {
    checkHDL(false)
  }

  def simulateHDL: Unit = {
    println("GHDL run")
    assert((s"ghdl -r ${getName}_tb${if (!withWaveform) "" else s" --vcd=$getName.vcd"}" !) == 0, "run fail")
  }

  def withWaveform : Boolean = false

  def getName: String
  def createToplevel: Component
}

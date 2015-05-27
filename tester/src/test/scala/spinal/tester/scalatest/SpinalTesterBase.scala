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

import org.scalatest.FunSuite
import spinal.core._

import scala.sys.process._

abstract class SpinalTesterBase extends FunSuite  {

  var withWaveform = false
  var elaborateMustFail = false
  var checkHDLMustFail = false
  var simulateMustFail = false

  test(getName) {
    testIt
    postTest
  }


  def testIt: Unit = {
    if(!elaborateMustFail) elaborate else elaborateWithFail
    if(!checkHDLMustFail) checkHDL else checkHDLWithFail
    if(!simulateMustFail) simulateHDL else simulateHDLWithFail
  }

  def postTest : Unit = {}

  def getLibraryName = "lib_" + getName
  def elaborate: Unit = {
    SpinalVhdl(createToplevel,_.setLibrary(getLibraryName))
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
    val comp = s"ghdl -a --ieee=synopsys --work=$getLibraryName $getName.vhd tester/src/test/resources/${getName}_tb.vhd"
    println("GHDL compilation " + comp)
    assert(((comp !) == 0) == mustSuccess, if (mustSuccess) "compilation fail" else "Compilation has not fail :(")


    val elab = s"ghdl -e --ieee=synopsys --work=$getLibraryName ${getName}_tb"
    println("GHDL elaborate " + elab)
    assert(((elab !) == 0) == mustSuccess, if (mustSuccess) "compilation fail" else "Compilation has not fail :(")
  }

  def checkHDL: Unit = checkHDL(true)
  def checkHDLWithFail: Unit = checkHDL(false)


  def simulateHDL : Unit= simulateHDL(true)
  def simulateHDLWithFail : Unit= simulateHDL(false)

  def simulateHDL(mustSuccess : Boolean): Unit = {
    val run = s"ghdl -r --ieee=synopsys --work=$getLibraryName ${getName}_tb${if (!withWaveform) "" else s" --vcd=$getName.vcd"}"
    println("GHDL run " + run)
    val ret = (run !)
    assert(!mustSuccess || ret == 0,"Simulation fail")
    assert(mustSuccess || ret != 0,"Simulation has not fail :(")
  }



  def getName: String
  def createToplevel: Component
}

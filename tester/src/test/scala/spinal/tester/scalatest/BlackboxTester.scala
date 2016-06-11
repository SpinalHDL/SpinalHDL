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
import spinal.lib._

object BlackboxTester {

  case class BBGenerics(aWidth: Int, bWidth: Int) extends Generic

  class BlackBoxToTest(val generic : BBGenerics) extends BlackBox {
    import generic._
    val io = new Bundle {
      val clockPin,resetPin = in Bool

      val inA = in UInt(aWidth bits)
      val inB = in UInt(bWidth bits)
      val outA = out UInt(aWidth bits)
      val outB = out UInt(bWidth bits)
    }

    mapClockDomain(clock=io.clockPin,reset = io.resetPin)
  }

  class BlackBoxTester extends Component {
    val generic = BBGenerics(8,16)
    import generic._
    val io = new Bundle{
      val inA = in UInt(aWidth bits)
      val inB = in UInt(bWidth bits)
      val outA = out UInt(aWidth bits)
      val outB = out UInt(bWidth bits)
    }
    val blackBoxtoTest  = new BlackBoxToTest(generic)
    io.inA <> blackBoxtoTest.io.inA
    io.inB <> blackBoxtoTest.io.inB
    io.outA <> blackBoxtoTest.io.outA
    io.outB <> blackBoxtoTest.io.outB
  }

}

class BlackboxTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "BlackBoxTester"
  override def createToplevel: Component =  new BlackboxTester.BlackBoxTester
  override def pythonTestLocation: String = "tester/src/test/python/spinal/BlackBoxTester"
}
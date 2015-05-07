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
import spinal.lib._
import spinal.lib.com.uart._

object SpinalLibTest {

  class BundleAA extends BundleA {
    val a = Bool
    val d = Bool
    val e = MyEnum.craft()
  }

  class BundleA extends Bundle {
    val b = Bool
    val c = UInt(8 bit)
  }

  object MyEnum extends SpinalEnum {
    val s0, s1, s2 = Value
  }

  object MyEnum2 extends SpinalEnum {
    val e0, e1, e2 = Value
  }

  class TopLevel extends Component {
    val io = new Bundle {
      val clkA = in Bool
      val resetA = in Bool

      val clkB = in Bool
      val resetB = in Bool

      val inRegBundle = in(new BundleAA())
      val outRegBundle = out(new BundleAA())

      val slaveFlow = slave(new Flow(new BundleA))
      val masterFlow = master(new Flow(new BundleA))

      val slaveHandshake = slave(new Handshake(new BundleA))
      val masterHandshake = master(new Handshake(new BundleA))


      val slaveHandshakeClkA = slave(new Handshake(new BundleA))
      val masterHandshakeClkB = master(new Handshake(new BundleA))

      //val arbiter = new HandshakeArbiterIO(new BundleA,4)

      val uart = new UartCtrlIo()
      val uartX = new UartCtrlIo()

     // val fifo = new HandshakeFifoIo(Bits(36 bit),256)
    }

    /*val fifo = new HandshakeFifo(Bits(36 bit),256)
    fifo.io <> io.fifo*/

    val uartCtrl = new UartCtrl()
    io.uart <> uartCtrl.io

    val uartCtrlX = new UartCtrl()
    io.uartX <> uartCtrlX.io


    val clockA = ClockDomain(io.clkA, io.resetA)
    val clockB = ClockDomain(io.clkB, io.resetB)


//    val arbiter = new HandshakeArbiterPriorityImpl(new BundleA,4,true)
//    arbiter.io <> io.arbiter

    {




      val regBundle = Reg(io.inRegBundle)
      regBundle.a.init(True)
      regBundle.e.init(MyEnum.s1())

      regBundle := io.inRegBundle
      io.outRegBundle := regBundle
    }


    io.masterFlow <-< io.slaveFlow
    io.masterHandshake connectFrom io.slaveHandshake


//    val crossClockHandshake = new CrossClockStream_HandShake(io.slaveHandshakeClkA.data,clockA,clockB)
//    crossClockHandshake.io.input << io.slaveHandshakeClkA
//    io.masterHandshakeClkB << crossClockHandshake.io.output


   io.masterHandshakeClkB << HandshakeCCByToggle(io.slaveHandshakeClkA,clockA,clockB)

  }


  def main(args: Array[String]) {
    println("START")
    var comp: TopLevel = null

    SpinalVhdl({
      comp = new TopLevel
      comp
    })


    println("DONE")


  }

}


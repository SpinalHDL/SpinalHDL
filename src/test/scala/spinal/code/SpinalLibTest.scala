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
import spinal.lib._


object SpinalLibTest {

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

  class TopLevel extends Component {
    val io = new Bundle {
      val clkA = in Bool()
      val resetA = in Bool()

      val clkB = in Bool()
      val resetB = in Bool()

      val inRegBundle = in(new BundleAA())
      val outRegBundle = out(new BundleAA())

      val slaveFlow = spinal.slave(new Flow(new BundleA))
      val masterFlow = spinal.master(new Flow(new BundleA))

      val slaveHandshake = spinal.slave(new Handshake(new BundleA))
      val masterHandshake = spinal.master(new Handshake(new BundleA))


      val slaveHandshakeClkA = spinal.slave(new Handshake(new BundleA))
      val masterHandshakeClkB = spinal.master(new Handshake(new BundleA))

      val arbiter = new HandshakeArbiterCoreIO(new BundleA,4)


      val uart = master(new Uart)
      val uartCmd = slave Handshake( Bits(8 bit))
      val uartConfig = in(new UartConfig())
    }

    val uartTx = new UartTx()
    uartTx.io.config := io.uartConfig
    uartTx.io.cmd << io.uartCmd
    io.uart.txd := uartTx.io.txd

    val clockA = ClockDomain(io.clkA, io.resetA)
    val clockB = ClockDomain(io.clkB, io.resetB)


    val arbiter = new HandshakeArbiterPriorityImpl(new BundleA,4,true)
    arbiter.io <> io.arbiter

    {
      var regBundleInit = io.inRegBundle.clone()
      regBundleInit := regBundleInit.getZero
      regBundleInit.a := Bool(true)
      regBundleInit.e := MyEnum.s1



      val regBundle = RegInit(regBundleInit)
      regBundle := io.inRegBundle
      io.outRegBundle := regBundle
    }


    io.masterFlow <-< io.slaveFlow
    io.masterHandshake connectFrom io.slaveHandshake


//    val crossClockHandshake = new CrossClockStream_HandShake(io.slaveHandshakeClkA.data,clockA,clockB)
//    crossClockHandshake.io.input << io.slaveHandshakeClkA
//    io.masterHandshakeClkB << crossClockHandshake.io.output


   io.masterHandshakeClkB << CCHandshakeByToggle(io.slaveHandshakeClkA,clockA,clockB)

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


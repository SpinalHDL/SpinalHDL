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

package spinal.test

import spinal.ImportMe._
import spinal._

/**
 * Created by PIC18F on 22.08.2014.
 */
object Try4 {


  class BundleA extends Bundle {
    val first = Bool()
    val second = UInt(8 bit)

    val sub = new Bundle {
      val third = SInt(8 bit)
    }
  }




  class TopLevel extends Component {
    val io = new Bundle {
      val cond0 = in.Bool()
      val cond1 = in.Bool()
      val outBool = out.Bool()

      val inBundleA = in(new BundleA)
      val outBundleA = out(new BundleA)

      val slaveHandshake = slave Handshake(new BundleA)
      val masterHandshake = master Handshake(new BundleA)
    }


    io.masterHandshake << (io.slaveHandshake & io.cond0)


    val myReg = Reg(io.inBundleA.second)

    when(io.cond0) {
      myReg := myReg + UInt(1)
    }
    val myReg2 = RegNext(io.cond0 && io.cond1 && (myReg !== UInt(0)))
    io.outBool := myReg2

    io.outBundleA := io.inBundleA


  }


  def main(args: Array[String]) {
    println("START")
    SpinalVhdl(new TopLevel)
    println("DONE")
  }

}


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



object Debug {

  class MyBundle extends Bundle{
    val a = Bool
    val b = Bool
    val c = Vec(2,new MyBundle2)
  }

  class MyBundle2 extends Bundle{
    val a = Bool
    val b = Bool
  }

  class TopLevel extends Component {

    val io = new Bundle {
//      val in1 = in (new MyBundle)
//      val out1 = out (new MyBundle2)

//      val outputVec = Vec(3, master Handshake (new MyBundle))

      val input = slave Handshake (new MyBundle)
      val output = master Handshake (new MyBundle)

    }

    //io.out1 assignAllByName io.in1

    val forks = HandshakeFork(io.input,3)
    io.output << HandshakeArbiterPriorityToLow(forks)
  //  io.output <> HandshakeFork(io.input,3)
   // (io.outputVec , HandshakeFork(io.input,3)).zipped.foreach(_ << _)
  }


  def main(args: Array[String]) {
    println("START")
    SpinalVhdl(new TopLevel)
    println("DONE")
  }

}




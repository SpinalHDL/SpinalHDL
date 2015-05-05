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


  class TopLevel extends Component {
    class MyBundle extends Bundle{
      val a = Bool
      val b = Bool
    }

    class MyBundle2 extends Bundle{
      val a = Bool
      val b = Bool
    }
    val io = new Bundle {
      val in1 = in (new MyBundle)
      val out1 = out (new MyBundle2)
    }

    io.out1 := io.in1


  }


  def main(args: Array[String]) {
    println("START")
    SpinalVhdl(new TopLevel)
    println("DONE")
  }

}


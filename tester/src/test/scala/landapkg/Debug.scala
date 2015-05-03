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

package landapkg

import spinal.core._
import spinal.lib._

case class MyBundle(paramBool : Bool,asd : Int) extends Bundle{
  val a = cloneOf(paramBool)
}

object Debug {


  class TopLevel extends Component {

    val io = new Bundle {
      val in1 = in (new MyBundle(Bool,1))
      val out1 = out (new MyBundle(Bool,2))
    }


    val temp = cloneOf(io.in1)
    temp := io.in1
    io.out1 := temp

  }


  def main(args: Array[String]) {
    println("START")
    SpinalVhdl(new TopLevel)
    println("DONE")
  }

}


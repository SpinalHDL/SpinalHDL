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

import scala.collection.mutable.ArrayBuffer


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



  object MyEnum extends SpinalEnum{
    val s0,s1,s2 = Value
  }
  object MyEnum2 extends SpinalEnum{
    val s0,s1,s2 = Value
  }
  class TopLevel extends Component {

    val io = new Bundle {
//      val in1 = in (new MyBundle)
//      val out1 = out (new MyBundle2)

//      val outputVec = Vec(3, master Handshake (new MyBundle))

      val input = slave Handshake (new MyBundle)
      val output = master Handshake (new MyBundle)

//      val romCmd = slave Handshake(UInt(2 bit))
//      val romRead = master Handshake(RomData())
      val romCmd = in(UInt(2 bit))
      val romRead = out(MyData())
    }
    MyEnum.s1 === MyEnum.s2()
//    implicit def EnumElementToCraft[T <: SpinalEnum](enumDef : T) : SpinalEnumCraft[T] = enumDef.craft().asInstanceOf[SpinalEnumCraft[T]]
//    implicit def EnumElementToCraft2[T <: SpinalEnum](enumDef : SpinalEnumElement[T]) : SpinalEnumCraft[T] = enumDef.craft().asInstanceOf[SpinalEnumCraft[T]]
//
    val s0Reg = RegNext(MyEnum.s0())


    val forks = HandshakeFork(io.input,3)
    io.output << HandshakeArbiterPriorityToLow(forks)

    object MyData{
      def apply(a : Boolean,b : BigInt) : MyData ={
        val ret = MyData()
        ret.a := Bool(a)
        ret.b := b
        ret
      }
    }

    case class MyData() extends Bundle{
      val a = Bool
      val b = UInt(3 bit)
    }

    val romData = ArrayBuffer(MyData(false,1),MyData(false,2),MyData(true,3),MyData(false,4))
    for(i <- 0 to 63){
      romData += MyData(false,i)
    }
    val rom = Mem(MyData(),68) init(romData)

    io.romRead := rom(io.romCmd)
  }


  def main(args: Array[String]) {
    println("START")
    SpinalVhdl(new TopLevel)
    println("DONE")
  }

}




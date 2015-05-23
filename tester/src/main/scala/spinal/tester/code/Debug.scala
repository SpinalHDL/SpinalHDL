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

  class A{
    val a = 1
    val aa = 2
  }

  class B extends A{
    val b = 2
  }

  class C(xx : Int) extends B{

  }

  class MyBundle extends Bundle{
    val a = Bool
    val b = Bool
    val c = Vec(2,new MyBundle2)
  }

  class MyBundle2 extends Bundle{
    val a = Bool
    val b = Bool
  }


  class MyBundleSub extends MyBundle{

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
//      val romCmd = in(UInt(2 bit))
//      val romRead = out(MyData())
      val sin = out SInt(16 bit)
      val fir = out SInt(16 bit)
    }
    MyEnum.s1 === MyEnum.s2()
//    implicit def EnumElementToCraft[T <: SpinalEnum](enumDef : T) : SpinalEnumCraft[T] = enumDef.craft().asInstanceOf[SpinalEnumCraft[T]]
//    implicit def EnumElementToCraft2[T <: SpinalEnum](enumDef : SpinalEnumElement[T]) : SpinalEnumCraft[T] = enumDef.craft().asInstanceOf[SpinalEnumCraft[T]]
//
    val s0Reg = RegNext(MyEnum.s0())


    val forks = HandshakeFork(io.input,3)
    io.output << HandshakeArbiter.lowIdPortFirst.transactionLock.build(forks)

    object MyData{
      def apply(a : Boolean,b : BigInt) : MyData ={
        val ret = MyData()
        ret.a := Bool(a)
        ret.b := b
        ret
      }
    }

    //    for(i <- 0 to 63){
    //      romData += MyData(false,i)
    //    }

    case class MyData() extends Bundle{
      val a = Bool
      val b = UInt(3 bit)
    }

//    val romData = ArrayBuffer(MyData(false,1),MyData(false,2),MyData(true,3),MyData(false,4))
//    val rom = Mem(MyData(),68) init(romData)
//
//    io.romRead := rom(io.romCmd)

//    val lockupTable = Mem(SInt(16 bit),1024)
//    lockupTable.init((0 to 1023).map(i => S((Math.sin(i*2*Math.PI/1024.0)*32767).toInt)))
//
//    val counter = CounterFreeRun(1024)
//    io.sin := (lockupTable(counter)*lockupTable(counter)) >> 16


    io.sin := Mem(SInt(16 bit),(0 to 1023).map(i => S((Math.pow(Math.sin(i*2*Math.PI*8/1024.0),1)*32767).toInt))).readSync(CounterFreeRun(1024))


    val firLength = 32
    val coefs = (0 until firLength).map(i => S(((0.54-0.46*Math.cos(2*Math.PI*i/firLength))*32767/firLength).toInt,16 bit))
    io.fir := (coefs,Delays(io.sin,firLength)).zipped.map((coef,delay) => (coef * delay) >> 15).reduce(_ + _)

  }


  def main(args: Array[String]) {
    println("START")
    val a = new A
    val b = new B
    val c = new C(2)

    SpinalVhdl(new TopLevel)
    println("DONE")
  }

}




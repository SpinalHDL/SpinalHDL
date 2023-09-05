

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

package spinal.core

import spinal.lib._
import spinal.lib.io.{TriState, TriStateArray}
import spinal.tester.{SpinalAnyFunSuite, SpinalTesterCocotbBase}

object InOutTester {
  def analogType = Bool()
  case class Bus() extends Bundle with IMasterSlave{
    val cmd = TriState(analogType)
    val gpio = Analog(analogType)
    cmd.writeEnable.setCompositeName(cmd,"writeenable")
    override def asMaster(): Unit = {
      master(cmd)
      inout(gpio)
    }
  }

  class Sub extends Component{
    val bus2 = slave(Bus())
    val busX = master(Bus())
    val tmp2 = Analog(analogType)
    when(bus2.cmd.writeEnable){
      tmp2 := bus2.cmd.write
    }
    bus2.cmd.read := tmp2

    bus2.gpio := tmp2


    busX.cmd.write := False
    busX.cmd.writeEnable := False
    busX.gpio := False
  }

  class BlackBoxed extends BlackBox{
    val bus3 = slave(Bus())
  }



  class InOutTester extends Component {
    val bus = slave(Bus())
    val cmd = slave(TriState(analogType))
    val cmdbb = slave(TriState(analogType))
    val busX = master(Bus())

    cmd.writeEnable.setCompositeName(cmd,"writeenable")
    cmdbb.writeEnable.setCompositeName(cmdbb,"writeenable")
    val tmp = Analog(analogType)
    when(cmd.writeEnable){
      tmp := cmd.write
    }
    cmd.read := tmp

    val busCpy = Bus()
    busCpy <> bus
    busCpy.gpio <> tmp

    val sub = new Sub
    sub.bus2 <> busCpy
    busX <> sub.busX

    val buscpy_gpio_readed = out(analogType)
    buscpy_gpio_readed := busCpy.gpio


    val bb = new BlackBoxed
    bb.bus3.cmd <> cmdbb
    bus.gpio := bb.bus3.gpio
    bb.bus3.cmd.writeEnable.setCompositeName(bb.bus3.cmd,"writeenable")

  }
}

class InOutTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "InOutTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/InOutTester"
  override def createToplevel: Component = new InOutTester.InOutTester
}


object InOutTester2 {
  def analogType = UInt(8 bits)
  case class Bus() extends Bundle with IMasterSlave{
    val cmd = TriState(analogType)
    val gpio = Analog(analogType)
    cmd.writeEnable.setCompositeName(cmd,"writeenable")
    override def asMaster(): Unit = {
      master(cmd)
      inout(gpio)
    }
  }

  class Sub extends Component{
    val bus2 = slave(Bus())
    val tmp2 = Analog(analogType)
    when(bus2.cmd.writeEnable){
      tmp2 := bus2.cmd.write
    }
    bus2.cmd.read := tmp2

    bus2.gpio := tmp2
  }

  class BlackBoxed extends BlackBox{
    val bus3 = slave(Bus())
  }


  class InOutTester2 extends Component {
    val bus = slave(Bus())
    val cmd = slave(TriState(analogType))
    val cmdbb = slave(TriState(analogType))

    cmd.writeEnable.setCompositeName(cmd,"writeenable")
    cmdbb.writeEnable.setCompositeName(cmdbb,"writeenable")
    val tmp = Analog(analogType)
    when(cmd.writeEnable){
      tmp := cmd.write
    }
    cmd.read := tmp

    val busCpy = Bus()
    busCpy <> bus
    busCpy.gpio <> tmp

    val sub = new Sub
    sub.bus2 <> busCpy

    val buscpy_gpio_readed = out(analogType)
    buscpy_gpio_readed := busCpy.gpio


    val bb = new BlackBoxed
    bb.bus3.cmd <> cmdbb
    bus.gpio := bb.bus3.gpio
    bb.bus3.cmd.writeEnable.setCompositeName(bb.bus3.cmd,"writeenable")

  }
}

class InOutTester2CocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "InOutTester2"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/InOutTester2"
  override def createToplevel: Component = new InOutTester2.InOutTester2
  
}


object InOutTester3 {
  def analogType = Bits(8 bits)
  case class Bus() extends Bundle with IMasterSlave{
    val cmd = TriStateArray(8 bits)
    val gpio = Analog(analogType)
    cmd.writeEnable.setCompositeName(cmd,"writeenable")
    override def asMaster(): Unit = {
      master(cmd)
      inout(gpio)
    }
  }

  class Sub extends Component{
    val bus2 = slave(Bus())
    val tmp2 = Analog(analogType)
    for(i <- bus2.gpio.range) {
      when(bus2.cmd.writeEnable(i)) {
        tmp2(i) := bus2.cmd.write(i)
      }
    }
    bus2.cmd.read := tmp2

    bus2.gpio := tmp2
  }



  class InOutTester3 extends Component {
    val bus = slave(Bus())
    val sub = new Sub()
    sub.bus2 <> bus
  }
}

class InOutTester3CocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "InOutTester3"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/InOutTester3"
  override def createToplevel: Component = new InOutTester3.InOutTester3
  withWaveform = false
}


class InOutTester4 extends SpinalAnyFunSuite {
  case class A() extends Component {
    val data=inout(Analog(Bool()))
  }
  case class B() extends Component {
    val data=inout(Analog(Bool()))
  }
  case class D() extends Component {
    val data=inout(Analog(Bool()))
  }
  case  class C() extends Component{
    val aInst=A()
    val bInst=B()
    val cInst, dInst=D()


//    val x,y = Analog(Bool())
//    x := y
//    x := aInst.data
//    y := bInst.data
    cInst.data := dInst.data
    bInst.data := cInst.data
    aInst.data := bInst.data

//    val data = Analog(Bool())
//    data <> aInst.data
//    data <> bInst.data
  }
  test("test inout connect"){
    SpinalSystemVerilog(C())
  }
}
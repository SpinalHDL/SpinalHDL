

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
import spinal.lib.io.TriState
object InOutTester {
  case class Bus() extends Bundle with IMasterSlave{
    val cmd = TriState(Bool)
    val gpio = analog(Bool)
    cmd.writeEnable.setCompositeName(cmd,"writeenable")
    override def asMaster(): Unit = {
      master(cmd)
      inout(gpio)
    }
  }

  class Sub extends Component{
    val bus2 = slave(Bus())
    val tmp2 = analog(Bool)
    when(bus2.cmd.writeEnable){
      tmp2 := bus2.cmd.write
    }
    bus2.cmd.read := tmp2

    bus2.gpio := tmp2
  }

  class BlackBoxed extends BlackBox{
    val bus3 = slave(Bus())
  }


  class InOutTester extends Component {
    val bus = slave(Bus())
    val cmd = slave(TriState(Bool))
    val cmdbb = slave(TriState(Bool))

    cmd.writeEnable.setCompositeName(cmd,"writeenable")
    cmdbb.writeEnable.setCompositeName(cmdbb,"writeenable")
    val tmp = analog(Bool)
    when(cmd.writeEnable){
      tmp := cmd.write
    }
    cmd.read := tmp

    val busCpy = Bus()
    busCpy <> bus
    busCpy.gpio <> tmp

    val sub = new Sub
    sub.bus2 <> busCpy

    val buscpy_gpio_readed = out(Bool)
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
  withWaveform = true
}
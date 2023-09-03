package spinal.core

import spinal.lib._
import spinal.lib.io._
import spinal.lib.blackbox.xilinx.s7.IOBUF
import spinal.tester.{SpinalAnyFunSuite, SpinalTesterCocotbBase}

class PortBlackBox extends BlackBox {
  val write = in(Bool())
  val read = out(Bool())
  val writeEnable = in(Bool())
  val pad = inout(Analog(Bool()))
}

class AnalogConnectionTester extends Component {
  val simple_driver = new Bundle {
    val tri = slave(TriState(Bool()))
    val pad = inout(Analog(Bool()))
  }
  simple_driver.tri.read := simple_driver.pad
  when(simple_driver.tri.writeEnable) {
    simple_driver.pad := simple_driver.tri.write
  }

  val analog_bus_width = 2
  val analog_bus = new Bundle {
    val tri = slave(TriStateArray(analog_bus_width bit))
    val pad = inout(Analog(Bits(analog_bus_width bit)))
  }
  for (i <- 0 until analog_bus_width) {
    analog_bus.tri(i).read := analog_bus.pad(i)
    when(analog_bus.tri(i).writeEnable) {
      analog_bus.pad(i) := analog_bus.tri(i).write
    }
  }

  val blackbox = new Bundle {
    val tri = slave(TriState(Bool()))
    val pad = inout(Analog(Bool()))
  }
  val blackbox_inst = new PortBlackBox()
  blackbox_inst.write := blackbox.tri.write
  blackbox_inst.writeEnable := blackbox.tri.writeEnable
  blackbox.tri.read := blackbox_inst.read
  blackbox_inst.pad <> blackbox.pad

  val sliced_bus = new Bundle {
    val tri = slave(TriState(Bool()))
    val pad = inout(Analog(Bits(2 bit)))
  }
  val sliced_sub = new Component {
    val tri = slave(TriState(Bool()))
    val pad = inout(Analog(Bits(1 bit)))
    tri.read := pad(0)
    when(tri.writeEnable) {
      pad(0) := tri.write
    }
  }
  sliced_bus.pad(0 downto 0) <> sliced_sub.pad
  sliced_sub.tri <> sliced_bus.tri
}

class AnalogConnectionCocotbBoot extends SpinalTesterCocotbBase {
  override def getName = "AnalogConnection"
  override def pythonTestLocation = "tester/src/test/python/spinal/AnalogConnectionTester"
  override def createToplevel = new AnalogConnectionTester
}

class AnalogConnectionTest extends SpinalAnyFunSuite {
  import CheckTester._

  test("drive analog signal") {
    generationShouldPass {
      new Component {
        val tri = slave(TriState(Bool()))
        val pad = inout(Analog(Bool()))

        tri.read := pad
        when(tri.writeEnable) {
          pad := tri.write
        }
      }
    }
  }

  test("drive analog bus") {
    val width = 1
    generationShouldPass {
      new Component {
        val tri = slave(TriStateArray(width bit))
        val pad = inout(Analog(Bits(width bit)))

        for (i <- 0 until width) {
          tri(i).read := pad(i).setAsComb()
          when(tri(i).writeEnable) {
            pad(i) := tri(i).write
          }
        }
      }
    }
  }

  test("drive with bus slice") {
    generationShouldPass {
      new Component {
        val io = new Bundle {
          val tri = slave(TriState(Bool()))
          val pad = inout(Analog(Bits(2 bit)))
        }
        val sub = new Component {
          val tri = slave(TriState(Bool()))
          val pad = inout(Analog(Bits(1 bit)))
          tri.read := pad(0)
          when(tri.writeEnable) {
            pad(0) := tri.write
          }
        }
        io.pad(0 downto 0) <> sub.pad
        sub.tri <> io.tri
      }
    }
  }

  test("connect slices to blackbox") {
    val width = 3
    generationShouldPass {
      new Component {
        val tri = slave(TriStateArray(width bit))
        val pad = inout(Analog(Bits(width bit)))

        val b = Array.fill[IOBUF](width)(IOBUF())
        for (i <- 0 until width) {
          b(i).T := tri(i).writeEnable
          b(i).I := tri(i).write
          tri(i).read := b(i).O
          pad(i) := b(i).IO
        }
      }
    }
  }

  test("allow unconnected analog") {
    generationShouldPass {
      new Component {
        val y = inout(Analog(Bits(2 bit)))
      }
    }
  }

  test("allow partly unconnected analog") {
    generationShouldPass {
      new Component {
        val y = inout(Analog(Bits(2 bit)))
        y(0) := True
      }
    }
  }

  test("allow assignment to non-analog") {
    generationShouldPass {
      new Component {
        val sub = new Component {
          val x = in(Bool())
          x.dontSimplifyIt()
        }
        val y = inout(Analog(Bool()))
        sub.x := y
      }
    }
  }

  test("allow mixed analog and non-analog connection") {
    generationShouldPass {
      new Component {
        val sub1 = new Component {
          val x = in(Bool())
          x.dontSimplifyIt()
        }
        val sub2 = new Component {
          val x = inout(Analog(Bool()))
          x := True
        }
        val y = inout(Analog(Bool()))
        sub1.x := y
        sub2.x := y
      }
    }
  }

  test("slicing must retain analog-ness") {
    generationShouldPass {
      new Component {
        val sub = new Component {
          val x = inout(Analog(Bits(1 bit)))
          x(0) := True
        }
        val y = inout(Analog(Bits(2 bit)))
        sub.x := y(0 downto 0)
        y(1) := True
      }
    }
  }

  test("prohibit floating bit access for analog") {
    generationShouldFail {
      new Component {
        val y = inout(Analog(Bits(2 bit)))
        val sel = in(UInt(1 bit))
        val x = inout(Analog(Bool()))
        x := y(sel)
      }
    }
  }

  test("analog assignment width mismatch must be caught") {
    generationShouldFail {
      new Component {
        new Component {
          val sub = new Component {
            val x = inout(Analog(Bits(2 bit)))
            x(0) := True
            x(1) := True
          }
          val y = inout(Analog(Bits(1 bit)))
          sub.x := y
        }
      }
    }
  }

  test("prohibit analog connection on same hierarchy") {
    generationShouldFail {
      new Component {
        val y = inout(Analog(Bits(2 bit)))
        val x = inout(Analog(Bits(1 bit)))
        x(0 downto 0) := y(1 downto 1)
      }
    }
  }
}

package spinal.lib.bus.amba3.ahblite

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.tester._

class AhbLite3Tester extends SpinalAnyFunSuite {
  case class WriteMaskDut(cfg: AhbLite3Config) extends Component {
    val io = new Bundle {
      val ahb       = slave port AhbLite3(cfg)
      val writeMask = out   port Bits(cfg.bytePerWord bits)
    }
    io.ahb.HRESP     := False
    io.ahb.HREADYOUT := False
    io.ahb.HRDATA    := 0
    io.writeMask     := io.ahb.writeMask
  }
  
  case class WriteMaskTestValue(address: Int, hsize: Int, writeMask: BigInt)
  
  def writeMaskTest(dataBits: Int, check: Seq[WriteMaskTestValue]) = {
    val cfg = AhbLite3Config(32, dataBits)
    SimConfig.compile(WriteMaskDut(cfg)).doSim("test", 42) { dut =>
      // Set up a dummy address phase.
      dut.io.ahb.HADDR     #= 0
      dut.io.ahb.HSEL      #= true
      dut.io.ahb.HWRITE    #= true
      dut.io.ahb.HSIZE     #= 0
      dut.io.ahb.HTRANS    #= 2 // NONSEQ
      dut.io.ahb.HMASTLOCK #= false
      
      for (value <- check) {
        dut.io.ahb.HADDR #= value.address
        dut.io.ahb.HSIZE #= value.hsize
        sleep(5)
        assert(dut.io.writeMask.toBigInt == value.writeMask, f"address 0x${value.address}%x, size ${value.hsize}: expected ${value.writeMask}%x, got ${dut.io.writeMask.toBigInt}%x")
        sleep(5)
      }
    }
  }
  
  test("writeMask8bit") {
    writeMaskTest(8, Seq(
      WriteMaskTestValue(0x0, 0, 0x1),
      WriteMaskTestValue(0x1, 0, 0x1),
    ))
  }
  test("writeMask16bit") {
    writeMaskTest(16, Seq(
      WriteMaskTestValue(0x0, 0, 0x1),
      WriteMaskTestValue(0x1, 0, 0x2),
      WriteMaskTestValue(0x0, 1, 0x3),
    ))
  }
  test("writeMask32bit") {
    writeMaskTest(32, Seq(
      WriteMaskTestValue(0x0, 0, 0x1),
      WriteMaskTestValue(0x1, 0, 0x2),
      WriteMaskTestValue(0x2, 0, 0x4),
      WriteMaskTestValue(0x3, 0, 0x8),
      WriteMaskTestValue(0x0, 1, 0x3),
      WriteMaskTestValue(0x2, 1, 0xc),
      WriteMaskTestValue(0x0, 2, 0xf),
    ))
  }
  test("writeMask64bit") {
    writeMaskTest(64, Seq(
      WriteMaskTestValue(0x0, 0, 0x01),
      WriteMaskTestValue(0x1, 0, 0x02),
      WriteMaskTestValue(0x2, 0, 0x04),
      WriteMaskTestValue(0x3, 0, 0x08),
      WriteMaskTestValue(0x4, 0, 0x10),
      WriteMaskTestValue(0x5, 0, 0x20),
      WriteMaskTestValue(0x6, 0, 0x40),
      WriteMaskTestValue(0x7, 0, 0x80),
      WriteMaskTestValue(0x0, 1, 0x03),
      WriteMaskTestValue(0x2, 1, 0x0c),
      WriteMaskTestValue(0x4, 1, 0x30),
      WriteMaskTestValue(0x6, 1, 0xc0),
      WriteMaskTestValue(0x0, 2, 0x0f),
      WriteMaskTestValue(0x4, 2, 0xf0),
      WriteMaskTestValue(0x0, 3, 0xff),
    ))
  }
  test("writeMask1024bit") {
    writeMaskTest(1024, Seq(
      WriteMaskTestValue(0x0, 0, 0x1),
      WriteMaskTestValue(0x1, 0, 0x2),
      WriteMaskTestValue(0x2, 0, 0x4),
      WriteMaskTestValue(0x3, 0, 0x8),
      WriteMaskTestValue(0x0, 1, 0x3),
      WriteMaskTestValue(0x2, 1, 0xc),
      WriteMaskTestValue(0x0, 2, 0xf),
      WriteMaskTestValue(0x0, 7, (BigInt(1) << 128) - 1),
    ))
  }
}

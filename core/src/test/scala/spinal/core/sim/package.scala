package spinal.core.sim

import org.scalatest.funsuite.AnyFunSuite

import spinal.core._
import spinal.tester.SpinalSimTester

class SimBigIntPimperTest extends AnyFunSuite {
  def toBytes(bi: BigInt, bits: Int = -1, endian: Endianness = LITTLE) =
    bi.toBytes(bits, endian).map(i => f"$i%02x").mkString(" ")
  test("positive") {
    // simple cases
    assert(toBytes(0xff) == "ff")
    assert(toBytes(0x80ff) == "ff 80")
    // check result is prepended if necessary
    assert(toBytes(0xff, 4*8) == "ff 00 00 00")
    // check with BIG endian
    assert(toBytes(0x80ff, endian=BIG) == "80 ff")
    assert(toBytes(0x80ff, 4*8, endian=BIG) == "00 00 80 ff")
    // check that bit length != n*8 works
    assert(toBytes(0x3ff, 10) == "ff 03")
    // also needs to work for BigInt bigger than Int
    assert(toBytes(0x1122334455667788L) == "88 77 66 55 44 33 22 11")
    // ... and longer than a Long
    assert(toBytes(BigInt("112233445566778899aabbccddeeff", 16)) == "ff ee dd cc bb aa 99 88 77 66 55 44 33 22 11")

    // negative numbers are sign extended to full bytes by default
    assert(toBytes(-1) == "ff")
    // result must be prepended correctly if expanded
    assert(toBytes(-1, 16) == "ff ff")
    // sign extend is only up to given number of bytes
    assert(toBytes(-1, 4) == "0f")
    // negative numbers also need to be extended to big bitwidths
    assert(toBytes(-1, 7*8) == "ff ff ff ff ff ff ff")
  }
  test("negative") {
    // must throw if too little space is given
    assertThrows[java.lang.AssertionError] { toBytes(0xffff, 8) }
    // length must be checked precisely
    assertThrows[java.lang.AssertionError] { toBytes(0x07, 2) }
    // space for sign bit must be available
    assertThrows[java.lang.AssertionError] { toBytes(-256, 8) }
  }
}

object SpinalSimAccessSubComponents {
  class SubSub extends Component {
    val io = new Bundle {
      val a,b = in UInt(8 bits)
      val result = out UInt(8 bits)
    }

    io.result := io.a ^ io.b
    val miaou = (io.a & io.b)

    val miaouVec = Vec(UInt(8 bits), 4)
    miaouVec := Vec(io.a,io.b,io.result,miaou)
  }

  class Sub extends Component {
    val io = new Bundle {
      val a,b = in UInt(8 bits)
      val result = out UInt(8 bits)
    }
    val subSubInst = new SubSub
    subSubInst.io.a <> io.a
    subSubInst.io.b <> io.b
    subSubInst.io.result <> io.result
  }

  class Dut extends Component {
    val io = new Bundle {
      val a,b = in UInt(8 bits)
      val result = out UInt(8 bits)
    }

    val subInst = new Sub
    subInst.io.a <> io.a
    subInst.io.b <> io.b
    subInst.io.result <> io.result
  }

}

class SpinalSimAccessSubComponents extends AnyFunSuite {
  SpinalSimTester { env =>
    import env._

    var compiled: SimCompiled[SpinalSimAccessSubComponents.Dut] = null
    test(prefix + "compile") {
      compiled = env.SimConfig.compile {
        val dut = new SpinalSimAccessSubComponents.Dut
        dut.subInst.subSubInst.miaouVec.simPublic()
        dut.subInst.subSubInst.io.a.simPublic()
        dut.subInst.subSubInst.miaou.simPublic()
        dut.subInst.subSubInst.miaou.pull()
        dut.subInst.subSubInst.io.pull()
        dut.subInst.subSubInst.miaouVec.pull()
        dut.subInst.subSubInst.miaouVec.pull()
        dut.subInst.subSubInst.miaouVec.pull()
        dut.subInst.subSubInst.miaouVec.pull()
        dut
      }
    }

    test(prefix + "simulate") {
      compiled.doSim { dut =>
        for (repeat <- 0 until 1000) {
          dut.io.a.randomize()
          dut.io.b.randomize()
          sleep(1)
          assert(dut.io.result.toInt == (dut.io.a.toInt ^ dut.io.b.toInt))
          assert(dut.subInst.subSubInst.miaou.toInt == (dut.io.a.toInt & dut.io.b.toInt))
          assert(dut.subInst.subSubInst.io.a.toInt == (dut.io.a.toInt))
          assert(dut.subInst.subSubInst.miaouVec(0).toInt == (dut.io.a.toInt))
          assert(dut.subInst.subSubInst.miaouVec(1).toInt == (dut.io.b.toInt))
          assert(dut.subInst.subSubInst.miaouVec(2).toInt == (dut.io.a.toInt ^ dut.io.b.toInt))
          assert(dut.subInst.subSubInst.miaouVec(3).toInt == (dut.io.a.toInt & dut.io.b.toInt))
        }
      }
    }
  }
}

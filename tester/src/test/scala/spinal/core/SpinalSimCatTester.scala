package spinal.core

import spinal.core.sim._
import spinal.tester.SpinalSimFunSuite

class SpinalSimCatTester extends SpinalSimFunSuite {

  onlyVerilator()

  test("binary_##") {
    SimConfig.doSim(new Component {
      val a, b = in UInt (4 bits)
      val result = out(a ## b)
    }) { dut =>
      dut.clockDomain.forkStimulus(10)
      for (_ <- 0 until 100) {
        val aVal = (Math.random() * 15).toInt
        val bVal = (Math.random() * 15).toInt
        dut.a #= aVal
        dut.b #= bVal
        dut.clockDomain.waitSampling()
        val expected = (BigInt(aVal) << 4) | BigInt(bVal)
        assert(
          dut.result.toBigInt == expected,
          s"a=$aVal b=$bVal: got 0x${dut.result.toBigInt.toString(16)}, expected 0x${expected.toString(16)}"
        )
      }
    }
  }

  test("nested_##") {
    // Tests nested ##: (a ## b) ## c produces Cat(Cat(a, b), c) in the expression tree
    SimConfig.doSim(new Component {
      val a, b, c = in UInt (4 bits)
      val result = out((a ## b) ## c)
    }) { dut =>
      dut.clockDomain.forkStimulus(10)
      for (_ <- 0 until 100) {
        val aVal = (Math.random() * 15).toInt
        val bVal = (Math.random() * 15).toInt
        val cVal = (Math.random() * 15).toInt
        dut.a #= aVal
        dut.b #= bVal
        dut.c #= cVal
        dut.clockDomain.waitSampling()
        val expected = (BigInt(aVal) << 8) | (BigInt(bVal) << 4) | BigInt(cVal)
        assert(
          dut.result.toBigInt == expected,
          s"a=$aVal b=$bVal c=$cVal: got 0x${dut.result.toBigInt.toString(16)}, expected 0x${expected.toString(16)}"
        )
      }
    }
  }

  test("multi_cat") {
    // Tests Cat(a, b, c) variadic form
    SimConfig.doSim(new Component {
      val a, b, c = in UInt (4 bits)
      val result = out(Cat(a, b, c))
    }) { dut =>
      dut.clockDomain.forkStimulus(10)
      for (_ <- 0 until 100) {
        val aVal = (Math.random() * 15).toInt
        val bVal = (Math.random() * 15).toInt
        val cVal = (Math.random() * 15).toInt
        dut.a #= aVal
        dut.b #= bVal
        dut.c #= cVal
        dut.clockDomain.waitSampling()
        val expected = (BigInt(aVal) << 8) | (BigInt(bVal) << 4) | BigInt(cVal)
        assert(
          dut.result.toBigInt == expected,
          s"Cat($aVal,$bVal,$cVal): got 0x${dut.result.toBigInt.toString(16)}, expected 0x${expected.toString(16)}"
        )
      }
    }
  }

  test("vec_asBits") {
    // Tests that Vec.asBits produces correct flat concatenation
    SimConfig.doSim(new Component {
      val inVec = in Vec (UInt(4 bits), 4)
      val result = out(inVec.asBits)
    }) { dut =>
      dut.clockDomain.forkStimulus(10)
      for (_ <- 0 until 100) {
        val vals = (0 until 4).map(_ => (Math.random() * 15).toInt)
        dut.inVec.zip(vals).foreach { case (sig, v) => sig #= v }
        dut.clockDomain.waitSampling()
        // asBits concatenates right-to-left: element 0 = LSB
        var expected = BigInt(0)
        for (i <- 3 to 0 by -1) {
          expected = (expected << 4) | BigInt(vals(i))
        }
        assert(
          dut.result.toBigInt == expected,
          s"vals=${vals.mkString(",")}: got 0x${dut.result.toBigInt.toString(16)}, expected 0x${expected.toString(16)}"
        )
      }
    }
  }

  test("vec_asBits_reference") {
    // Tests that Vec.asBits matches equivalent ## chain
    SimConfig.doSim(new Component {
      val v0, v1, v2, v3 = in UInt (4 bits)
      val vec = Vec(v0, v1, v2, v3)
      val asBitsResult = out(vec.asBits)
      val manualResult = out(v3 ## v2 ## v1 ## v0)
    }) { dut =>
      dut.clockDomain.forkStimulus(10)
      for (_ <- 0 until 100) {
        val vals = (0 until 4).map(_ => (Math.random() * 15).toInt)
        dut.v0 #= vals(0)
        dut.v1 #= vals(1)
        dut.v2 #= vals(2)
        dut.v3 #= vals(3)
        dut.clockDomain.waitSampling()
        assert(
          dut.asBitsResult.toBigInt == dut.manualResult.toBigInt,
          s"Mismatch: asBits = 0x${dut.asBitsResult.toBigInt.toString(16)}, manual = 0x${dut.manualResult.toBigInt.toString(16)}"
        )
      }
    }
  }

  test("mixed_width") {
    // Tests that concatenating signals of different widths works
    SimConfig.doSim(new Component {
      val a = in UInt (2 bits)
      val b = in UInt (6 bits)
      val c = in UInt (4 bits)
      val result = out(Cat(a, b, c))
    }) { dut =>
      dut.clockDomain.forkStimulus(10)
      for (_ <- 0 until 100) {
        val aVal = (Math.random() * 3).toInt
        val bVal = (Math.random() * 63).toInt
        val cVal = (Math.random() * 15).toInt
        dut.a #= aVal
        dut.b #= bVal
        dut.c #= cVal
        dut.clockDomain.waitSampling()
        val expected = (BigInt(aVal) << 10) | (BigInt(bVal) << 4) | BigInt(cVal)
        assert(
          dut.result.toBigInt == expected,
          s"a=$aVal b=$bVal c=$cVal: got 0x${dut.result.toBigInt.toString(16)}, expected 0x${expected.toString(16)}"
        )
      }
    }
  }

  test("single_operand") {
    // Tests that Cat(a) and a ## Bits(0 bits) work
    SimConfig.doSim(new Component {
      val a = in UInt (8 bits)
      val resultCat = out(Cat(a))
      val resultZeroPad = out(a ## Bits(0 bits))
    }) { dut =>
      dut.clockDomain.forkStimulus(10)
      for (_ <- 0 until 50) {
        val aVal = (Math.random() * 255).toInt
        dut.a #= aVal
        dut.clockDomain.waitSampling()
        assert(dut.resultCat.toBigInt == BigInt(aVal))
        assert(dut.resultZeroPad.toBigInt == BigInt(aVal))
      }
    }
  }
}

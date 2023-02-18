package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.core.sim._

class SpinalSimPackedBundleTester extends AnyFunSuite {
  test("pack") {
    SimConfig.withWave.compile(new Component {
      val packedBundle = new PackedBundle {
        val a = Bits(3 bit) // 0 to 2
        val b = Bits(3 bit).packFrom(4) // 4 to 6
        val c = Bits(3 bit).packTo(9) // 7 to 9
        val d = Bits(3 bit).pack(10 to 14) // 10 11 12 00 00
        val e = Bits(3 bit).pack(19 downto 15) // 00 00 17 18 19
        val f = Bits(6 bit).pack(20 to 24) // 20 21 22 23 24 (25 drop)
        val g = Bits(6 bit).pack(29 downto 25) // (25 drop) 26 27 28 29 30
      }
      val io = new Bundle {
        val a = in(Bits(3 bit))
        val b = in(Bits(3 bit))
        val c = in(Bits(3 bit))
        val d = in(Bits(3 bit))
        val e = in(Bits(3 bit))
        val f = in(Bits(6 bit))
        val g = in(Bits(6 bit))
        val packed = out(packedBundle.asBits.clone)
      }

      io.packed := RegNext(packedBundle.asBits)
      packedBundle.a := io.a
      packedBundle.b := io.b
      packedBundle.c := io.c
      packedBundle.d := io.d
      packedBundle.e := io.e
      packedBundle.f := io.f
      packedBundle.g := io.g
    }).doSim(dut => {
      dut.clockDomain.forkStimulus(10)

      def pack(a: BigInt, b: BigInt, c: BigInt, d: BigInt, e: BigInt, f: BigInt, g: BigInt): BigInt = {
        var res = BigInt(0)
        res += a
        res += b << 4
        res += c << 7
        res += d << 10
        res += e << 17
        res += (f & 0x1f) << 20
        res += (g >> 1) << 25
        res
      }

      for(i <- 0 to 29) {
        val n = 1 << i
        dut.io.a #= n & 0x7
        dut.io.b #= (n >> 4) & 0x7
        dut.io.c #= (n >> 7) & 0x7
        dut.io.d #= (n >> 10) & 0x7
        dut.io.e #= (n >> 17) & 0x7
        dut.io.f #= (n >> 20) & 0x3F
        dut.io.g #= (n >> 25) & 0x3F
        dut.clockDomain.waitFallingEdge()
        val packedCalc = pack(dut.io.a.toBigInt,
          dut.io.b.toBigInt,
          dut.io.c.toBigInt,
          dut.io.d.toBigInt,
          dut.io.e.toBigInt,
          dut.io.f.toBigInt,
          dut.io.g.toBigInt)
        // SpinalInfo(s"d = ${dut.io.d.toBigInt}, rev = ${bitReversed(dut.io.d.toBigInt, 6)}")
        assert(dut.io.packed.toBigInt == packedCalc, s"0x${dut.io.packed.toBigInt.toString(16)} =!= 0x${packedCalc.toString(16)}\n")
      }
    })
  }

  test("unpack") {
    SimConfig.withWave.compile(new Component {
      val packedBundle = new PackedBundle {
        val a = Bits(3 bit) // 0 to 2
        val b = Bits(3 bit).packFrom(4) // 4 to 6
        val c = Bits(3 bit).packTo(9) // 7 to 9
        val d = Bits(3 bit).pack(10 to 14) // 10 11 12 00 00
        val e = Bits(3 bit).pack(19 downto 15) // 00 00 17 18 19
        val f = Bits(6 bit).pack(20 to 24) // 19 20 21 22 23 (24 drop)
        val g = Bits(6 bit).pack(29 downto 25) // (25 drop) 26 27 28 29 30
      }
      val io = new Bundle {
        val a = out(Bits(3 bit))
        val b = out(Bits(3 bit))
        val c = out(Bits(3 bit))
        val d = out(Bits(3 bit))
        val e = out(Bits(3 bit))
        val f = out(Bits(6 bit))
        val g = out(Bits(6 bit))
        val packed = in(packedBundle.asBits.clone)
      }

      packedBundle.assignFromBits(RegNext(io.packed))
      io.a := packedBundle.a
      io.b := packedBundle.b
      io.c := packedBundle.c
      io.d := packedBundle.d
      io.e := packedBundle.e
      io.f := packedBundle.f
      io.g := packedBundle.g
    }).doSim(dut => {
      dut.clockDomain.forkStimulus(10)

      def pack(a: BigInt, b: BigInt, c: BigInt, d: BigInt, e: BigInt, f: BigInt, g: BigInt): BigInt = {
        var res = BigInt(0)
        res += a
        res += b << 4
        res += c << 7
        res += d << 10
        res += e << 17
        res += (f & 0x1f) << 20
        res += (g >> 1) << 25
        res
      }

      for (i <- 0 to 29) {
        dut.io.packed #= 1 << i
        dut.clockDomain.waitFallingEdge()
        var packedCalc = pack(dut.io.a.toBigInt,
          dut.io.b.toBigInt,
          dut.io.c.toBigInt,
          dut.io.d.toBigInt,
          dut.io.e.toBigInt,
          dut.io.f.toBigInt,
          dut.io.g.toBigInt)
        // Add the ignored bits back in
        packedCalc |= (dut.io.packed.toBigInt & 1 << 3)
        packedCalc |= (dut.io.packed.toBigInt & 1 << 13)
        packedCalc |= (dut.io.packed.toBigInt & 1 << 14)
        packedCalc |= (dut.io.packed.toBigInt & 1 << 15)
        packedCalc |= (dut.io.packed.toBigInt & 1 << 16)
        assert(dut.io.packed.toBigInt == packedCalc, s"0x${dut.io.packed.toBigInt.toString(16)} =!= 0x${packedCalc.toString(16)}\n")
      }
    })
  }
}

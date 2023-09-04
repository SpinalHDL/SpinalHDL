package spinal.lib

import spinal.core._
import spinal.core.sim._
import spinal.tester.SpinalAnyFunSuite

class SpinalSimPackedWordBundleTester extends SpinalAnyFunSuite {
  test("pack") {
    SimConfig.withWave
      .compile(new Component {
        val packedBundle = new PackedWordBundle(8 bit) {
          val a = Bits(4 bit).pack(3 downto 0)
          val b = Bits(8 bit).pack(7 downto 0).inWord(1)
          val c = Bits(16 bit).inWord(2)
          val d = Bits(3 bit).packTo(7).inWord(0)
        }

        val io = new Bundle {
          val a = in(Bits(4 bit))
          val b = in(Bits(8 bit))
          val c = in(Bits(16 bit))
          val d = in(Bits(3 bit))
          val packed = out(packedBundle.packed.clone)
        }

        io.packed := RegNext(packedBundle.packed)
        packedBundle.a := io.a
        packedBundle.b := io.b
        packedBundle.c := io.c
        packedBundle.d := io.d
      })
      .doSim(dut => {
        dut.clockDomain.forkStimulus(10)

        def pack(a: BigInt, b: BigInt, c: BigInt, d: BigInt): BigInt = {
          var res = BigInt(0)
          res += a
          res += b << 8
          res += c << 16
          res += d << 5
          res
        }

        for (i <- 0 to 31) {
          val n = 1 << i
          dut.io.a #= n & 0xf
          dut.io.b #= (n >> 8) & 0xff
          dut.io.c #= (n >> 16) & 0xffff
          dut.io.d #= (n >> 5) & 0x7
          dut.clockDomain.waitFallingEdge()
          val packedCalc = pack(dut.io.a.toBigInt, dut.io.b.toBigInt, dut.io.c.toBigInt, dut.io.d.toBigInt)
          assert(
            dut.io.packed.toBigInt == packedCalc,
            s"0x${dut.io.packed.toBigInt.toString(16)} =!= 0x${packedCalc.toString(16)}\n"
          )
        }
      })
  }

  test("unpack") {
    SimConfig.withWave
      .compile(new Component {
        val packedBundle = new PackedWordBundle(8 bit) {
          val a = Bits(4 bit).pack(3 downto 0)
          val b = Bits(8 bit).pack(7 downto 0).inWord(1)
          val c = Bits(16 bit).inWord(2)
          val d = Bits(3 bit).packTo(7).inWord(0)
        }

        val io = new Bundle {
          val a = out(Bits(4 bit))
          val b = out(Bits(8 bit))
          val c = out(Bits(16 bit))
          val d = out(Bits(3 bit))
          val packed = in(packedBundle.packed.clone)
        }

        packedBundle.unpack(RegNext(io.packed))
        io.a := packedBundle.a
        io.b := packedBundle.b
        io.c := packedBundle.c
        io.d := packedBundle.d
      })
      .doSim(dut => {
        dut.clockDomain.forkStimulus(10)

        def pack(a: BigInt, b: BigInt, c: BigInt, d: BigInt): BigInt = {
          var res = BigInt(0)
          res += a
          res += b << 8
          res += c << 16
          res += d << 5
          res
        }

        for (i <- 0 to 31) {
          // Using `BitInt(1) << i` here because `1 << 31` result in a signed long with max negative value
          dut.io.packed #= BigInt(1) << i
          dut.clockDomain.waitFallingEdge()
          var packedCalc = pack(dut.io.a.toBigInt, dut.io.b.toBigInt, dut.io.c.toBigInt, dut.io.d.toBigInt)
          // Add the ignored bits back in
          packedCalc |= (dut.io.packed.toBigInt & 1 << 4)
          assert(
            dut.io.packed.toBigInt == packedCalc,
            s"0x${dut.io.packed.toBigInt.toString(16)} =!= 0x${packedCalc.toString(16)}\n"
          )
        }
      })
  }
}

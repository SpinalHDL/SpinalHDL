package spinal.lib

import spinal.core._
import spinal.core.sim._
import spinal.tester.SpinalAnyFunSuite

class SpinalSimPackedBundleTester extends SpinalAnyFunSuite {
  test("pack legacy") {
    SimConfig
      .compile(new Component {
        val packedBundle = new PackedBundle {
          val a = Bits(3 bit) // 0 to 2
          val b = Bits(3 bit).packFrom(4) // 4 to 6
          val c = Bits(3 bit).packTo(9) // 7 to 9
          val d = Bits(3 bit).pack(10 to 14, LITTLE) // 10 11 12 00 00
          val e = Bits(3 bit).pack(15 to 19, BIG) // 00 00 17 18 19
          val f = Bits(6 bit).pack(20 to 24, LITTLE) // 19 20 21 22 23 (24 drop)
          val g = Bits(6 bit).pack(25 to 29, BIG) // (25 drop) 26 27 28 29 30
        }
        val io = new Bundle {
          val a = in(Bits(3 bit))
          val b = in(Bits(3 bit))
          val c = in(Bits(3 bit))
          val d = in(Bits(3 bit))
          val e = in(Bits(3 bit))
          val f = in(Bits(6 bit))
          val g = in(Bits(6 bit))
          val packed = out(packedBundle.packed.clone)
        }

        io.packed := RegNext(packedBundle.packed)
        packedBundle.a := io.a
        packedBundle.b := io.b
        packedBundle.c := io.c
        packedBundle.d := io.d
        packedBundle.e := io.e
        packedBundle.f := io.f
        packedBundle.g := io.g
      })
      .doSim(dut => {
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
          val n = 1 << i
          dut.io.a #= n & 0x7
          dut.io.b #= (n >> 4) & 0x7
          dut.io.c #= (n >> 7) & 0x7
          dut.io.d #= (n >> 10) & 0x7
          dut.io.e #= (n >> 17) & 0x7
          dut.io.f #= (n >> 20) & 0x3f
          dut.io.g #= (n >> 25) & 0x3f
          dut.clockDomain.waitFallingEdge()
          val packedCalc = pack(
            dut.io.a.toBigInt,
            dut.io.b.toBigInt,
            dut.io.c.toBigInt,
            dut.io.d.toBigInt,
            dut.io.e.toBigInt,
            dut.io.f.toBigInt,
            dut.io.g.toBigInt
          )

          assert(
            dut.io.packed.toBigInt == packedCalc,
            s"0x${dut.io.packed.toBigInt.toString(16)} =!= 0x${packedCalc.toString(16)}\n"
          )
        }
      })
  }

  test("unpack legacy") {
    SimConfig
      .compile(new Component {
        val packedBundle = new PackedBundle {
          val a = Bits(3 bit) // 0 to 2
          val b = Bits(3 bit).packFrom(4) // 4 to 6
          val c = Bits(3 bit).packTo(9) // 7 to 9
          val d = Bits(3 bit).pack(10 to 14, LITTLE) // 10 11 12 00 00
          val e = Bits(3 bit).pack(15 to 19, BIG) // 00 00 17 18 19
          val f = Bits(6 bit).pack(20 to 24, LITTLE) // 19 20 21 22 23 (24 drop)
          val g = Bits(6 bit).pack(25 to 29, BIG) // (25 drop) 26 27 28 29 30
        }
        val io = new Bundle {
          val a = out(Bits(3 bit))
          val b = out(Bits(3 bit))
          val c = out(Bits(3 bit))
          val d = out(Bits(3 bit))
          val e = out(Bits(3 bit))
          val f = out(Bits(6 bit))
          val g = out(Bits(6 bit))
          val packed = in(packedBundle.packed.clone)
        }

        packedBundle.unpack(RegNext(io.packed))
        io.a := packedBundle.a
        io.b := packedBundle.b
        io.c := packedBundle.c
        io.d := packedBundle.d
        io.e := packedBundle.e
        io.f := packedBundle.f
        io.g := packedBundle.g
      })
      .doSim(dut => {
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
          var packedCalc = pack(
            dut.io.a.toBigInt,
            dut.io.b.toBigInt,
            dut.io.c.toBigInt,
            dut.io.d.toBigInt,
            dut.io.e.toBigInt,
            dut.io.f.toBigInt,
            dut.io.g.toBigInt
          )
          // Add the ignored bits back in
          packedCalc |= (dut.io.packed.toBigInt & 1 << 3)
          packedCalc |= (dut.io.packed.toBigInt & 1 << 13)
          packedCalc |= (dut.io.packed.toBigInt & 1 << 14)
          packedCalc |= (dut.io.packed.toBigInt & 1 << 15)
          packedCalc |= (dut.io.packed.toBigInt & 1 << 16)
          assert(
            dut.io.packed.toBigInt == packedCalc,
            s"0x${dut.io.packed.toBigInt.toString(16)} =!= 0x${packedCalc.toString(16)}\n"
          )
        }
      })
  }

  test("pack") {
    SimConfig.withWave
      .compile(new Component {
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
          val packed = out(packedBundle.packed.clone)
        }

        io.packed := RegNext(packedBundle.packed)
        packedBundle.a := io.a
        packedBundle.b := io.b
        packedBundle.c := io.c
        packedBundle.d := io.d
        packedBundle.e := io.e
        packedBundle.f := io.f
        packedBundle.g := io.g
      })
      .doSim(dut => {
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
          val n = 1 << i
          dut.io.a #= n & 0x7
          dut.io.b #= (n >> 4) & 0x7
          dut.io.c #= (n >> 7) & 0x7
          dut.io.d #= (n >> 10) & 0x7
          dut.io.e #= (n >> 17) & 0x7
          dut.io.f #= (n >> 20) & 0x3f
          dut.io.g #= (n >> 25) & 0x3f
          dut.clockDomain.waitFallingEdge()
          val packedCalc = pack(
            dut.io.a.toBigInt,
            dut.io.b.toBigInt,
            dut.io.c.toBigInt,
            dut.io.d.toBigInt,
            dut.io.e.toBigInt,
            dut.io.f.toBigInt,
            dut.io.g.toBigInt
          )

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
          val packed = in(packedBundle.packed.clone)
        }

        packedBundle.unpack(RegNext(io.packed))
        io.a := packedBundle.a
        io.b := packedBundle.b
        io.c := packedBundle.c
        io.d := packedBundle.d
        io.e := packedBundle.e
        io.f := packedBundle.f
        io.g := packedBundle.g
      })
      .doSim(dut => {
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
          var packedCalc = pack(
            dut.io.a.toBigInt,
            dut.io.b.toBigInt,
            dut.io.c.toBigInt,
            dut.io.d.toBigInt,
            dut.io.e.toBigInt,
            dut.io.f.toBigInt,
            dut.io.g.toBigInt
          )
          // Add the ignored bits back in
          packedCalc |= (dut.io.packed.toBigInt & 1 << 3)
          packedCalc |= (dut.io.packed.toBigInt & 1 << 13)
          packedCalc |= (dut.io.packed.toBigInt & 1 << 14)
          packedCalc |= (dut.io.packed.toBigInt & 1 << 15)
          packedCalc |= (dut.io.packed.toBigInt & 1 << 16)
          assert(
            dut.io.packed.toBigInt == packedCalc,
            s"0x${dut.io.packed.toBigInt.toString(16)} =!= 0x${packedCalc.toString(16)}\n"
          )
        }
      })
  }

  test("pack mixed spec") {
    SimConfig
      .compile(new Component {
        val packedBundle = new PackedBundle {
          val a = Bits(4 bits) // 0 to 3
          val b = Bits(2 bits).packFrom(8) // 8 to 9
          val c = Bits(4 bits) // 10 to 13
          val d = Bits(2 bits).packTo(6) // 4 to 6
        }

        val io = new Bundle {
          val a = in(Bits(4 bits))
          val b = in(Bits(2 bits))
          val c = in(Bits(4 bits))
          val d = in(Bits(2 bits))
          val packed = out(Bits(packedBundle.getPackedWidth bits))
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
          res += c << 10
          res += d << 5
          res
        }

        for (i <- 0 to 13) {
          val n = 1 << i
          dut.io.a #= n & 0xF
          dut.io.b #= (n >> 8) & 0x3
          dut.io.c #= (n >> 10) & 0xF
          dut.io.d #= (n >> 5) & 0x3
          dut.clockDomain.waitFallingEdge()
          val packedCalc = pack(
            dut.io.a.toBigInt,
            dut.io.b.toBigInt,
            dut.io.c.toBigInt,
            dut.io.d.toBigInt
          )

          assert(
            dut.io.packed.toBigInt == packedCalc,
            s"Bit ${i}: 0x${dut.io.packed.toBigInt.toString(16)} =!= 0x${packedCalc.toString(16)}\n"
          )
        }
      })
  }

  test("unpack mixed spec") {
    SimConfig
      .compile(new Component {
        val packedBundle = new PackedBundle {
          val a = Bits(4 bits) // 0 to 3
          val b = Bits(2 bits).packFrom(8) // 8 to 9
          val c = Bits(4 bits) // 10 to 13
          val d = Bits(2 bits).packTo(6) // 5 to 6
        }

        val io = new Bundle {
          val a = out(Bits(4 bit))
          val b = out(Bits(2 bit))
          val c = out(Bits(4 bit))
          val d = out(Bits(2 bit))
          val packed = in(Bits(packedBundle.getPackedWidth bits))
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
          res += c << 10
          res += d << 5
          res
        }

        for (i <- 0 to 13) {
          dut.io.packed #= 1 << i
          dut.clockDomain.waitFallingEdge()
          var packedCalc = pack(
            dut.io.a.toBigInt,
            dut.io.b.toBigInt,
            dut.io.c.toBigInt,
            dut.io.d.toBigInt
          )
          // Add the ignored bits back in
          packedCalc |= (dut.io.packed.toBigInt & 1 << 4)
          packedCalc |= (dut.io.packed.toBigInt & 1 << 7)
          assert(
            dut.io.packed.toBigInt == packedCalc,
            s"Bit ${i}: 0x${dut.io.packed.toBigInt.toString(16)} =!= 0x${packedCalc.toString(16)}\n"
          )
        }
      })
  }
}

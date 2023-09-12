package spinal.core

import spinal.sim._
import spinal.core.sim._
import spinal.lib.misc.test.MultithreadedTester
import spinal.tester.{SpinalAnyFunSuite, SpinalSimTester}

import scala.concurrent.{Await, Future}
import scala.util.Random

object SpinalSimVerilatorIoTest{
  object State extends SpinalEnum{
    val A,B,C,D,E = newElement()
  }

  class newEnumTest(encoding : SpinalEnumEncoding) extends Area{
    val stateInput = in(State(encoding))
    val stateOutput = out(State(encoding))
    val stateDecoded = out(Bits(5 bits))

    stateDecoded := stateInput.mux[Bits](
      State.A -> 1,
      State.B -> 2,
      State.C -> 4,
      State.D -> 8,
      State.E -> 16
    )
    stateOutput := stateInput
  }

  class SpinalSimVerilatorIoTestTop extends Component {
    val io = new Bundle {
      val bool = in Bool()
      val u1  = in UInt (1 bits)
      val u8  = in UInt (8 bits)
      val u16 = in UInt (16 bits)
      val u31 = in UInt (31 bits)
      val u32 = in UInt (32 bits)
      val u63 = in UInt (63 bits)
      val u64 = in UInt (64 bits)
      val u65 = in UInt (65 bits)
      val u127 = in UInt (127 bits)
      val u128 = in UInt (128 bits)
      val s1  = in SInt (1 bits)
      val s8  = in SInt (8 bits)
      val s16 = in SInt (16 bits)
      val s31 = in SInt (31 bits)
      val s32 = in SInt (32 bits)
      val s63 = in SInt (63 bits)
      val s64 = in SInt (64 bits)
      val s65 = in SInt (65 bits)
      val s127 = in SInt (127 bits)
      val s128 = in SInt (128 bits)
      val sum = out SInt(32*5 bits)
    }

    val signeds = List(io.s1, io.s8, io.s16, io.s31, io.s32, io.s63, io.s64, io.s65, io.s127, io.s128)
    val unsigneds = List( io.u1, io.u8, io.u16, io.u31, io.u32, io.u63, io.u64, io.u65, io.u127, io.u128)
    io.sum := signeds.map(_.resize(widthOf(io.sum))).reduce(_ + _) + unsigneds.map(_.resize(widthOf(io.sum)).asSInt).reduce(_ + _)


    val miaou = out(Reg(Bits(128 bits)))
    miaou := 42
    val nativeEncoding = new newEnumTest(native)
    val binarySequentialEncoding =new newEnumTest(binarySequential)
    val binaryOneHotEncoding = new newEnumTest(binaryOneHot)
    val graySequentialEncoding = new newEnumTest(graySequential)
  }
}

class SpinalSimVerilatorIoTest extends SpinalAnyFunSuite {
  SpinalSimTester { env =>
    import env._
    var compiled: SimCompiled[SpinalSimVerilatorIoTest.SpinalSimVerilatorIoTestTop] = null

    def doTest: Unit = {
      compiled.doSim { dut =>
        def checkBoolean(value: Boolean, that: Bool): Unit = {
          that #= value
          sleep(1)
          assert(that.toBoolean == value, that.getName() + " " + value)
        }

        def checkInt(value: Int, that: BitVector): Unit = {
          that #= value
          sleep(1)
          assert(that.toInt == value, that.getName() + " " + value)
        }

        def checkLong(value: Long, that: BitVector): Unit = {
          that #= value
          sleep(1)
          assert(that.toLong == value, that.getName() + " " + value)
        }

        def checkBigInt(value: BigInt, that: BitVector): Unit = {
          that #= value
          sleep(1)
          assert(that.toBigInt == value, that.getName() + " " + value)
        }

        fork {
          dut.signeds.foreach(_ #= 0)
          dut.unsigneds.foreach(_ #= 0)
          while (true) {
            sleep(1)
            assert(dut.signeds.map(_.toBigInt).reduce(_ + _) + dut.unsigneds.map(_.toBigInt).reduce(_ + _) == dut.io.sum.toBigInt)
          }
        }


        (0 to 19).foreach { e =>
          List(false, true).foreach(value => checkBoolean(value, dut.io.bool))

          //checkInt
          List(0, 1).foreach(value => checkInt(value, dut.io.u1))
          List(0, 1, 127, 255).foreach(value => checkInt(value, dut.io.u8))
          List(0, 1, 0xFFFF).foreach(value => checkInt(value, dut.io.u16))
          List(0, 1, 0x7FFFFFFF).foreach(value => checkInt(value, dut.io.u31))

          List(0, -1).foreach(value => checkInt(value, dut.io.s1))
          List(0, 1, -1, 127, -128).foreach(value => checkInt(value, dut.io.s8))
          List(0, 1, -1, Short.MaxValue, Short.MinValue).foreach(value => checkInt(value, dut.io.s16))
          List(0, 1, -1, 0xFFFFFFFF, -1, Int.MaxValue, Int.MinValue).foreach(value => checkInt(value, dut.io.s32))

          //checkLong
          List(0, 1).foreach(value => checkLong(value, dut.io.u1))
          List(0, 1, 127, 255).foreach(value => checkLong(value, dut.io.u8))
          List(0, 1, 0xFFFF).foreach(value => checkLong(value, dut.io.u16))
          List(0, 1, 0x7FFFFFFF).foreach(value => checkLong(value, dut.io.u32))
          List(0l, 1l, 0x7FFFFFFFFFFFFFFFl).foreach(value => checkLong(value, dut.io.u63))

          List(0, -1).foreach(value => checkLong(value, dut.io.s1))
          List(0, 1, -1, 127, -128).foreach(value => checkLong(value, dut.io.s8))
          List(0, 1, -1, Short.MaxValue, Short.MinValue).foreach(value => checkLong(value, dut.io.s16))
          List(0, 1, -1, 0xFFFFFFFF, -1, Int.MaxValue, Int.MinValue).foreach(value => checkLong(value, dut.io.s32))
          List(0l, 1l, 0xFFFFFFFFFFFFFFFFl, -1l, Long.MaxValue, Long.MinValue).foreach(value => checkLong(value, dut.io.s64))

          //checkBigInt
          List(0, 1).foreach(value => checkBigInt(value, dut.io.u1))
          List(0, 1, 127, 255).foreach(value => checkBigInt(value, dut.io.u8))
          List(0, 1, 0xFFFF).foreach(value => checkBigInt(value, dut.io.u16))
          List(0, 1, 0x7FFFFFFF).foreach(value => checkBigInt(value, dut.io.u32))
          List(0l, 1l, 0x7FFFFFFFFFFFFFFFl).foreach(value => checkBigInt(value, dut.io.u63))

          List(0, -1).foreach(value => checkBigInt(value, dut.io.s1))
          List(0, 1, -1, 127, -128).foreach(value => checkBigInt(value, dut.io.s8))
          List(0, 1, -1, Short.MaxValue, Short.MinValue).foreach(value => checkBigInt(value, dut.io.s16))
          List(0, 1, -1, 0xFFFFFFFF, -1, Int.MaxValue, Int.MinValue).foreach(value => checkBigInt(value, dut.io.s32))
          List(0l, 1l, 0xFFFFFFFFFFFFFFFFl, -1l, Long.MaxValue, Long.MinValue).foreach(value => checkBigInt(value, dut.io.s64))

          forkJoin(
            () => Random.shuffle((0 to 1)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.u1)),
            () => Random.shuffle((0 to 8)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.u8)),
            () => Random.shuffle((0 to 16)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.u16)),
            () => Random.shuffle((0 to 31)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.u31)),
            () => Random.shuffle((0 to 32)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.u32)),
            () => Random.shuffle((0 to 63)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.u63)),
            () => Random.shuffle((0 to 64)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.u64)),
            () => Random.shuffle((0 to 65)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.u65)),
            () => Random.shuffle((0 to 127)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.u127)),
            () => Random.shuffle((0 to 128)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.u128)),
            () => Random.shuffle((0 to 1 - 1)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.s1)),
            () => Random.shuffle((0 to 8 - 1)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.s8)),
            () => Random.shuffle((0 to 16 - 1)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.s16)),
            () => Random.shuffle((0 to 31 - 1)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.s31)),
            () => Random.shuffle((0 to 32 - 1)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.s32)),
            () => Random.shuffle((0 to 62)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.s63)),
            () => Random.shuffle((0 to 63)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.s64)),
            () => Random.shuffle((0 to 64)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.s65)),
            () => Random.shuffle((0 to 126)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.s127)),
            () => Random.shuffle((0 to 127)).map(n => BigInt("0" + "1" * n, 2)).foreach(value => checkBigInt(value, dut.io.s128))
          )

          forkJoin(
            () => Random.shuffle((0 to 1 - 1)).map(n => -BigInt("0" + "1" * n, 2) - 1).foreach(value => checkBigInt(value, dut.io.s1)),
            () => Random.shuffle((0 to 8 - 1)).map(n => -BigInt("0" + "1" * n, 2) - 1).foreach(value => checkBigInt(value, dut.io.s8)),
            () => Random.shuffle((0 to 16 - 1)).map(n => -BigInt("0" + "1" * n, 2) - 1).foreach(value => checkBigInt(value, dut.io.s16)),
            () => Random.shuffle((0 to 31 - 1)).map(n => -BigInt("0" + "1" * n, 2) - 1).foreach(value => checkBigInt(value, dut.io.s31)),
            () => Random.shuffle((0 to 32 - 1)).map(n => -BigInt("0" + "1" * n, 2) - 1).foreach(value => checkBigInt(value, dut.io.s32)),
            () => Random.shuffle((0 to 62)).map(n => -BigInt("0" + "1" * n, 2) - 1).foreach(value => checkBigInt(value, dut.io.s63)),
            () => Random.shuffle((0 to 63)).map(n => -BigInt("0" + "1" * n, 2) - 1).foreach(value => checkBigInt(value, dut.io.s64)),
            () => Random.shuffle((0 to 64)).map(n => -BigInt("0" + "1" * n, 2) - 1).foreach(value => checkBigInt(value, dut.io.s65)),
            () => Random.shuffle((0 to 126)).map(n => -BigInt("0" + "1" * n, 2) - 1).foreach(value => checkBigInt(value, dut.io.s127)),
            () => Random.shuffle((0 to 127)).map(n => -BigInt("0" + "1" * n, 2) - 1).foreach(value => checkBigInt(value, dut.io.s128))
          )


          import SpinalSimVerilatorIoTest._
          def newEnumTest(test: newEnumTest) = {
            for (i <- 0 until 40) {
              val e = State.elements(Random.nextInt(State.elements.length))
              test.stateInput #= e
              sleep(1)
              assert(test.stateOutput.toEnum == e)
              assert(test.stateDecoded.toInt == (1 << e.position))

            }
          }

          newEnumTest(dut.nativeEncoding)
          newEnumTest(dut.binaryOneHotEncoding)
          newEnumTest(dut.binarySequentialEncoding)
          newEnumTest(dut.graySequentialEncoding)
        }
      }
    }

    test(prefix + "compile") {
      compiled = SimConfig.compile(new SpinalSimVerilatorIoTest.SpinalSimVerilatorIoTestTop)
    }

    test(prefix + "test1") {
      doTest
    }
    test(prefix + "test2") {
      doTest
    }
    test(prefix + "test3") {
      doTest
    }


    test(prefix + "testMulticore") {
      import scala.concurrent.ExecutionContext.Implicits.global

      val futures = for (i <- 0 to 8) yield {
        Future {
          doTest
        }
      }
      import scala.concurrent.duration._

      futures.foreach(f => Await.result(f, 60 seconds))
    }

  }
}

class SpinalSimVerilatorDeterministicTester extends SpinalAnyFunSuite {
  test("1") {
    val x = new MultithreadedTester {
      println("Compile")
      val c = SimConfig.compile(new Component {
        val wuff = in SInt(64 bits)
        val reg = out(Reg(SInt(64 bits)))
        reg := reg + 1
      })

      println("get ref")
      var regRef = 0l
      var randomRef = 0l
      var inRef = 0l
      c.doSim(seed = 42) { dut =>
        dut.wuff.randomize()
        sleep(2)
        regRef = dut.reg.toLong
        randomRef = simRandom.nextLong()
        inRef = dut.wuff.toLong
      }

      println("testit")
      for (i <- 0 until 100) test("test" + i){
        c.doSim(seed = 42) { dut =>
          dut.wuff.randomize()
          sleep(2)
          assert(dut.reg.toLong == regRef)
          assert(dut.wuff.toLong == inRef)
          assert(simRandom.nextLong() == randomRef)
        }
      }
      await()
    }
  }
}
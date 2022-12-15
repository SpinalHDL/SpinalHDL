package spinal.core.sim

import org.scalatest.funsuite.AnyFunSuite
import spinal.tester.SpinalSimTester

import spinal.core._

import java.io.File
import org.apache.commons.io.FileUtils
import spinal.lib.Delay
import java.io.PrintStream

import scala.concurrent.{Await, Future}
import scala.util.Random

object VerilatorCacheTester {
  case class ComponentA(n: Int, x: BigInt) extends Component {
    val io = new Bundle {
      val x = out Bits(n bits)
      val something = in Bits(n bits)
      val dummy = out Bits(n bits)
    }

    io.x := x

    io.dummy := Delay(io.something, 4000)
  }


  class VerilatorCacheUsedChecker(out: PrintStream) extends PrintStream(out) {
    var verilatorCompilationRunning = false
    var verilatorCompilationDone = false
    var verilatorCacheUsed = false

    def reset(): Unit = {
      verilatorCompilationRunning = false
      verilatorCompilationDone = false
      verilatorCacheUsed = false
    }

    def test(shouldUseCache: Boolean): Unit = {
      assert(verilatorCompilationRunning == false, "Verilator compilation should not be running now")
      assert(verilatorCompilationDone == true, "Verilator compilation should be done now")

      if (shouldUseCache) {
        assert(verilatorCacheUsed == true, "Verilator cache not used but it should be used")
      } else {
        assert(verilatorCacheUsed == false, "Verilator cache used but it should not be used")
      }
    }

    override def println(x: Any): Unit = {
      if (x.isInstanceOf[String]) {
        val str = x.asInstanceOf[String]

        if (str == "[Progress] Verilator compilation started") {
          verilatorCompilationRunning = true
          verilatorCompilationDone = false
          verilatorCacheUsed = false
        } else if (str.startsWith("[Progress] Verilator compilation done in ")) {
          verilatorCompilationRunning = false
          verilatorCompilationDone = true
        }

        if (verilatorCompilationRunning) {
          if (str == "[info] Found cached verilator binaries") {
            verilatorCacheUsed = true
          }
        }

        super.println(x)
      } else {
        super.println(x)
      }
    }
  }
}

class VerilatorCacheTester extends AnyFunSuite {
  import VerilatorCacheTester._

  val cacheDir = new File(SimConfig._workspacePath + "/.cache_cachetest")

  def deleteCache(): Unit = {
    if (cacheDir.exists()) {
      FileUtils.forceDelete(cacheDir)
    }
  }

  def testComponentA(verilatorCacheUsedChecker: VerilatorCacheUsedChecker, n: Int, x: BigInt, shouldUseCache: Boolean, disableCache: Boolean = false, maxCacheEntries: Int = -1, waveDepth: Int = -1): Long = {
    var cfg = SimConfig.cachePath(cacheDir.getAbsolutePath())
    if (disableCache) cfg = cfg.disableCache
    if (maxCacheEntries >= 0) cfg = cfg.maxCacheEntries(maxCacheEntries)
    if (waveDepth >= 0) cfg = cfg.withWave(waveDepth)

    verilatorCacheUsedChecker.reset()

    val tStart = System.nanoTime()
    cfg.compile(ComponentA(n, x)).doSim(dut => assert(dut.io.x.toBigInt == x))
    val duration = System.nanoTime()-tStart

    verilatorCacheUsedChecker.test(shouldUseCache)

    duration
  }

  test("verilator cache") {
    deleteCache()

    var durationWithoutCacheTotal: Double = 0
    var durationWithoutCacheCount = 0

    var durationWithCacheTotal: Double = 0
    var durationWithCacheCount = 0

    val verilatorCacheUsedChecker = new VerilatorCacheUsedChecker(Console.out)
    Console.withOut(verilatorCacheUsedChecker) {
      // first compilation with cache disabled
      durationWithoutCacheTotal += testComponentA(verilatorCacheUsedChecker, n=3, x=0, shouldUseCache=false, disableCache=true)
      durationWithoutCacheCount += 1

      // first compilation with cache enabled
      durationWithoutCacheTotal += testComponentA(verilatorCacheUsedChecker, n=3, x=0, shouldUseCache=false)
      durationWithoutCacheCount += 1

      // now cache should be used
      durationWithCacheTotal += testComponentA(verilatorCacheUsedChecker, n=3, x=0, shouldUseCache=true)
      durationWithCacheCount += 1

      // nothing changed, cache should be used again
      durationWithCacheTotal += testComponentA(verilatorCacheUsedChecker, n=3, x=0, shouldUseCache=true)
      durationWithCacheCount += 1

      // change component, should not use cache
      durationWithoutCacheTotal += testComponentA(verilatorCacheUsedChecker, n=3, x=1, shouldUseCache=false)
      durationWithoutCacheCount += 1

      // now the cache should be used again
      durationWithCacheTotal += testComponentA(verilatorCacheUsedChecker, n=3, x=1, shouldUseCache=true)
      durationWithCacheCount += 1

      // nothing changed, cache should be used again
      durationWithCacheTotal += testComponentA(verilatorCacheUsedChecker, n=3, x=1, shouldUseCache=true)
      durationWithCacheCount += 1

      // cache disabled, cache should not be used
      durationWithoutCacheTotal += testComponentA(verilatorCacheUsedChecker, n=3, x=1, shouldUseCache=false, disableCache=true)
      durationWithoutCacheCount += 1

      // cache reenabled, cache should be used
      durationWithCacheTotal += testComponentA(verilatorCacheUsedChecker, n=3, x=1, shouldUseCache=true)
      durationWithCacheCount += 1

      // restore previous component configuration, should use cache
      durationWithCacheTotal += testComponentA(verilatorCacheUsedChecker, n=3, x=0, shouldUseCache=true)
      durationWithCacheCount += 1

      // 2 cache entries used till now, should not use cache and generate 2 new cache entries
      durationWithoutCacheTotal += testComponentA(verilatorCacheUsedChecker, n=4, x=0, shouldUseCache=false, maxCacheEntries=4)
      durationWithoutCacheTotal += testComponentA(verilatorCacheUsedChecker, n=4, x=1, shouldUseCache=false, maxCacheEntries=4)
      durationWithoutCacheCount += 2

      // 4 (=max) cache entries used, should use cache
      durationWithCacheTotal += testComponentA(verilatorCacheUsedChecker, n=3, x=0, shouldUseCache=true, maxCacheEntries=4)
      Thread.sleep(1100)    // ensure different timestamps
      durationWithCacheTotal += testComponentA(verilatorCacheUsedChecker, n=3, x=1, shouldUseCache=true, maxCacheEntries=4)
      Thread.sleep(1100)    // ensure different timestamps
      durationWithCacheTotal += testComponentA(verilatorCacheUsedChecker, n=4, x=0, shouldUseCache=true, maxCacheEntries=4)
      Thread.sleep(1100)    // ensure different timestamps
      durationWithCacheTotal += testComponentA(verilatorCacheUsedChecker, n=4, x=1, shouldUseCache=true, maxCacheEntries=4)
      Thread.sleep(1100)    // ensure different timestamps
      durationWithCacheCount += 4

      // 4 (=max) cache entries used, new configuration, should not use cache
      durationWithoutCacheTotal += testComponentA(verilatorCacheUsedChecker, n=4, x=2, shouldUseCache=false, maxCacheEntries=4)
      durationWithoutCacheCount += 1

      // cache entry deleted in previous test, should not use cache
      durationWithoutCacheTotal += testComponentA(verilatorCacheUsedChecker, n=3, x=0, shouldUseCache=false, maxCacheEntries=5)
      durationWithoutCacheCount += 1

      // other cache entries (+ new cache entry) should not have been deleted, should use cache
      durationWithCacheTotal += testComponentA(verilatorCacheUsedChecker, n=3, x=1, shouldUseCache=true, maxCacheEntries=5)
      durationWithCacheTotal += testComponentA(verilatorCacheUsedChecker, n=4, x=0, shouldUseCache=true, maxCacheEntries=5)
      durationWithCacheTotal += testComponentA(verilatorCacheUsedChecker, n=4, x=1, shouldUseCache=true, maxCacheEntries=5)
      durationWithCacheTotal += testComponentA(verilatorCacheUsedChecker, n=4, x=2, shouldUseCache=true, maxCacheEntries=5)
      durationWithCacheCount += 4

      // change sim config, should not use cache
      durationWithoutCacheTotal += testComponentA(verilatorCacheUsedChecker, n=4, x=2, shouldUseCache=false, waveDepth=1)
      durationWithoutCacheCount += 1
    }

    val durationWithoutCacheAvg = durationWithoutCacheTotal / durationWithoutCacheCount
    val durationWithCacheAvg = durationWithCacheTotal / durationWithCacheCount

    assert(durationWithCacheAvg < durationWithoutCacheAvg, "Verilator compilation needs more time when using the cache")
  }
}

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

class SpinalSimVerilatorIoTest extends AnyFunSuite {
  import SpinalSimVerilatorIoTest._

  SpinalSimTester { env =>
    import env._
    var compiled: SimCompiled[SpinalSimVerilatorIoTestTop] = null

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
      compiled = env.SimConfig.compile(new SpinalSimVerilatorIoTestTop)
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

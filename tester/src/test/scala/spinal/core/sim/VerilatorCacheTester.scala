package spinal.core.sim

import spinal.core._
import spinal.lib.Delay
import spinal.tester.SpinalAnyFunSuite

import java.io.{File, PrintStream}
import org.apache.commons.io.FileUtils

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

class VerilatorCacheTester extends SpinalAnyFunSuite {
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


//  test("large_design") {
//    SimConfig.compile(new Component{
//      val i = in UInt(8 bits)
//      val o = out UInt(8 bits)
//      o := Delay(i, 10000)
//    })
//  }

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


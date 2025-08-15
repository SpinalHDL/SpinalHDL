package spinal.tester.scalatest

import spinal.core._
import spinal.core.sim._
import spinal.tester.SpinalAnyFunSuite

import scala.collection.mutable
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
 * SpinalHDL Random Number Isolation Test Suite
 *
 * This test suite focuses on isolation and consistency verification of SpinalHDL's
 * random number generation mechanisms, complementing the basic functionality tests
 * in SpinalSimRandomizeTester.scala.
 *
 * Test Coverage:
 * 1. SpinalHDL's simRandom isolation between simulation runs
 * 2. Signal randomization consistency and reproducibility
 * 3. Seed independence verification
 */
case class RandomIsolationTestDut() extends Component {
  val io = new Bundle {
    // Randomizable input signals for testing randomize() method
    val randomInput8 = in UInt(8 bits)
    val randomInput16 = in UInt(16 bits)
    val randomInput32 = in UInt(32 bits)
    val randomInputBool = in Bool()

    // Output signals that directly reflect the randomized inputs
    val randomData8 = out UInt(8 bits)
    val randomData16 = out UInt(16 bits)
    val randomData32 = out UInt(32 bits)
    val randomBool = out Bool()
  }

  // Direct output connections - no register delay
  io.randomData8 := io.randomInput8
  io.randomData16 := io.randomInput16
  io.randomData32 := io.randomInput32
  io.randomBool := io.randomInputBool
}

class SpinalSimRandomIsolationTester extends SpinalAnyFunSuite {
  var dut: SimCompiled[RandomIsolationTestDut] = null
  val testIterations = 30
  val testTimeout = 120.seconds

  test("compile") {
    dut = SimConfig
      .withConfig(SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)
      ))
      .compile(RandomIsolationTestDut())
  }

  test("same seed produces identical results") {
    val testSeed = 12345

    def runSimulation(name: String): (List[Long], List[Long], List[Long]) = {
      val simRandomSeq = mutable.ArrayBuffer[Long]()
      val signalRandomSeq8 = mutable.ArrayBuffer[Long]()
      val signalRandomSeq16 = mutable.ArrayBuffer[Long]()

      dut.doSim(seed = testSeed, name = name) { dut =>
        dut.clockDomain.forkStimulus(10)

        for (_ <- 0 until testIterations) {
          // Collect SpinalHDL's simRandom
          simRandomSeq += simRandom.nextLong()

          // Randomize input signals and collect output results
          dut.io.randomInput8.randomize()
          dut.io.randomInput16.randomize()
          signalRandomSeq8 += dut.io.randomData8.toLong
          signalRandomSeq16 += dut.io.randomData16.toLong

          dut.clockDomain.waitSampling()
        }
      }

      (simRandomSeq.toList, signalRandomSeq8.toList, signalRandomSeq16.toList)
    }

    // Run two simulations with the same seed
    val (simSeq1, sigSeq8_1, sigSeq16_1) = runSimulation("test1")
    val (simSeq2, sigSeq8_2, sigSeq16_2) = runSimulation("test2")

    // Verify consistency
    assert(simSeq1 == simSeq2, "simRandom sequences should be identical!")
    assert(sigSeq8_1 == sigSeq8_2, "8-bit signal randomization sequences should be identical!")
    assert(sigSeq16_1 == sigSeq16_2, "16-bit signal randomization sequences should be identical!")
  }

  test("different seeds produce different results") {
    val seed1 = 11111
    val seed2 = 22222

    def runWithSeed(seed: Int, name: String): (List[Long], List[Long]) = {
      val simRandomSeq = mutable.ArrayBuffer[Long]()
      val signalRandomSeq = mutable.ArrayBuffer[Long]()

      dut.doSim(seed = seed, name = name) { dut =>
        dut.clockDomain.forkStimulus(10)

        for (_ <- 0 until testIterations) {
          simRandomSeq += simRandom.nextLong()
          dut.io.randomInput16.randomize()
          dut.clockDomain.waitSampling()
          signalRandomSeq += dut.io.randomData16.toLong
        }
      }

      (simRandomSeq.toList, signalRandomSeq.toList)
    }

    val (simSeq1, sigSeq1) = runWithSeed(seed1, "seed1")
    val (simSeq2, sigSeq2) = runWithSeed(seed2, "seed2")

    // Calculate difference ratios
    val simDiff = simSeq1.zip(simSeq2).count { case (a, b) => a != b }
    val sigDiff = sigSeq1.zip(sigSeq2).count { case (a, b) => a != b }
    val simRatio = simDiff.toDouble / testIterations
    val sigRatio = sigDiff.toDouble / testIterations

    assert(simRatio > 0.95, s"Different seeds should produce mostly different simRandom values, actual difference: ${simRatio * 100}%")
    assert(sigRatio > 0.95, s"Different seeds should produce mostly different signal randomization, actual difference: ${sigRatio * 100}%")
  }

  test("concurrent simulations with same seed") {
    val testSeed = 33333
    val numRuns = 3

    // Create concurrent simulation tasks
    val futures = (1 to numRuns).map { run =>
      Future {
        val simRandomSeq = mutable.ArrayBuffer[Long]()

        dut.doSim(seed = testSeed, name = s"concurrent_${testSeed}_$run") { dut =>
          dut.clockDomain.forkStimulus(10)

          for (_ <- 0 until 5) { 
            simRandomSeq += simRandom.nextLong()
            dut.clockDomain.waitSampling()
          }
        }

        simRandomSeq.toList
      }
    }

    // Wait for all simulations to complete
    val results = Await.result(Future.sequence(futures), testTimeout)

    // Verify consistency - all runs with same seed should produce identical sequences
    val firstSeq = results.head
    val allSame = results.forall(_ == firstSeq)
    assert(allSame, s"All runs with seed $testSeed should produce identical sequences")
  }

  test("reproducibility across multiple runs") {
    val testSeed = 98765
    val numRuns = 3

    // Run the same test multiple times to verify reproducibility
    val allResults = (1 to numRuns).map { run =>
      val simRandomSeq = mutable.ArrayBuffer[Long]()
      val signalRandomSeq = mutable.ArrayBuffer[Long]()

      dut.doSim(seed = testSeed, name = s"repro_$run") { dut =>
        dut.clockDomain.forkStimulus(10)

        for (_ <- 0 until testIterations) {
          simRandomSeq += simRandom.nextLong()
          dut.io.randomInput32.randomize()
          signalRandomSeq += dut.io.randomData32.toLong
          dut.clockDomain.waitSampling()
        }
      }

      (simRandomSeq.toList, signalRandomSeq.toList)
    }

    // Verify all runs produce identical results
    val (referenceSimSeq, referenceSignalSeq) = allResults.head
    allResults.foreach { case (simSeq, signalSeq) =>
      assert(simSeq == referenceSimSeq, "All runs should produce identical simRandom sequences")
      assert(signalSeq == referenceSignalSeq, "All runs should produce identical signal randomization sequences")
    }
  }
}

package spinal.tester.scalatest

import spinal.core._
import spinal.core.sim._
import spinal.tester.SpinalAnyFunSuite

import scala.collection.mutable
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
 * Verilator Random Number Generator Isolation Test Suite
 *
 * This test suite focuses on Verilator's VL_RAND_RESET mechanism and VerilatedContext
 * architecture isolation testing, complementing SpinalHDL's random isolation tests.
 *
 * Key Focus Areas:
 * 1. Signal initialization randomization isolation between simulation runs
 * 2. VerilatedContext architecture validation
 * 3. VL_RAND_RESET function verification and consistency testing
 */
case class VerilatorRandomTestDut() extends Component {
  val io = new Bundle {
    val enable = in Bool()

    // Signals for testing Verilator randomization
    val verilatorRandom8 = out UInt(8 bits)
    val verilatorRandom16 = out UInt(16 bits)
    val verilatorRandom32 = out UInt(32 bits)
    val verilatorRandom64 = out UInt(64 bits)
    val verilatorRandomBool = out Bool()

    // Signals for testing initialization with explicit init values
    val initReg8 = out UInt(8 bits)
    val initReg16 = out UInt(16 bits)
    val initReg32 = out UInt(32 bits)
    val initReg64 = out UInt(64 bits)
    val initRegBool = out Bool()
  }

  // Registers without init() - rely on Verilator VL_RAND_RESET mechanism
  val randomReg8 = Reg(UInt(8 bits))
  val randomReg16 = Reg(UInt(16 bits))
  val randomReg32 = Reg(UInt(32 bits))
  val randomReg64 = Reg(UInt(64 bits))
  val randomRegBool = Reg(Bool())

  // Simple assignment logic to avoid compilation errors while preserving Verilator initialization
  when(io.enable) {
    randomReg8 := randomReg8
    randomReg16 := randomReg16
    randomReg32 := randomReg32
    randomReg64 := randomReg64
    randomRegBool := randomRegBool
  }

  // Registers with explicit init() values for comparison
  val initReg8 = Reg(UInt(8 bits)) init(0)
  val initReg16 = Reg(UInt(16 bits)) init(0)
  val initReg32 = Reg(UInt(32 bits)) init(0)
  val initReg64 = Reg(UInt(64 bits)) init(0)
  val initRegBool = Reg(Bool()) init(False)

  // Simple counter logic to avoid simulation warnings
  when(io.enable) {
    initReg8 := initReg8 + 1
    initReg16 := initReg16 + 1
    initReg32 := initReg32 + 1
    initReg64 := initReg64 + 1
    initRegBool := !initRegBool
  }

  // Output connections
  io.verilatorRandom8 := randomReg8
  io.verilatorRandom16 := randomReg16
  io.verilatorRandom32 := randomReg32
  io.verilatorRandom64 := randomReg64
  io.verilatorRandomBool := randomRegBool

  io.initReg8 := initReg8
  io.initReg16 := initReg16
  io.initReg32 := initReg32
  io.initReg64 := initReg64
  io.initRegBool := initRegBool
}

class VerilatorRandomIsolationTester extends SpinalAnyFunSuite {
  var dut: SimCompiled[VerilatorRandomTestDut] = null
  val testTimeout = 120.seconds

  test("compile") {
    dut = SimConfig
      .withConfig(SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)
      ))
      .compile(VerilatorRandomTestDut())
  }

  /**
   * Run a single simulation and collect initial values
   */
  def runSingleSimulation(instanceName: String, seed: Int): Map[String, Long] = {
    val verilatorRandomValues = mutable.Map[String, Long]()

    dut.doSim(seed = seed, name = instanceName) { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.enable #= true

      dut.clockDomain.waitSampling(1)

      // Collect Verilator randomized signal initial values
      verilatorRandomValues("random8") = dut.io.verilatorRandom8.toLong
      verilatorRandomValues("random16") = dut.io.verilatorRandom16.toLong
      verilatorRandomValues("random32") = dut.io.verilatorRandom32.toLong
      verilatorRandomValues("random64") = dut.io.verilatorRandom64.toBigInt.toLong
      verilatorRandomValues("randomBool") = if (dut.io.verilatorRandomBool.toBoolean) 1L else 0L

      dut.clockDomain.waitSampling()
    }

    verilatorRandomValues.toMap
  }

  test("same seed produces identical Verilator random values") {
    val testSeed = 12345
    val numInstances = 3

    // Run multiple instances with the same seed
    val results = (1 to numInstances).map { i =>
      runSingleSimulation(s"consistency_$i", testSeed)
    }

    // Verify all instances produce identical initial values
    val referenceValues = results.head
    results.foreach { result =>
      assert(result == referenceValues,
        "All instances with same seed should have identical Verilator random values")
    }
  }

  test("different seeds produce different Verilator random values") {
    val seeds = List(11111, 22222, 33333, 44444)

    // Run simulations with different seeds
    val results = seeds.zipWithIndex.map { case (seed, i) =>
      runSingleSimulation(s"diffSeed_$i", seed)
    }

    // Verify different seeds produce different results
    val allDifferent = results.combinations(2).forall { case List(r1, r2) =>
      r1 != r2
    }

    assert(allDifferent, "Different seeds should produce different Verilator random values")
  }

  test("concurrent simulations maintain isolation") {
    val testSeed = 55555
    val numRuns = 3

    // Create concurrent simulation tasks
    val futures = (1 to numRuns).map { run =>
      Future {
        runSingleSimulation(s"concurrent_${testSeed}_$run", testSeed)
      }
    }

    // Wait for all simulations to complete
    val results = Await.result(Future.sequence(futures), testTimeout)

    // Verify consistency - all runs with same seed should produce identical values
    val firstResult = results.head
    val allSame = results.forall(_ == firstResult)
    assert(allSame, s"All concurrent runs with seed $testSeed should produce identical values")
  }

  test("VerilatedContext architecture validation") {
    // This test verifies the VerilatedContext isolation mechanism
    val testSeeds = List(24680, 13579, 97531)

    val results = testSeeds.map { seed =>
      val result = runSingleSimulation(s"context_arch_$seed", seed)
      (seed, result)
    }

    // Verify that each seed produces unique results
    val seedToValues = results.toMap
    val allValues = seedToValues.values.toList
    val uniqueValues = allValues.toSet
    assert(uniqueValues.size == allValues.size,
      "Each seed should produce unique values (VerilatedContext isolation working)")

    // Run the same seeds again to verify reproducibility
    val reproducibilityResults = testSeeds.map { seed =>
      val result = runSingleSimulation(s"context_repro_$seed", seed)
      (seed, result)
    }

    // Verify reproducibility
    reproducibilityResults.foreach { case (seed, values) =>
      assert(seedToValues(seed) == values,
        s"Seed $seed should produce identical values on repeated runs")
    }
  }
}

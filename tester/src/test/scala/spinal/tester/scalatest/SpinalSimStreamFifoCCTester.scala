package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.StreamFifoCC
import spinal.tester

import scala.collection.mutable
import scala.util.Random

class SpinalSimStreamFifoCCTester extends SpinalSimFunSuite {

//  onlyVerilator()

  def testbench(dut : StreamFifoCC[Bits]): Unit ={
    val queueModel = mutable.Queue[Long]()

    //Push data randomly and fill the queueModel with pushed transactions
    val pushThread = fork{
      while(true){
        dut.io.push.valid.randomize()
        dut.io.push.payload.randomize()
        dut.pushClock.waitSampling()
        if(dut.io.push.valid.toBoolean && dut.io.push.ready.toBoolean){
          queueModel.enqueue(dut.io.push.payload.toLong)
        }
      }
    }

    //Pop data randomly and check that it match with the queueModel
    val popThread = fork{
      dut.io.pop.ready #= false
      dut.pushClock.waitSampling()
      dut.popClock.waitSampling()
      for(repeat <- 0 until 10000){
        dut.io.pop.ready.randomize()
        dut.popClock.waitSampling()
        if(dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean){
          assert(dut.io.pop.payload.toLong == queueModel.dequeue())
        }
      }
      simSuccess()
    }
  }

  for(pushResetLevel <- List(LOW, HIGH);
      popResetLevel <- List(LOW, HIGH);
      popResetEnable <- List(false, true)) {
    val postfix = s"${pushResetLevel}_${popResetLevel}_${popResetEnable}"
    test("testAsyncReset_" + postfix) {
      //Compile the simulator
      val compiled = SimConfig.allOptimisation.compile(
        rtl = new StreamFifoCC(
          dataType = Bits(32 bits),
          depth = 32,
          pushClock = ClockDomain.external("clkA", config = ClockDomainConfig(resetActiveLevel = pushResetLevel)),
          popClock = ClockDomain.external("clkB", withReset = popResetEnable, config = ClockDomainConfig(resetActiveLevel = popResetLevel)),
          withPopBufferedReset = !popResetEnable
        )
      )

      //Run the simulation
      compiled.doSimUntilVoid { dut =>
        fork {
          //Clear clock domains signals, to be sure the simulation capture their first edge.
          dut.pushClock.fallingEdge()
          dut.popClock.fallingEdge()
          dut.pushClock.deassertReset()
          if(popResetEnable) dut.popClock.deassertReset()
          sleep(0)

          //Do the resets
          dut.pushClock.assertReset()
          if(popResetEnable)dut.popClock.assertReset()
          sleep(10)
          dut.pushClock.deassertReset()
          if(popResetEnable)dut.popClock.deassertReset()
          sleep(1)

          //Forever, randomly toggle one of the clocks (will create asynchronous clocks without fixed frequencies)
          while (true) {
            if (Random.nextBoolean()) {
              dut.pushClock.clockToggle()
            } else {
              dut.popClock.clockToggle()
            }
            sleep(1)
          }
        }
        testbench(dut)
      }
    }

    test("testSyncReset_" + postfix) {
      //Compile the simulator
      val compiled = SimConfig.withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))).allOptimisation.compile(
        rtl = new StreamFifoCC(
          dataType = Bits(32 bits),
          depth = 32,
          pushClock = ClockDomain.external("clkA", config = ClockDomainConfig(resetActiveLevel = pushResetLevel)),
          popClock = ClockDomain.external("clkB", withReset = popResetEnable, config = ClockDomainConfig(resetActiveLevel = popResetLevel)),
          withPopBufferedReset = !popResetEnable
        )
      )

      //Run the simulation
      compiled.doSimUntilVoid { dut =>
        dut.pushClock.fallingEdge()
        dut.popClock.fallingEdge()
        dut.pushClock.deassertReset()
        if(popResetEnable) dut.popClock.deassertReset()
        fork {
          //Clear clock domains signals, to be sure the simulation capture their first edge.
          sleep(0)

          //Do the resets
          dut.pushClock.assertReset()
          if(popResetEnable) dut.popClock.assertReset()
          sleep(10)
          dut.pushClock.deassertReset()
          if(popResetEnable) dut.popClock.deassertReset()
          sleep(1)
        }

        fork {
          sleep(1)
          for (i <- 0 until 5) {
            dut.pushClock.clockToggle()
            dut.popClock.clockToggle()
            sleep(1)
          }
          //Forever, randomly toggle one of the clocks (will create asynchronous clocks without fixed frequencies)
          while (true) {
            if (Random.nextBoolean()) {
              dut.pushClock.clockToggle()
            } else {
              dut.popClock.clockToggle()
            }
            sleep(1)
          }
        }
        testbench(dut)
      }
    }
  }

  test("testBootReset") {
    //Compile the simulator
    val compiled = SimConfig.withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT))).allOptimisation.compile(
      rtl = new StreamFifoCC(
        dataType = Bits(32 bits),
        depth = 32,
        pushClock = ClockDomain.external("clkA"),
        popClock = ClockDomain.external("clkB", withReset = false)
      )
    )

    //Run the simulation
    compiled.doSimUntilVoid { dut =>
      fork {
        dut.pushClock.fallingEdge()
        dut.popClock.fallingEdge()
        sleep(1)
        //Forever, randomly toggle one of the clocks (will create asynchronous clocks without fixed frequencies)
        while (true) {
          if (Random.nextBoolean()) {
            dut.pushClock.clockToggle()
          } else {
            dut.popClock.clockToggle()
          }
          sleep(1)
        }
      }
      testbench(dut)
    }
  }
}


object TesterBugPlay extends App{

  def testbench(dut : StreamFifoCC[Bits]): Unit ={
    val queueModel = mutable.Queue[Long]()

    //Push data randomly and fill the queueModel with pushed transactions
    val pushThread = fork{
      while(true){
        dut.io.push.valid.randomize()
        dut.io.push.payload.randomize()
        dut.pushClock.waitSampling()
        if(dut.io.push.valid.toBoolean && dut.io.push.ready.toBoolean){
          queueModel.enqueue(dut.io.push.payload.toLong)
        }
      }
    }

    //Pop data randomly and check that it match with the queueModel
    val popThread = fork{
      for(repeat <- 0 until 10000){
        dut.io.pop.ready.randomize()
        dut.popClock.waitSampling()
        if(dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean){
          assert(dut.io.pop.payload.toLong == queueModel.dequeue())
        }
      }
      simSuccess()
    }
  }
  val compiled = SimConfig.withIVerilog.withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))).allOptimisation.compile(
    rtl = new StreamFifoCC(
      dataType = Bits(32 bits),
      depth = 32,
      pushClock = ClockDomain.external("clkA"),
      popClock = ClockDomain.external("clkB", withReset = false)
    )
  )

  for(i <- 132 until 1000) {

    //Run the simulation
    compiled.doSimUntilVoid(seed = i) { dut =>
      dut.pushClock.fallingEdge()
      dut.popClock.fallingEdge()
      dut.pushClock.deassertReset()
//      dut.popClock.deassertReset()
      fork {
        //Clear clock domains signals, to be sure the simulation capture their first edge.
        sleep(0)

        //Do the resets
        dut.pushClock.assertReset()
//        dut.popClock.assertReset()
        sleep(10)
        dut.pushClock.deassertReset()
//        dut.popClock.deassertReset()
        sleep(1)
      }

      fork {
        sleep(1)
        for(i <- 0 until 5) {
          dut.pushClock.clockToggle()
          dut.popClock.clockToggle()
          sleep(1)
        }
        //Forever, randomly toggle one of the clocks (will create asynchronous clocks without fixed frequencies)
        while (true) {
          if (Random.nextBoolean()) {
            dut.pushClock.clockToggle()
          } else {
            dut.popClock.clockToggle()
          }
          sleep(1)
        }
      }
      testbench(dut)
    }
  }
}
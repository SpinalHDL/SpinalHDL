package spinal.tester.scalatest

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.wishbone._
import spinal.lib.wishbone.sim._
import spinal.lib.sim._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class WishboneSimpleSlave(config : WishboneConfig, regFeedback : Boolean) extends Component{
  val io = new Bundle{
    val bus = slave(Wishbone(config))
  }
  val busCtrl = WishboneSlaveFactory(io.bus, regFeedback)
  val regA = busCtrl.createReadOnly(Bits(busCtrl.busDataWidth bits), 10 * config.dataWidth / 8)
  regA := 100
  val regB = busCtrl.createReadAndWrite(Bits(busCtrl.busDataWidth bits), 0)
  regB.init(1)

  val mem = Mem((0 until 32).map(U(_, config.dataWidth bits)))
  busCtrl.readSyncMemMultiWord(mem, 20 * config.dataWidth / 8)
  busCtrl.writeMemMultiWord(mem, 20 * config.dataWidth / 8)
}

class SpinalSimWishboneSlaveFactoryTester extends SpinalAnyFunSuite{
  def testBus(conf:WishboneConfig, description : String = "", regFeedback : Boolean): Unit = {
    val fixture = SimConfig.allOptimisation.compile(rtl = new WishboneSimpleSlave(conf, regFeedback).setDefinitionName("WishboneSimpleSlave_" + description))
    fixture.doSim(description){ dut =>
      dut.clockDomain.forkStimulus(period=10)
      SimTimeout(1000*20*100)

      val dri = WishboneDriver(dut.io.bus)

      dut.clockDomain.waitSampling()

      val wordInc = conf.wordAddressInc(AddressGranularity.WORD)

      val scoreboard = ScoreboardInOrder[WishboneTransaction]()
      for(ref <- List(
        WishboneTransaction(10 * wordInc, 100), WishboneTransaction(0, 1),
        WishboneTransaction(10 * wordInc, 100), WishboneTransaction(0, 1),
        WishboneTransaction(10 * wordInc, 100), WishboneTransaction(0, 2),
        WishboneTransaction(21 * wordInc, 1),
        WishboneTransaction(21 * wordInc, 1),
        WishboneTransaction(21 * wordInc, 42)
      )) {
        scoreboard.pushRef(ref)
      }

      new WishboneMonitor(dut.io.bus, dut.clockDomain).addResponseCallback(
        (bus, transaction, we) => {
          scoreboard.pushDut(transaction.copy(data = bus.DAT_MISO.toBigInt))
        }
      )

      // Make sure that all configurations exhibit proper latencies.
      val expectedResponseTime = if(regFeedback) 10 else 0
      val busStatus = WishboneStatus(dut.io.bus)
      val responseTimes = new ArrayBuffer[Long]

      var responseTime : Option[Long] = None
      dut.clockDomain.onSamplings({
        if(busStatus.isCycle) {
          // Start the clock as soon as we see a request
          if (busStatus.masterHasRequest && responseTime.isEmpty) {
            responseTime = Some(simTime())
          }

          var expectedResponseTimeForAddr = expectedResponseTime

          // Measure until ACK time.
          if(busStatus.isResponse) {
            val elapsedTime = simTime() - responseTime.get
            responseTimes += elapsedTime
            // For the pipeline case; if STB is assert it means we got a request now
            responseTime = if(conf.isPipelined && dut.io.bus.STB.toBoolean && regFeedback) Some(simTime()) else None
          }
        }
      })

      dri.drive(scala.collection.immutable.Seq(WishboneTransaction(10*wordInc), WishboneTransaction(0)), we = false)
      dri.drive(scala.collection.immutable.Seq(WishboneTransaction(10*wordInc, 200), WishboneTransaction(0, 2)), we = true)
      dri.drive(scala.collection.immutable.Seq(WishboneTransaction(10*wordInc), WishboneTransaction(0)), we = false)
      dri.drive(scala.collection.immutable.Seq(WishboneTransaction(21*wordInc, 0)), we = false)
      dri.drive(scala.collection.immutable.Seq(WishboneTransaction(21*wordInc, 42)), we = true)
      dri.drive(scala.collection.immutable.Seq(WishboneTransaction(21*wordInc)), we = false)

      scoreboard.checkEmptyness()

      // We check min since memory read times are slower
      assert(responseTimes.min == expectedResponseTime)

    }
  }
  for(regFeedback <- Seq(true, false);
      pipelined <- Seq(true, false);
      granularity <- Seq(AddressGranularity.BYTE, AddressGranularity.WORD, AddressGranularity.UNSPECIFIED);
      dataWidth <- Seq(32)) {
    val description = (if (pipelined) "pipelined" else "classic") + "_" + dataWidth + "_" + granularity.toString + "_" + (if (regFeedback) "reg" else "noreg")
    test(description){
      val conf = WishboneConfig(32,dataWidth, useSTALL = pipelined, addressGranularity = granularity)
      testBus(conf,description=description, regFeedback = regFeedback)
    }
  }

}
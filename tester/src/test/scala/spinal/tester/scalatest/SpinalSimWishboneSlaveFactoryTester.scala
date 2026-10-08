package spinal.tester.scalatest

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.wishbone._
import spinal.lib.wishbone.sim._
import spinal.lib.sim._

import scala.language.postfixOps

class WishboneSlaveWithCustomError(config: WishboneConfig) extends Component {
  val io = new Bundle {
    val bus  = slave(Wishbone(config))
    val busy = in Bool()
  }
  val busCtrl = WishboneSlaveFactory(io.bus)
  val reg0 = busCtrl.createReadAndWrite(Bits(config.dataWidth bits), 0)
  reg0.init(0)
  busCtrl.setError(io.busy)
}

class WishboneSimpleSlave(config : WishboneConfig, reg_feedback: Boolean = true, errorOnUnmapped: Boolean = true) extends Component{
  val io = new Bundle{
    val bus = slave(Wishbone(config))
  }
  val busCtrl = WishboneSlaveFactory(io.bus, reg_feedback=reg_feedback, errorOnUnmapped=errorOnUnmapped)
  val regA = busCtrl.createReadOnly(Bits(busCtrl.busDataWidth bits), 10 * config.dataWidth / 8)
  regA := 100
  val regB = busCtrl.createReadAndWrite(Bits(busCtrl.busDataWidth bits), 0)
  regB.init(1)
}

class SpinalSimWishboneSlaveFactoryTester extends SpinalAnyFunSuite{
  def testBus(conf:WishboneConfig, description : String = ""): Unit = {
    val fixture = SimConfig.allOptimisation.compile(rtl = new WishboneSimpleSlave(conf))
    fixture.doSim(description){ dut =>
      dut.clockDomain.forkStimulus(period=10)
      SimTimeout(1000*20*100)

      dut.io.bus.CYC #= false
      dut.io.bus.STB #= false
      dut.io.bus.WE #= false
      dut.io.bus.ADR #= 0
      dut.io.bus.DAT_MOSI #= 0
      if(dut.io.bus.config.useLOCK) dut.io.bus.LOCK #= false
      if(dut.io.bus.config.isPipelined) dut.io.bus.STALL #= false

      dut.clockDomain.waitSampling()

      val wordInc = conf.wordAddressInc(AddressGranularity.WORD)

      val scoreboard = ScoreboardInOrder[WishboneTransaction]()
      for(ref <- List(
        WishboneTransaction(10 * wordInc, 100), WishboneTransaction(0, 1),
        WishboneTransaction(10 * wordInc, 100), WishboneTransaction(0, 1),
        WishboneTransaction(10 * wordInc, 100), WishboneTransaction(0, 2)
      )) {
        scoreboard.pushRef(ref)
      }

      WishboneMonitor(dut.io.bus, dut.clockDomain){ bus =>
           scoreboard.pushDut(WishboneTransaction.sampleAsMaster(bus))
      }

      val dri = new WishboneDriver(dut.io.bus, dut.clockDomain)
      dri.drive(scala.collection.immutable.Seq(WishboneTransaction(10*wordInc), WishboneTransaction(0)), we = false)
      dri.drive(scala.collection.immutable.Seq(WishboneTransaction(10*wordInc, 200), WishboneTransaction(0, 2)), we = true)
      dri.drive(scala.collection.immutable.Seq(WishboneTransaction(10*wordInc), WishboneTransaction(0)), we = false)

      scoreboard.checkEmptiness()
//
//      for(repeat <- 0 until 100){
//        driver.drive(for(x <- 1 to 10) yield WishboneTransaction(address=10).randomizeData(dut.io.bus.config.dataWidth), we = true)
//        dut.clockDomain.waitSampling(10)
//        driver.drive(for(x <- 1 to 10) yield WishboneTransaction(address=10), we = false)
//        dut.clockDomain.waitSampling(10)
//      }
//

    }
  }

  test("classic"){
    val conf = WishboneConfig(8,8)
    testBus(conf,description="classic")
  }

  test("classic32"){
    val conf = WishboneConfig(32, 32)
    testBus(conf, description="classic32")
  }

  test("classic32_byte"){
    val conf = WishboneConfig(32, 32, addressGranularity = AddressGranularity.BYTE)
    testBus(conf, description="classic32_byte")
  }

  test("classic32_word"){
    val conf = WishboneConfig(32, 32, addressGranularity = AddressGranularity.WORD)
    testBus(conf, description="classic32_word")
  }

  //  test("pipelined"){
//    val conf = WishboneConfig(8,8).pipelined
//    testBus(conf,description="pipelined")
//  }

  def initBus(dut: WishboneSimpleSlave): Unit = {
    dut.io.bus.CYC      #= false
    dut.io.bus.STB      #= false
    dut.io.bus.WE       #= false
    dut.io.bus.ADR      #= 0
    dut.io.bus.DAT_MOSI #= 0
    if(dut.io.bus.config.useLOCK)     dut.io.bus.LOCK  #= false
    if(dut.io.bus.config.isPipelined) dut.io.bus.STALL #= false
  }

  test("err_on_unmapped_address") {
    val conf = WishboneConfig(32, 32, useERR = true)
    val fixture = SimConfig.allOptimisation.compile(rtl = new WishboneSimpleSlave(conf))
    fixture.doSim("err_on_unmapped_address") { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      SimTimeout(1000 * 20 * 100)

      initBus(dut)
      dut.clockDomain.waitSampling()

      // Access a mapped address: ACK expected, ERR must not fire
      dut.io.bus.CYC #= true
      dut.io.bus.STB #= true
      dut.io.bus.WE  #= false
      dut.io.bus.ADR #= 0  // regB lives at byte address 0, word address 0
      dut.clockDomain.waitSamplingWhere(dut.io.bus.ACK.toBoolean || dut.io.bus.ERR.toBoolean)
      assert(!dut.io.bus.ERR.toBoolean, "ERR should not be asserted for a mapped address")
      assert( dut.io.bus.ACK.toBoolean, "ACK should be asserted for a mapped address")
      dut.io.bus.STB #= false
      dut.io.bus.CYC #= false
      dut.clockDomain.waitSampling()

      // Access an unmapped address: ERR expected, ACK must not fire
      dut.io.bus.CYC #= true
      dut.io.bus.STB #= true
      dut.io.bus.WE  #= false
      dut.io.bus.ADR #= 5  // no register mapped at this address
      dut.clockDomain.waitSamplingWhere(dut.io.bus.ACK.toBoolean || dut.io.bus.ERR.toBoolean)
      assert( dut.io.bus.ERR.toBoolean, "ERR should be asserted for an unmapped address")
      assert(!dut.io.bus.ACK.toBoolean, "ACK must not be asserted when ERR fires")
      dut.io.bus.STB #= false
      dut.io.bus.CYC #= false
      dut.clockDomain.waitSampling()
    }
  }

  test("classic_ack_requires_cyc") {
    val conf = WishboneConfig(32, 32, useERR = true)
    val fixture = SimConfig.allOptimisation.compile(rtl = new WishboneSimpleSlave(conf))
    fixture.doSim("classic_ack_requires_cyc") { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      SimTimeout(1000 * 20 * 100)

      initBus(dut)
      dut.clockDomain.waitSampling()

      // On a shared bus STB may be seen while this slave is not selected (CYC low): no response allowed
      dut.io.bus.STB #= true
      dut.io.bus.ADR #= 0
      for(_ <- 0 until 4) {
        dut.clockDomain.waitSampling()
        assert(!dut.io.bus.ACK.toBoolean, "ACK must not be asserted while CYC is low")
        assert(!dut.io.bus.ERR.toBoolean, "ERR must not be asserted while CYC is low")
      }

      // Regular read of regB: one wait state, then ACK
      dut.io.bus.CYC #= true
      dut.clockDomain.waitSampling()
      assert(!dut.io.bus.ACK.toBoolean, "ACK expected after one wait state")
      dut.clockDomain.waitSampling()
      assert(dut.io.bus.ACK.toBoolean, "ACK expected after one wait state")
      assert(dut.io.bus.DAT_MISO.toLong == 1)

      // The next transfer on the shared bus targets another slave: STB stays high, CYC goes low here
      dut.io.bus.CYC #= false
      dut.io.bus.ADR #= 10
      for(_ <- 0 until 2) {
        dut.clockDomain.waitSampling()
        assert(!dut.io.bus.ACK.toBoolean, "ACK must not be asserted while CYC is low")
        assert(!dut.io.bus.ERR.toBoolean, "ERR must not be asserted while CYC is low")
      }
      dut.io.bus.STB #= false
      dut.clockDomain.waitSampling()
    }
  }

  test("classic_back_to_back") {
    val conf = WishboneConfig(32, 32, useERR = true)
    val fixture = SimConfig.allOptimisation.compile(rtl = new WishboneSimpleSlave(conf))
    fixture.doSim("classic_back_to_back") { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      SimTimeout(1000 * 20 * 100)

      initBus(dut)
      dut.clockDomain.waitSampling()

      // Keep CYC and STB asserted across transfers; each one must get its own response, after one wait state
      def transfer(address: Int, expectErr: Boolean): Unit = {
        dut.io.bus.ADR #= address
        dut.clockDomain.waitSampling()
        assert(!dut.io.bus.ACK.toBoolean, s"ACK expected after one wait state (address $address)")
        assert(!dut.io.bus.ERR.toBoolean, s"ERR expected after one wait state (address $address)")
        dut.clockDomain.waitSampling()
        assert(dut.io.bus.ERR.toBoolean == expectErr, s"Wrong ERR for address $address")
        assert(dut.io.bus.ACK.toBoolean == !expectErr, s"Wrong ACK for address $address")
      }

      dut.io.bus.CYC #= true
      dut.io.bus.STB #= true
      dut.io.bus.WE  #= false
      transfer(5, expectErr = true)       // unmapped
      transfer(0, expectErr = false)      // regB
      assert(dut.io.bus.DAT_MISO.toLong == 1)
      transfer(5, expectErr = true)       // unmapped
      transfer(10, expectErr = false)     // regA
      assert(dut.io.bus.DAT_MISO.toLong == 100)
      dut.io.bus.STB #= false
      dut.io.bus.CYC #= false
      dut.clockDomain.waitSampling()
    }
  }

  test("pipelined_ack_requires_cyc") {
    val conf = WishboneConfig(32, 32, useERR = true).pipelined
    val fixture = SimConfig.allOptimisation.compile(rtl = new WishboneSimpleSlave(conf))
    fixture.doSim("pipelined_ack_requires_cyc") { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      SimTimeout(1000 * 20 * 100)

      initBus(dut)
      dut.clockDomain.waitSampling()

      // STB without CYC is not a request: no ACK/ERR in the following cycles
      for(address <- List(0, 5)) {
        dut.io.bus.STB #= true
        dut.io.bus.ADR #= address
        dut.clockDomain.waitSampling()
        dut.io.bus.STB #= false
        for(_ <- 0 until 2) {
          dut.clockDomain.waitSampling()
          assert(!dut.io.bus.ACK.toBoolean, "ACK must not answer a STB seen while CYC is low")
          assert(!dut.io.bus.ERR.toBoolean, "ERR must not answer a STB seen while CYC is low")
        }
      }

      // Accepted requests are answered in the next cycle
      dut.io.bus.CYC #= true
      for((address, expectErr) <- List((0, false), (5, true))) {
        dut.io.bus.STB #= true
        dut.io.bus.ADR #= address
        dut.clockDomain.waitSampling()
        dut.io.bus.STB #= false
        dut.clockDomain.waitSampling()
        assert(dut.io.bus.ERR.toBoolean == expectErr, s"Wrong ERR for address $address")
        assert(dut.io.bus.ACK.toBoolean == !expectErr, s"Wrong ACK for address $address")
      }
      dut.io.bus.CYC #= false
      dut.clockDomain.waitSampling()
    }
  }

  test("setError") {
    val conf = WishboneConfig(32, 32, useERR = true)
    val fixture = SimConfig.allOptimisation.compile(rtl = new WishboneSlaveWithCustomError(conf))
    fixture.doSim("setError") { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      SimTimeout(1000 * 20 * 100)

      dut.io.bus.CYC      #= false
      dut.io.bus.STB      #= false
      dut.io.bus.WE       #= false
      dut.io.bus.ADR      #= 0
      dut.io.bus.DAT_MOSI #= 0
      dut.io.busy         #= false
      if(dut.io.bus.config.useLOCK)     dut.io.bus.LOCK  #= false
      if(dut.io.bus.config.isPipelined) dut.io.bus.STALL #= false
      dut.clockDomain.waitSampling()

      // Write while not busy: ACK expected, no ERR
      dut.io.bus.CYC      #= true
      dut.io.bus.STB      #= true
      dut.io.bus.WE       #= true
      dut.io.bus.ADR      #= 0
      dut.io.bus.DAT_MOSI #= 0xFFFFFFFFL
      dut.io.busy         #= false
      dut.clockDomain.waitSamplingWhere(dut.io.bus.ACK.toBoolean || dut.io.bus.ERR.toBoolean)
      assert(!dut.io.bus.ERR.toBoolean, "ERR should not fire when not busy")
      assert( dut.io.bus.ACK.toBoolean, "ACK should be asserted when not busy")
      dut.io.bus.STB #= false
      dut.io.bus.CYC #= false
      dut.clockDomain.waitSampling()

      // Write while busy: ERR expected, no ACK
      dut.io.bus.CYC      #= true
      dut.io.bus.STB      #= true
      dut.io.bus.WE       #= true
      dut.io.bus.ADR      #= 0
      dut.io.bus.DAT_MOSI #= 0xFFFFFFFFL
      dut.io.busy         #= true
      dut.clockDomain.waitSamplingWhere(dut.io.bus.ACK.toBoolean || dut.io.bus.ERR.toBoolean)
      assert( dut.io.bus.ERR.toBoolean, "ERR should fire when busy")
      assert(!dut.io.bus.ACK.toBoolean, "ACK should not be asserted when busy")
      dut.io.bus.STB #= false
      dut.io.bus.CYC #= false
      dut.clockDomain.waitSampling()
    }
  }

  test("no_err_on_unmapped_when_disabled") {
    val conf = WishboneConfig(32, 32, useERR = true)
    val fixture = SimConfig.allOptimisation.compile(rtl = new WishboneSimpleSlave(conf, errorOnUnmapped=false))
    fixture.doSim("no_err_on_unmapped_when_disabled") { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      SimTimeout(1000 * 20 * 100)

      initBus(dut)
      dut.clockDomain.waitSampling()

      // Access an unmapped address with errorOnUnmapped disabled: ACK expected, no ERR
      dut.io.bus.CYC #= true
      dut.io.bus.STB #= true
      dut.io.bus.WE  #= false
      dut.io.bus.ADR #= 5  // no register mapped here
      dut.clockDomain.waitSamplingWhere(dut.io.bus.ACK.toBoolean || dut.io.bus.ERR.toBoolean)
      assert(!dut.io.bus.ERR.toBoolean, "ERR should not fire for unmapped address when errorOnUnmapped is disabled")
      assert( dut.io.bus.ACK.toBoolean, "ACK should be asserted for unmapped address when errorOnUnmapped is disabled")
      dut.io.bus.STB #= false
      dut.io.bus.CYC #= false
      dut.clockDomain.waitSampling()
    }
  }
}

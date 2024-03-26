package spinal.tester.scalatest

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif.{AccessType, WishboneBusInterface}
import spinal.lib.bus.wishbone._
import spinal.lib.sim._
import spinal.lib.wishbone.sim._

import scala.language.postfixOps
import scala.util.Random

class WishboneSimpleBusInterface( config : WishboneConfig,
                                  readSync: Boolean = true) extends Component{
  val io = new Bundle{
    val bus = slave(Wishbone(config))
  }

  val busIf = WishboneBusInterface(io.bus, SizeMapping(0, 32), readSync = readSync)
  val id = busIf.newReg("Register1").field(UInt(config.dataWidth bits), AccessType.RO, "ID of registers")
  val value = busIf.newReg("Register2").field(UInt(config.dataWidth bits), AccessType.RW, 1, "RW value")
  id := 0x78

  if(!readSync) {
    assert(io.bus.isRead === (!io.bus.WE && io.bus.ACK), "Bus interface is configured for async operation; ACK should be wired to CYC && STB && !WE")
  }
}

class SpinalSimWishboneBusInterfaceTester extends SpinalAnyFunSuite{
  def testBus(conf:WishboneConfig, readSync: Boolean = true, description : String = ""): Unit = {
    val fixture = SimConfig.allOptimisation.compile(rtl = new WishboneSimpleBusInterface(conf, readSync).setDefinitionName("WishboneSimpleBusInterface_" + description))
    fixture.doSim(description){ dut =>
      dut.clockDomain.forkStimulus(period=10)
      SimTimeout(1000*20*100)

      dut.io.bus.CYC #= false
      dut.io.bus.STB #= false
      dut.io.bus.WE #= false
      dut.io.bus.ADR #= 0
      dut.io.bus.DAT_MOSI #= 0
      if(dut.io.bus.config.useLOCK) dut.io.bus.LOCK #= false

      dut.clockDomain.waitSampling()

      val wordInc = conf.wordAddressInc(AddressGranularity.BYTE)

      val sco = ScoreboardInOrder[WishboneTransaction]()
      for(ref_output <- List(
        WishboneTransaction(0, 0x78L), WishboneTransaction(1 * wordInc, 1),
        WishboneTransaction(0, 0), WishboneTransaction(1 * wordInc, 0),
        WishboneTransaction(0, 0x78L), WishboneTransaction(1 * wordInc, 2)
      )) {
        sco.pushRef(ref_output)
      }

      new WishboneMonitor(dut.io.bus, dut.clockDomain).addResponseCallback(
        (bus, transaction, we) => {
          sco.pushDut(transaction.copy(data = bus.DAT_MISO.toBigInt))
        }
      )

      val dri = new WishboneDriver(dut.io.bus, dut.clockDomain)
      dri.drive(scala.collection.immutable.Seq(WishboneTransaction(0), WishboneTransaction(1 * wordInc)), we = false)
      dri.drive(scala.collection.immutable.Seq(WishboneTransaction(0, 100), WishboneTransaction(1 * wordInc, 2)), we = true)
      dri.drive(scala.collection.immutable.Seq(WishboneTransaction(0), WishboneTransaction(1 * wordInc)), we = false)

      sco.checkEmptyness()
    }
  }

//
//  test("sync"){
//    val conf = WishboneConfig(32,32)
//    testBus(conf, description="sync")
//  }
//
//  test("sync_byte"){
//    val conf = WishboneConfig(32,32, addressGranularity = AddressGranularity.BYTE)
//    testBus(conf, description="sync_byte")
//  }
//
//  test("sync_word"){
//    val conf = WishboneConfig(32,32, addressGranularity = AddressGranularity.WORD)
//    testBus(conf, description="sync_word")
//  }
//
//  test("async"){
//    val conf = WishboneConfig(32,32)
//    testBus(conf,readSync = false, description="async")
//  }
//

  for(regFeedback <- Seq(true, false)) {
    for(pipelined <- Seq(true, false)) {
      for(granularity <- Seq(AddressGranularity.BYTE, AddressGranularity.WORD, AddressGranularity.UNSPECIFIED)) {
        for(dataWith <- Seq(8, 16, 32)) {
          val description = (if (pipelined) "pipelined" else "classic") + "_" + dataWith + "_" + granularity.toString + "_" + (if (regFeedback) "reg" else "noreg")
          test(description){
            val conf = WishboneConfig(32,dataWith, useSTALL = pipelined, addressGranularity = granularity)
            testBus(conf,description=description, readSync = regFeedback)
          }
        }
      }
    }
  }
}
package spinal.tester.scalatest


import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._


object Axi4CrossbarTester{
  class Axi4CrossbarTester extends Component {
    val axiMasterConfig = Axi4Config(addressWidth = 13,dataWidth = 32,idWidth = 4)
    val axiSlaveConfig  = Axi4Config(addressWidth = 13,dataWidth = 32,idWidth = 6)

    val axiMasters = Vec(slave(Axi4(axiMasterConfig)),3)
    val axiSlaves  = Vec(master(Axi4(axiSlaveConfig)),4)

    val crossbarFactory = Axi4CrossbarFactory()
      .addSlaves(
        axiSlaves(0) -> (0x000,0x400),
        axiSlaves(1) -> (0x400,0x400),
        axiSlaves(2) -> (0x800,0x400),
        axiSlaves(3) -> (0xC00,0x400)
      )
      .addConnections(
        axiMasters(0) -> List(axiSlaves(1),axiSlaves(2),axiSlaves(3)),
        axiMasters(1) -> List(axiSlaves(0),axiSlaves(2),axiSlaves(3)),
        axiMasters(2) -> List(axiSlaves(0),axiSlaves(1),axiSlaves(3))
      )

    val readCrossbar = crossbarFactory.build()
  }
}

class Axi4CrossbarTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "Axi4CrossbarTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/Axi4CrossbarTester"
  override def createToplevel: Component = new Axi4CrossbarTester.Axi4CrossbarTester
}
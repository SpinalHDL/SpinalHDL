package spinal.tester.scalatest

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahb.{Ahb3InterconnectFactory, Ahb3Slave, Ahb3Master, Ahb3Config}


object Ahb3InterconnectTester{
  class Ahb3InterconnectTester extends Component {
    val ahbConfig = Ahb3Config(addressWidth = 12,dataWidth = 32)

    val ahbMasters = Vec(slave(Ahb3Master(ahbConfig)),3)
    val ahbSlaves  = Vec(master(Ahb3Slave(ahbConfig)),4)

    val interconnect = Ahb3InterconnectFactory(ahbConfig)
      .addSlaves(
        ahbSlaves(0) -> (0x000,0x400),
        ahbSlaves(1) -> (0x400,0x400),
        ahbSlaves(2) -> (0x800,0x400),
        ahbSlaves(3) -> (0xC00,0x400)
      )
      .addConnections(
        ahbMasters(0) -> List(ahbSlaves(1),ahbSlaves(2),ahbSlaves(3)),
        ahbMasters(1) -> List(ahbSlaves(0),ahbSlaves(2),ahbSlaves(3)),
        ahbMasters(2) -> List(ahbSlaves(0),ahbSlaves(1),ahbSlaves(3))
      )
      .build()
////
//    val ahbConfig = Ahb3Config(addressWidth = 16,dataWidth = 32)
//
//    val ahbMasters = Vec(slave(Ahb3Master(ahbConfig)),3)
//    val ahbSlaves  = Vec(master(Ahb3Slave(ahbConfig)),1)
//
//    val interconnect = Ahb3InterconnectFactory(ahbConfig)
//      .addSlaves(
//        ahbSlaves(0) -> (0x000,0x400)
//      )
//      .addConnections(
//        ahbMasters(0) -> List(ahbSlaves(0)),
//        ahbMasters(1) -> List(ahbSlaves(0)),
//        ahbMasters(2) -> List(ahbSlaves(0))
//      )
//      .build()

//    val ahbConfig = Ahb3Config(addressWidth = 12,dataWidth = 32)
//
//    val ahbMasters = Vec(slave(Ahb3Master(ahbConfig)),1)
//    val ahbSlaves  = Vec(master(Ahb3Slave(ahbConfig)),4)
//
//    val interconnect = Ahb3InterconnectFactory(ahbConfig)
//      .addSlaves(
//        ahbSlaves(0) -> (0x000,0x400),
//        ahbSlaves(1) -> (0x400,0x400),
//        ahbSlaves(2) -> (0x800,0x400),
//        ahbSlaves(3) -> (0xC00,0x400)
//      )
//      .addConnections(
//        ahbMasters(0) -> List(ahbSlaves(0),ahbSlaves(1),ahbSlaves(2),ahbSlaves(3))
//      )
//      .build()
  }
}

class Ahb3InterconnectTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "Ahb3InterconnectTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/Ahb3InterconnectTester"
  override def createToplevel: Component = new Ahb3InterconnectTester.Ahb3InterconnectTester

  override def backendConfig(config: SpinalConfig): SpinalConfig = config.dumpWave()
}
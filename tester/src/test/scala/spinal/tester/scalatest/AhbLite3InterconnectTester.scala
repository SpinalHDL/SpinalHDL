package spinal.tester.scalatest

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite.{AhbLite3InterconnectFactory, AhbLite3, AhbLite3Master, AhbLite3Config}


object AhbLite3InterconnectTester{
  class AhbLite3InterconnectTester extends Component {
    val ahbConfig = AhbLite3Config(addressWidth = 12,dataWidth = 32)

    val ahbMasters = Vec(slave(AhbLite3Master(ahbConfig)),3)
    val ahbSlaves  = Vec(master(AhbLite3(ahbConfig)),4)

    val interconnect = AhbLite3InterconnectFactory(ahbConfig)
      .addSlaves(
        ahbSlaves(0) -> (0x000,0x400),
        ahbSlaves(1) -> (0x400,0x400),
        ahbSlaves(2) -> (0x800,0x400),
        ahbSlaves(3) -> (0xC00,0x400)
      )
      .addConnections(
        ahbMasters(0).toAhbLite3() -> List(ahbSlaves(1),ahbSlaves(2),ahbSlaves(3)),
        ahbMasters(1).toAhbLite3() -> List(ahbSlaves(0),ahbSlaves(2),ahbSlaves(3)),
        ahbMasters(2).toAhbLite3() -> List(ahbSlaves(0),ahbSlaves(1),ahbSlaves(3))
      )
      .build()
////
//    val ahbConfig = AhbLite3Config(addressWidth = 16,dataWidth = 32)
//
//    val ahbMasters = Vec(slave(AhbLite3Master(ahbConfig)),3)
//    val ahbSlaves  = Vec(master(AhbLite3Slave(ahbConfig)),1)
//
//    val interconnect = AhbLite3InterconnectFactory(ahbConfig)
//      .addSlaves(
//        ahbSlaves(0) -> (0x000,0x400)
//      )
//      .addConnections(
//        ahbMasters(0) -> List(ahbSlaves(0)),
//        ahbMasters(1) -> List(ahbSlaves(0)),
//        ahbMasters(2) -> List(ahbSlaves(0))
//      )
//      .build()

//    val ahbConfig = AhbLite3Config(addressWidth = 12,dataWidth = 32)
//
//    val ahbMasters = Vec(slave(AhbLite3Master(ahbConfig)),1)
//    val ahbSlaves  = Vec(master(AhbLite3Slave(ahbConfig)),4)
//
//    val interconnect = AhbLite3InterconnectFactory(ahbConfig)
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

class AhbLite3InterconnectTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "AhbLite3InterconnectTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/AhbLite3InterconnectTester"
  override def createToplevel: Component = new AhbLite3InterconnectTester.AhbLite3InterconnectTester
  override def backendConfig(config: SpinalConfig): SpinalConfig = config.dumpWave()
}
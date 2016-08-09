package spinal.tester.code

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._
import spinal.lib.bus.amba3.apb.Apb3Config

object PlayAhbLite3{
  class TopLevel extends Component{
    val ahbConfig = AhbLite3Config(addressWidth = 16,dataWidth = 32)
    val apbConfig = Apb3Config(addressWidth = 16,dataWidth = 32)

    val ahbMasters = Vec(slave(AhbLite3Master(ahbConfig)),3)
    val ahbSlaves  = Vec(master(AhbLite3Slave(ahbConfig)),4)

    val perMaster = for(ahbMaster <- ahbMasters) yield new Area{
      val decoder = AhbLite3Decoder(
        AhbLite3Config = ahbConfig,
        decodings = List(
          (0x1000,0x1000),
          (0x3000,0x1000),
          (0x4000,0x1000),
          (0x6000,0x1000)
        )
      )
      decoder.io.input <> ahbMaster
    }

    val perSlave = for((ahbSlave,idx) <- ahbSlaves.zipWithIndex) yield new Area{
      val arbiter = AhbLite3Arbiter(
        AhbLite3Config = ahbConfig,
        inputsCount = ahbMasters.length
      )
      (arbiter.io.inputs,perMaster).zipped.foreach(_ <> _.decoder.io.outputs(idx))
      arbiter.io.output <> ahbSlave
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}


object PlayAhbLite3_2{
  class TopLevel extends Component{
    val ahbConfig = AhbLite3Config(addressWidth = 16,dataWidth = 32)
//    val apbConfig = Apb3Config(addressWidth = 16,dataWidth = 32)

    val ahbMasters = Vec(slave(AhbLite3Master(ahbConfig)),3)
    val ahbSlaves  = Vec(master(AhbLite3Slave(ahbConfig)),4)

    val interconnect = AhbLite3InterconnectFactory(ahbConfig)
      .addSlaves(
        ahbSlaves(0) -> (0x1000,0x1000),
        ahbSlaves(1) -> (0x3000,0x1000),
        ahbSlaves(2) -> (0x4000,0x1000),
        ahbSlaves(3) -> (0x5000,0x1000)
      )
      .addConnections(
        ahbMasters(0) -> List(ahbSlaves(0),ahbSlaves(1)),
        ahbMasters(1) -> List(ahbSlaves(1),ahbSlaves(2),ahbSlaves(3)),
        ahbMasters(2) -> List(ahbSlaves(0),ahbSlaves(3))
      )
      .build()
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel).printPruned()
  }
}
package spinal.tester.code

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahb._
import spinal.lib.bus.amba3.apb.Apb3Config
import spinal.lib.bus.misc.BaseSize

object PlayAhb3{
  class TopLevel extends Component{
    val ahbConfig = Ahb3Config(addressWidth = 16,dataWidth = 32)
    val apbConfig = Apb3Config(addressWidth = 16,dataWidth = 32)

    val ahbMasters = Vec(slave(Ahb3Master(ahbConfig)),3)
    val ahbSlaves  = Vec(master(Ahb3Slave(ahbConfig)),4)

    val perMaster = for(ahbMaster <- ahbMasters) yield new Area{
      val decoder = Ahb3Decoder(
        ahb3Config = ahbConfig,
        decodings = List(
          BaseSize(0x1000,0x1000),
          BaseSize(0x3000,0x1000),
          BaseSize(0x4000,0x1000),
          BaseSize(0x6000,0x1000)
        )
      )
      decoder.io.input <> ahbMaster
    }

    val perSlave = for((ahbSlave,idx) <- ahbSlaves.zipWithIndex) yield new Area{
      val arbiter = Ahb3Arbiter(
        ahb3Config = ahbConfig,
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
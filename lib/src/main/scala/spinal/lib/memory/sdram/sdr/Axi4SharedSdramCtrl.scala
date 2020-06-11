package spinal.lib.memory.sdram.sdr

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.memory.sdram.SdramLayout

/**
 * Created by PIC32F_USER on 28/08/2016.
 */

object Axi4SharedSdramCtrl{
  def getAxiConfig(dataWidth : Int,idWidth : Int,layout : SdramLayout) : Axi4Config = {
    val widthFactor = dataWidth/layout.dataWidth
    Axi4Config(
      addressWidth = layout.byteAddressWidth,
      dataWidth = dataWidth,
      idWidth = idWidth,
      useLock = false,
      useRegion = false,
      useCache = false,
      useProt = false,
      useQos = false
    )
  }
}

case class Axi4SharedSdramCtrl(axiDataWidth : Int, axiIdWidth : Int, layout : SdramLayout, timing : SdramTimings, CAS : Int) extends Component{
  val dataWidthFactor = axiDataWidth/layout.dataWidth
  require(dataWidthFactor != 0)
  require(isPow2(dataWidthFactor))
  val axiConfig = Axi4SharedSdramCtrl.getAxiConfig(axiDataWidth,axiIdWidth,layout)

  val io = new Bundle{
    val axi   = slave(Axi4Shared(axiConfig))
    val sdram = master(SdramInterface(layout))
  }

  val ctrl = SdramCtrl(layout,timing,CAS,SdramCtrlAxi4SharedContext(axiConfig.idWidth))
  val ctrlBusAdapted = dataWidthFactor match {
    case 1 => ctrl.io.bus
    case _ => ctrl.io.bus.genScaledUpDriver(dataWidthFactor)
  }
  val bridge = ctrlBusAdapted.driveFrom(io.axi)

  io.sdram <> ctrl.io.sdram
}

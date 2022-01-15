package spinal.lib.com.usb.ohci

import spinal.core._
import spinal.lib.bus.bmb.{BmbParameter, BmbToWishbone}
import spinal.lib.bus.wishbone.{Wishbone, WishboneConfig, WishboneToBmb}
import spinal.lib.com.usb.phy.UsbHubLsFs.CtrlCc
import spinal.lib.com.usb.phy.{UsbLsFsPhy, UsbPhyFsNativeIo}
import spinal.lib._

object UsbOhciWishbone extends App{
  var netlistDirectory = "."
  var netlistName = "UsbOhciWishbone"
  var portCount = 1
  var phyFrequency = 48000000
  var dmaWidth = 32

  assert(new scopt.OptionParser[Unit]("VexRiscvLitexSmpClusterCmdGen") {
    help("help").text("prints this usage text")
    opt[String]("netlist-directory") action { (v, c) => netlistDirectory = v }
    opt[String]("netlist-name") action { (v, c) => netlistName = v }
    opt[Int]("port-count") action { (v, c) => portCount = v }
    opt[Int]("phy-frequency") action { (v, c) => phyFrequency = v }
    opt[Int]("dma-width") action { (v, c) => dmaWidth = v }
  }.parse(args))



  val p = UsbOhciParameter(
    noPowerSwitching = false,
    powerSwitchingMode = false,
    noOverCurrentProtection = false,
    powerOnToPowerGoodTime = 10,
    dataWidth = dmaWidth,
    portsConfig = List.fill(portCount)(OhciPortParameter())
  )

  SpinalConfig(
    globalPrefix = s"${netlistName}_",
    targetDirectory = netlistDirectory
  ).generateVerilog(
    UsbOhciWishbone(
      p,
      ClockDomain.external("ctrl"),
      ClockDomain.external("phy", frequency = FixedFrequency(phyFrequency Hz))
    ).setDefinitionName(netlistName)
  )
}

case class UsbOhciWishbone(p : UsbOhciParameter, frontCd : ClockDomain, backCd : ClockDomain) extends Component {
  val ctrlParameter = WishboneConfig(
    addressWidth = 10,
    dataWidth = 32,
    selWidth = 4
  )

  val dmaParameter = BmbToWishbone.getWishboneConfig(UsbOhci.dmaParameter(p).access)

  val io = new Bundle {
    val dma = master(Wishbone(dmaParameter))
    val ctrl = slave(Wishbone(ctrlParameter))
    val interrupt = out Bool()
    val usb = Vec(master(UsbPhyFsNativeIo()), p.portCount)
  }

  val front = frontCd on new Area {
    val dmaBridge = BmbToWishbone(UsbOhci.dmaParameter(p))
    dmaBridge.io.output <> io.dma

    val ctrlBridge = WishboneToBmb(ctrlParameter)
    ctrlBridge.io.input <> io.ctrl

    val ohci = UsbOhci(p, BmbParameter(
      addressWidth = 12,
      dataWidth = 32,
      sourceWidth = 0,
      contextWidth = 0,
      lengthWidth = 2
    ))
    ohci.io.dma <> dmaBridge.io.input
    ohci.io.ctrl <> ctrlBridge.io.output
    ohci.io.interrupt <> io.interrupt
  }

  val back = backCd on new Area {
    val phy = UsbLsFsPhy(p.portCount)
    //  phy.io.usb  <> io.usb
    phy.io.management.map(e => e.overcurrent := False)
    val native = phy.io.usb.map(_.toNativeIo())
    val buffer = native.map(_.stage())
    io.usb <> Vec(buffer.map(e => e.stage()))
  }

  val cc = CtrlCc(p.portCount, frontCd, backCd)
  cc.input <> front.ohci.io.phy
  cc.output <> back.phy.io.ctrl
}

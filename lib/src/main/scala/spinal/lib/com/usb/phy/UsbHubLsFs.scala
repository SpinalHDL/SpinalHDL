package spinal.lib.com.usb.phy

import spinal.core._
import spinal.lib._


object UsbHubLsFs{
  object RxKind extends SpinalEnum{
    val NONE, RESUME, PACKET = newElement()
  }

  case class CtrlPort() extends Bundle with IMasterSlave {
    val disable = Event
    val removable = Bool()
    val power = Bool()
    val reset = Event
    val suspend = Event
    val resume = Event

    val connect, disconnect = Bool()
    val overcurrent = Bool()
    val remoteResume = Bool()
    val lowSpeed = Bool()


    override def asMaster(): Unit = {
      out(removable, power)
      master(reset, suspend, resume, disable)
      in(connect, disconnect, overcurrent, lowSpeed, remoteResume)
    }
  }

  case class CtrlRxPayload() extends Bundle{
    val stuffingError = Bool()
    val data = Bits(8 bits)
  }
  case class CtrlRx() extends Bundle with IMasterSlave {
    val flow = Flow(CtrlRxPayload())
    val active = Bool()

    override def asMaster(): Unit = out(this)
  }

  case class Ctrl(portCount : Int) extends Bundle with IMasterSlave{
    val lowSpeed = Bool()
    val tx = Stream(Fragment(Bits(8 bits)))
    val txEop = Bool()
    val rx = CtrlRx()

    val usbReset = Bool()
    val usbResume = Bool()
    val overcurrent = Bool()
    val tick = Bool()

    val ports = Vec(CtrlPort(), portCount)

    override def asMaster(): Unit = {
      in(overcurrent, tick)
      out(lowSpeed, usbReset, usbResume)
      master(tx)
      in(txEop)
      slave(rx)
      ports.foreach(master(_))
    }


    def cc(cdFrom : ClockDomain, cdTo : ClockDomain) : Ctrl = {
      val c = CtrlCc(portCount, cdFrom, cdTo).setCompositeName(this, "cc")
      c.input <> this
      c.output
    }
  }

  case class CtrlCc(portCount : Int, cdInput : ClockDomain, cdOutput : ClockDomain) extends Component {
    val input = slave(Ctrl(portCount))
    val output = master(Ctrl(portCount))

    output.lowSpeed := cdOutput(BufferCC(input.lowSpeed))
    output.usbReset := cdOutput(BufferCC(input.usbReset))
    output.usbResume := cdOutput(BufferCC(input.usbResume))
    input.overcurrent := cdInput(BufferCC(output.overcurrent))

    output.tx << cdOutput(input.tx.ccToggle(cdInput, cdOutput).stage())
    input.txEop := PulseCCByToggle(output.txEop, cdOutput, cdInput)

    input.rx.flow << output.rx.flow.ccToggle(cdOutput, cdInput)
    input.rx.active := cdInput(BufferCC(output.rx.active))
    input.tick := PulseCCByToggle(output.tick, cdOutput, cdInput)

    for((pi, po) <- (input.ports, output.ports).zipped){
      po.removable := cdOutput(BufferCC(pi.removable))
      po.power := cdOutput(BufferCC(pi.power))
      pi.lowSpeed := cdInput(BufferCC(po.lowSpeed))
      pi.overcurrent := cdInput(BufferCC(po.overcurrent))

      pi.connect := PulseCCByToggle(po.connect, cdOutput, cdInput)
      pi.disconnect := PulseCCByToggle(po.disconnect, cdOutput, cdInput)
      pi.remoteResume := PulseCCByToggle(po.remoteResume, cdOutput, cdInput)

      po.reset << pi.reset.ccToggleWithoutBuffer(cdInput, cdOutput)
      po.suspend << pi.suspend.ccToggleWithoutBuffer(cdInput, cdOutput)
      po.resume << pi.resume.ccToggleWithoutBuffer(cdInput, cdOutput)
      po.disable << pi.disable.ccToggleWithoutBuffer(cdInput, cdOutput)
    }
  }
}



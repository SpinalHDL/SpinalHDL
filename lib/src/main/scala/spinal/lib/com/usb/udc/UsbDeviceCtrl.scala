package spinal.lib.com.usb.udc

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.com.eth.{Crc, CrcKind}
import spinal.lib.com.usb._
import spinal.lib.com.usb.ohci.UsbPid
import spinal.lib.fsm._

case class UsbDeviceCtrlParameter(addressWidth : Int,
                                  epCount : Int){
  def lengthWidth = (addressWidth + 1) max 16
}
/*
Add descriptor =>
1) set head or add to last descriptor
2) check that we didn't had race condition during the update

remove descriptor =>
1) halt endpoint
2) clear head or unlink descriptor
3) unhalt

On interrupt =>
1) identify which ED had the interrupt
2) process its linked list from its head
 */

object UsbDeviceCtrl {
  case class Rx() extends Bundle with IMasterSlave {
    val flow = Flow(Bits(8 bits))
    val active = Bool()
    val stuffingError = Bool()

    override def asMaster(): Unit = out(this)
  }

  case class Tx() extends Bundle with IMasterSlave {
    val stream = Stream(Fragment(Bits(8 bits)))
    val eop = Bool()

    override def asMaster(): Unit = {
      master(stream)
      in(eop)
    }
  }

  case class PhyIo() extends Bundle with IMasterSlave {
    val tx = Tx()
    val rx = Rx()

    val reset, resume = Bool()
    val tick = Bool()
    val power = Bool()
    val resumeIt = Bool()

    override def asMaster(): Unit = {
      in(tick, reset, resume, power)
      out(resumeIt)
      master(tx)
      slave(rx)
    }
  }
}

case class UsbDeviceCtrl(p: UsbDeviceCtrlParameter, bmbParameter : BmbParameter) extends Component {
  import UsbDeviceCtrl._

  val io = new Bundle {
    val ctrl = slave(Bmb(bmbParameter))
    val phy = master(PhyIo())
  }

  val done = new Area {
    val pendings = Reg(Bits(p.epCount bits))
    //    val head = Reg(UInt(p.addressWidth bits))
  }

  val regs = new Area {
    val frame = Reg(UInt(12 bits))
    val address = Reg(Bits(7 bits))
  }

  val memory = new Area{
    val ram = Mem(Bits(32 bits), 1 << p.addressWidth)

    val readPort = ram.readSyncPort
    val writePort = ram.writePortWithMask

    val internal = new Area{
      val readCmd = cloneOf(readPort.cmd)
      val readRsp = readCmd.stage().translateWith(readPort.rsp)
      val writeCmd = cloneOf(writePort)

      def doRead(byteAddress : UInt): Unit ={
        readCmd.valid := True
        readCmd.payload := byteAddress >> 2
      }

      def doWrite(byteAddress : UInt, data : Bits, enable : Bool): Unit ={
        writeCmd.valid := enable
        writeCmd.address := byteAddress >> 2
        writeCmd.data.subdivideIn(8 bits).foreach(_ := data)
        writeCmd.mask := UIntToOh(byteAddress(1 downto 0), 4)
      }

      def doWrite(byteAddress : UInt, data : Bits, mask : Bits, enable : Bool): Unit ={
        writeCmd.valid := enable
        writeCmd.address := byteAddress >> 2
        writeCmd.data := data
        writeCmd.mask := mask
      }


      readCmd.valid := False
      readCmd.payload.assignDontCare()
    }


    readPort.cmd.valid := internal.readCmd.valid
    readPort.cmd.payload := internal.readCmd.payload

    writePort.valid := internal.writeCmd.valid
    writePort.payload := internal.writeCmd.payload
  }


  val transferFull = desc.full || byteCounter.full

  val rxTimer = new Area {
    val counter = Reg(UInt(log2Up(16) bits))
    val clear = False
    when(io.phy.tick) {
      counter := counter + 1
    }
    when(clear) {
      counter := 0
    }
    clear setWhen(io.phy.rx.active)

    def cycles(c: Int): Bool = counter === (c-1)

    val timeout = cycles(24)
    val turnover = cycles(2)
  }

  val byteCounter = new Area{
    val value = Reg(UInt(10 bits))
    val clear, increment = False
    val full = value === ep.maxPacketSize

    when(increment){ value := value + 1 }
    when(clear){ value := 0 }
  }

  val token = new UsbTokenRxFsm(
    rx           = io.phy.rx.flow,
    rxActive     = io.phy.rx.active,
    rxStuffing   = io.phy.rx.stuffingError,
    timeoutClear = rxTimer.clear,
    timeoutEvent = rxTimer.timeout
  )

  val dataRx = new UsbDataRxFsm(
    rx           = io.phy.rx.flow,
    rxActive     = io.phy.rx.active,
    rxStuffing   = io.phy.rx.stuffingError,
    timeoutClear = rxTimer.clear,
    timeoutEvent = rxTimer.timeout
  )

  val dataTx = new UsbDataTxFsm(tx = io.phy.tx.stream,
                                eop = io.phy.tx.eop) {

    val input = Stream(Fragment(Bits(8 bits)))
    data << input.halfPipe().stage()
    when(data.valid && isStopped){
      startFsm()
    }
  }

  val descAlign = 4
  val ep = new Area {
    def addressByte = token.endpoint << 2
    def addressWord = token.endpoint

    val word = Reg(Bits(32 bits))

    val head = word(4, p.addressWidth >> descAlign bits).asUInt
    val enable = word(0)
    val stall = word(1)
    val nack = word(2)
    val dataPhase = word(3)
    val maxPacketSize = word(22, 10 bits).asUInt
    val headByte = head << descAlign
  }

  val desc = new Area {
    def addressByte = ep.headByte
    def addressWord = ep.head << (descAlign-2)

    val words = Vec(Reg(Bits(32 bits)), 3)

    val offset = words(0)(0, p.lengthWidth bits)
    val code = words(0)(16, 4 bits)

    val next = words(1)(0, p.addressWidth-descAlign bits).asUInt
    val length = words(1)(16, p.lengthWidth bits)

    val direction = words(2)(16)
    val interrupt = words(2)(17)
    val withZeroLengthEnd = words(2)(18)
    val frame = words(2)(0, 12 bits)

    val offsetIncrement = False
    when(offsetIncrement){
      offset := B(U(offset) + 1)
    }

    assert(descAlign == 4)
    val currentByte = (ep.head + (U(offset) >> descAlign) + 1) @@ U(offset.resize(descAlign))
    val full = offset === length
    val dataPhaseMatch = ep.dataPhase ? (dataRx.pid === UsbPid.DATA1) | (dataRx.pid === UsbPid.DATA0)
  }




  val active = new StateMachineSlave{
    val IDLE = new State
    val TOCKEN = new StateFsm(token)
    val ADDRESS_HIT = new State
    val EP_READ, EP_ANALYSE = new State
    val DESC_READ_0, DESC_READ_1, DESC_READ_2 = new State
    val DESC_ANALYSE = new State
    val DATA_RX, DATA_RX_ANALYSE = new State
    val HANDSHAKE_TX_0, HANDSHAKE_TX_1 = new State
    val DATA_TX_0, DATA_TX_1 = new State
    val HANDSHAKE_RX_0, HANDSHAKE_RX_1 = new State
    val UPDATE_SETUP, UPDATE_DESC, UPDATE_EP = new State

    val handshakePid = Reg(Bits(4 bits))
    val completion = Reg(Bool())

    IDLE whenIsActive{
      completion := False
      when(io.phy.rx.active){
        goto(TOCKEN)
      }
    }

    TOCKEN whenCompleted{
      goto(ADDRESS_HIT)
    }

    ADDRESS_HIT whenIsActive{
      when(token.ok && token.address === regs.address){
        memory.internal.doRead(ep.addressWord)
        when(token.pid === UsbPid.SETUP || token.pid === UsbPid.OUT){
          dataRx.startFsm()
        }
        goto(EP_READ)
      } otherwise {
        goto(IDLE)
      }
    }

    EP_READ whenIsActive{
      ep.word := memory.internal.readRsp.payload
      goto(EP_ANALYSE)
    }

    EP_ANALYSE whenIsActive{
      memory.internal.doRead(desc.addressByte)
      goto(DESC_READ_0)
    }

    DESC_READ_0 whenIsActive{
      desc.words(0) := memory.internal.readRsp.payload
      memory.internal.doRead(desc.addressByte | 4)
      goto(DESC_READ_1)
    }
    DESC_READ_1 whenIsActive{
      desc.words(1) := memory.internal.readRsp.payload
      memory.internal.doRead(desc.addressByte | 8)
      goto(DESC_READ_1)
    }

    DESC_READ_2 whenIsActive{
      desc.words(2) := memory.internal.readRsp.payload
      goto(DESC_ANALYSE)
    }

    DESC_ANALYSE whenIsActive{
      byteCounter.clear := True
      switch(token.pid){
        is(UsbPid.SETUP, UsbPid.OUT){
          when(desc.direction){
            goto(IDLE) //TODO ERROR
          } otherwise {
            goto(DATA_RX)
          }
        }
        is(UsbPid.IN){
          when(!desc.direction){
            goto(IDLE) //TODO ERROR
          } otherwise {
            goto(DATA_TX_0)
          }
        }
        default {
          goto(IDLE) //TODO ERROR
        }
      }
    }

    dataTx.pid := ep.dataPhase ## B"011"
    DATA_TX_0 whenIsActive{
      when(!transferFull){
        when(dataTx.input.ready) {
          memory.internal.doRead(desc.currentByte)
          byteCounter.increment := True
          desc.offsetIncrement := True
          goto(DATA_TX_1)
        }
      } otherwise {
        when(io.phy.tx.eop) {
          goto(HANDSHAKE_RX_0)
        }
      }
    }

    DATA_TX_1 whenIsActive{
      dataTx.input.valid := True
      dataTx.input.last := transferFull
      dataTx.input.fragment := memory.internal.readRsp.payload
      goto(DATA_TX_0)
    }

    HANDSHAKE_RX_0 onEntry{
      rxTimer.clear := True
    }
    HANDSHAKE_RX_0 whenIsActive{
      when(io.phy.rx.flow.valid){
        //TODO
        goto(HANDSHAKE_RX_1)
      }
      when(rxTimer.timeout){
        //TODO
      }
    }

    HANDSHAKE_RX_1 whenIsActive{
      when(!io.phy.rx.active){
        goto(UPDATE_SETUP)
      }
      when(io.phy.rx.flow.valid){
        //TODO
      }
    }

    DATA_RX whenIsActive{
      when(dataRx.data.valid){
        memory.internal.doWrite(desc.currentByte, dataRx.data.payload, !transferFull)
        when(transferFull){
          goto(IDLE) //TODO error
        } otherwise  {
          byteCounter.increment := True
          desc.offsetIncrement := True
        }
      }
      when(dataRx.wantExit){
        goto(DATA_RX_ANALYSE)
      }
    }

    DATA_RX_ANALYSE whenIsActive{
      when(???) { //TODO error handeling / pid toggle
      } otherwise {
        handshakePid := UsbPid.ACK //TODO
        goto(HANDSHAKE_TX_0)
      }
    }

    HANDSHAKE_TX_0 whenIsActive {
      when(rxTimer.turnover){
        goto(HANDSHAKE_TX_1)
      }
    }
    HANDSHAKE_TX_1 whenIsActive{
      io.phy.tx.stream.valid := True
      io.phy.tx.stream.last := True
      io.phy.tx.stream.fragment := UsbPid.token(handshakePid)
      when(io.phy.tx.stream.ready){
        goto(UPDATE_SETUP)
      }
    }
    
    UPDATE_SETUP whenIsActive{
      goto(UPDATE_DESC) //TODO
    }

    UPDATE_DESC whenIsActive{
      memory.internal.writeCmd.valid    := True
      memory.internal.writeCmd.address  := desc.addressWord
      memory.internal.writeCmd.mask     := 0xF
      memory.internal.writeCmd.data(0, p.lengthWidth bits) := desc.offset
      memory.internal.writeCmd.data(16, 4 bits) := 0 //TODO
    }

    UPDATE_EP whenIsActive{
      memory.internal.writeCmd.valid    := True
      memory.internal.writeCmd.address  := ep.addressWord
      memory.internal.writeCmd.mask     := 0x3
      memory.internal.writeCmd.data(0, 4 bits) := B(ep.dataPhase, ep.nack, ep.stall, ep.enable)
      memory.internal.writeCmd.data(4, p.addressWidth bits) := B(completion ? desc.next | ep.head)
    }
  }


  val main = new StateMachine{
    val ATTACHED, POWERED, ACTIVE_INIT, ACTIVE = new State
    setEntry(ATTACHED)

    ATTACHED whenIsActive{
      when(io.phy.power){
        goto(POWERED)
      }
    }
    POWERED whenIsActive{
      when(io.phy.reset){
        goto(ACTIVE_INIT)
      }
    }
    ACTIVE_INIT whenIsActive{
      regs.address := 0
      when(!io.phy.reset){

      }
    }
    ACTIVE whenIsActive{
      when(io.phy.reset){
        goto(ACTIVE_INIT)
      }
    }
  }
}

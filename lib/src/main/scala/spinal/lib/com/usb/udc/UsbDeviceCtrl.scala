package spinal.lib.com.usb.udc

import spinal.core._
import spinal.lib
import spinal.lib._
import spinal.lib.bus.bmb._
import spinal.lib.bus.misc.MaskMapping
import spinal.lib.com.eth.{Crc, CrcKind}
import spinal.lib.com.usb._
import spinal.lib.com.usb.ohci.{OhciPortParameter, UsbOhci, UsbOhciParameter, UsbPid}
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}
import spinal.lib.fsm._

case class UsbDeviceCtrlParameter(addressWidth : Int,
                                  epCount : Int = 16){
  def lengthWidth = (addressWidth + 1) min 15
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

    val pullup = Bool()
    val reset, suspend, disconnect = Bool()
    val resume = Flow(NoData)
    val tick = Bool()
    val power = Bool()
    val resumeIt = Bool()

    override def asMaster(): Unit = {
      in(tick, reset, suspend, resume, power, disconnect)
      out(resumeIt, pullup)
      master(tx)
      slave(rx)
    }

    def cc(cdFrom : ClockDomain, cdTo : ClockDomain) : PhyIo = {
      val c = PhyCc(cdFrom, cdTo).setCompositeName(this, "cc")
      c.input <> this
      c.output
    }
  }

  case class PhyCc(cdInput : ClockDomain, cdOutput : ClockDomain) extends Component {
    val input = slave(PhyIo())
    val output = master(PhyIo())

    output.tx.stream << cdOutput(input.tx.stream.ccToggle(cdInput, cdOutput).stage())
    input.tx.eop := PulseCCByToggle(output.tx.eop, cdOutput, cdInput)

    input.rx.flow << output.rx.flow.ccToggle(cdOutput, cdInput)
    input.rx.active := cdInput(BufferCC(output.rx.active))
    input.rx.stuffingError := cdInput(BufferCC(output.rx.stuffingError))

    output.pullup := cdOutput(BufferCC(input.pullup))
    output.resumeIt := cdOutput(BufferCC(input.resumeIt))
    input.tick := PulseCCByToggle(output.tick, cdOutput, cdInput)
    input.reset  := cdInput(BufferCC(output.reset))
    input.suspend := cdInput(BufferCC(output.suspend))
    input.resume << output.resume.ccToggle(cdOutput, cdInput)
    input.power := cdInput(BufferCC(output.power))
    input.disconnect := cdInput(BufferCC(output.disconnect))
  }


  val ctrlAddressWidth = 16
  def ctrlCapabilities(accessSource : BmbAccessCapabilities) = BmbSlaveFactory.getBmbCapabilities(
    accessSource,
    addressWidth = ctrlAddressWidth,
    dataWidth = 32
  )

  object Status{
    val STALL = 1
  }

  object Regs{
    val FRAME = 0xFF00
    val ADDRESS = 0xFF04
    val INTERRUPT = 0xFF08
    val HALT = 0xFF0C
    val CONFIG = 0xFF10
    val ADDRESS_WIDTH = 0xFF20
  }
  object Code{
    val NONE = 0xF
    val DONE = 0x0
  }
}

case class UsbDeviceCtrl(p: UsbDeviceCtrlParameter, bmbParameter : BmbParameter) extends Component {
  import UsbDeviceCtrl._

  val io = new Bundle {
    val ctrl = slave(Bmb(bmbParameter))
    val phy = master(PhyIo())
    val interrupt = out Bool()
  }

  io.phy.resumeIt := False
  io.phy.tx.stream.valid := False
  io.phy.tx.stream.payload.assignDontCare()

  val ctrl = BmbSlaveFactory(io.ctrl)

  val done = new Area {
    val pendings = Reg(Bits(p.epCount bits))
    //    val head = Reg(UInt(p.addressWidth bits))
  }

  val regs = new Area {
    val frame = Reg(UInt(11 bits))
    val address = new Area {
      val value = Reg(Bits(7 bits))
      val enable = Reg(Bool()) init(False)
      val trigger = Reg(Bool()) init(False)
    }
    val interrupts = new Area{
      val endpoints = Reg(Bits(p.epCount bits)) init(0)
      val reset = RegInit(False)
      val suspend = RegInit(False) setWhen(io.phy.suspend.rise(False))
      val resume = RegInit(False)  setWhen(io.phy.resume.valid)
      val disconnect = RegInit(False) setWhen(io.phy.power.fall(False))
      val ep0Setup = RegInit(False)

      val enable = RegInit(False)
      val pending = (endpoints.orR || reset || suspend || resume || disconnect || ep0Setup) && enable
    }
    val halt = new Area{
      val id = Reg(UInt(log2Up(p.epCount) bits))
      val enable = RegInit(False)
      val effective = RegInit(False)
      val hit = Bool()
    }
    val pullup = Reg(Bool) init(False)
    io.phy.pullup := pullup
  }

  val memory = new Area{
    val ram = Mem(Bits(32 bits), 1 << p.addressWidth-2)

    val readPort = ram.readSyncPort
    val writePort = ram.writePortWithMask(4)

    val internal = new Area{
      val writeCmd = cloneOf(writePort)
      val readCmd = cloneOf(readPort.cmd)
      val readRsp = readCmd.stage().translateWith(readPort.rsp)

      writeCmd.mask.setWidth(4)

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
      writeCmd.valid := False
      writeCmd.payload.assignDontCare()
    }

    val external = new Area{
      val halt = False
      val writeCmd = Stream(writePort.payloadType)
      val readCmd = Stream(readPort.cmd.payloadType)
      val readRsp = readCmd.toFlowFire.stage.translateWith(readPort.rsp)

      val writeCmdHalted = writeCmd.haltWhen(halt)

      writeCmd.mask.setWidth(4)
    }


    readPort.cmd.valid := internal.readCmd.valid || external.readCmd.valid
    readPort.cmd.payload := internal.readCmd.valid ? internal.readCmd.payload | external.readCmd.payload
    external.readCmd.ready := !internal.readCmd.valid

    writePort.valid := internal.writeCmd.valid || external.writeCmdHalted.valid
    writePort.payload := internal.writeCmd.valid ? internal.writeCmd.payload | external.writeCmdHalted.payload
    external.writeCmdHalted.ready := !internal.writeCmd.valid
  }


  val rxTimer = new Area {
    val counter = Reg(UInt(5 bits))
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

  val token = new UsbTokenRxFsm(
    rx           = io.phy.rx.flow,
    rxActive     = io.phy.rx.active,
    rxStuffing   = io.phy.rx.stuffingError,
    timeoutClear = rxTimer.clear,
    timeoutEvent = rxTimer.timeout
  ){
    val isSetup = pid === UsbPid.SETUP
    val isIn = pid === UsbPid.IN
  }
  regs.halt.hit := regs.halt.enable && regs.halt.id === token.endpoint

  val dataRx = new UsbDataRxFsm(
    rx           = io.phy.rx.flow,
    rxActive     = io.phy.rx.active,
    rxStuffing   = io.phy.rx.stuffingError,
    timeoutClear = rxTimer.clear,
    timeoutEvent = rxTimer.timeout
  )

  val dataTx = new UsbDataTxFsm(tx = io.phy.tx.stream,
                                eop = io.phy.tx.eop) {

    val startNull = False
    val input = Stream(Fragment(Bits(8 bits))) //Allow sparse transactions
    input.valid := False
    input.payload.assignDontCare()

    data.valid.removeAssignments()
    data.payload.removeAssignments()
    data << input.halfPipe().stage()
    when(data.valid && isStopped || startNull){
      startFsm()
    }
  }

  val descAlign = 4
  val ep = new Area {
    def addressByte = addressWord << 2
    def addressWord = token.endpoint.resize(p.addressWidth-2)

    val word = Reg(Bits(32 bits))

    val head = word(4, p.addressWidth - descAlign bits).asUInt
    val enable = word(0)
    val stall = word(Status.STALL)
    val nack = word(2)
    val dataPhase = word(3)
    val isochronous = word(16)
    val maxPacketSize = word(22, 10 bits).asUInt
    val headByte = head << descAlign
  }

  val desc = new Area {
    def addressByte = ep.headByte
    def addressWord = ep.head << (descAlign-2)

    val words = Vec(Reg(Bits(32 bits)), 3)

    val offset = words(0)(0, p.lengthWidth bits)
    val code = words(0)(16, 4 bits)

    val next = words(1)(4, p.addressWidth-descAlign bits).asUInt
    val length = words(1)(16, p.lengthWidth bits)

    val direction = words(2)(16)
    val interrupt = words(2)(17)
    val completionOnFull = words(2)(18)
    val data1OnCompletion = words(2)(19)
    val frame = words(2)(0, 12 bits)

    val offsetIncrement = False
    when(offsetIncrement){
      offset := B(U(offset) + 1)
    }

    assert(descAlign == 4)
    val currentByte = ((ep.head @@ U"1100") + U(offset)).resize(p.addressWidth)
    val full = offset === length
    val dataPhaseMatch = ep.dataPhase ? (dataRx.pid === UsbPid.DATA1) | (dataRx.pid === UsbPid.DATA0)
  }

  val byteCounter = new Area{
    val value = Reg(UInt(10 bits))
    val clear, increment = False
    val full = value === ep.maxPacketSize

    when(increment){ value := value + 1 }
    when(clear){ value := 0 }
  }
  val transferFull = desc.full || byteCounter.full



  val active = new StateMachineSlave{
    val IDLE = new State
    val TOKEN = new StateFsm(token)
    val ADDRESS_HIT = new State
    val EP_READ, EP_ANALYSE = new State
    val DESC_READ_0, DESC_READ_1, DESC_READ_2 = new State
    val DESC_ANALYSE = new State
    val DATA_RX, DATA_RX_ANALYSE = new State
    val HANDSHAKE_TX_0, HANDSHAKE_TX_1 = new State
    val DATA_TX_0, DATA_TX_1 = new State
    val HANDSHAKE_RX_0, HANDSHAKE_RX_1 = new State
    val UPDATE_SETUP, UPDATE_DESC, UPDATE_EP = new State

    setEntry(IDLE)

    val handshakePid = Reg(Bits(4 bits))
    val completion = Reg(Bool())
    val noUpdate = Reg(Bool())

    regs.halt.effective setWhen(isStopped || isActive(IDLE) || isActive(TOKEN) || !regs.halt.hit)

    IDLE whenIsActive{
      completion := False
      noUpdate  := False

      when(io.phy.rx.active){
        goto(TOKEN)
      }
    }

    TOKEN whenCompleted{
      goto(ADDRESS_HIT)
    }

    ADDRESS_HIT whenIsActive{
      when(!token.ok){
        goto(IDLE)
      } otherwise {
        switch(token.pid) {
          is(UsbPid.SOF) {
            regs.frame := U(token.data)
            goto(IDLE)
          }
          is(UsbPid.SETUP, UsbPid.OUT, UsbPid.IN) {
            when(token.address === (regs.address.enable ? regs.address.value otherwise 0)) {
              memory.internal.doRead(ep.addressByte)
              when(token.pid === UsbPid.SETUP || token.pid === UsbPid.OUT) {
                dataRx.startFsm()
              }
              goto(EP_READ)
            } otherwise {
              goto(IDLE)
            }
          }
          default {
            goto(IDLE)
          }
        }
      }
    }

    EP_READ whenIsActive{
      ep.word := memory.internal.readRsp.payload
      goto(EP_ANALYSE)
    }

    EP_ANALYSE whenIsActive{
      memory.internal.doRead(desc.addressByte)
      when(!ep.enable){
        goto(IDLE)
      } elsewhen(token.isSetup){
        when(token.endpoint =/= 0){
          goto(IDLE)
        } otherwise {
          ep.word(15 downto 4) := 0 //Ensure no offset is applyed from memory address 0x40-0x47
          desc.offset := 0x40-12
          desc.length := 8
          desc.direction := False
          ep.dataPhase := False
          goto(DESC_ANALYSE)
        }
      } elsewhen(ep.head === 0 || ep.stall || regs.halt.hit){
        handshakePid := ((ep.stall && !regs.halt.hit) ? B(UsbPid.STALL) | B(UsbPid.NAK)).resized
        switch(token.pid){
          is(UsbPid.OUT){
            noUpdate := True
            goto(DATA_RX)
          }
          is(UsbPid.IN){
            noUpdate := True
            when(ep.isochronous){
              goto(IDLE)
            } otherwise {
              goto(HANDSHAKE_TX_0)
            }
          }
          default {
            goto(IDLE)
          }
        }
      } otherwise {
        goto(DESC_READ_0)
      }
    }

    DESC_READ_0 whenIsActive{
      desc.words(0) := memory.internal.readRsp.payload
      memory.internal.doRead(desc.addressByte | 4)
      goto(DESC_READ_1)
    }
    DESC_READ_1 whenIsActive{
      desc.words(1) := memory.internal.readRsp.payload
      memory.internal.doRead(desc.addressByte | 8)
      goto(DESC_READ_2)
    }

    DESC_READ_2 whenIsActive{
      desc.words(2) := memory.internal.readRsp.payload
      goto(DESC_ANALYSE)
    }

    DESC_ANALYSE whenIsActive{
      byteCounter.clear := True
      switch(token.pid){
        is(UsbPid.SETUP) {
          goto(DATA_RX)
        }
        is(UsbPid.OUT){
          when(desc.direction){
            goto(IDLE)
          } otherwise {
            goto(DATA_RX)
          }
        }
        is(UsbPid.IN){
          when(!desc.direction){
            goto(IDLE)
          } otherwise {
            when(desc.full){
              dataTx.startNull := True
            }
            goto(DATA_TX_0)
          }
        }
        default {
          goto(IDLE)
        }
      }
    }

    dataTx.pid := ep.dataPhase ## B"011"
    val byteSel = Reg(UInt(2 bits))
    DATA_TX_0 whenIsActive{
      byteSel := U(desc.offset).resized
      when(!transferFull){
        when(dataTx.input.ready) {
          memory.internal.doRead(desc.currentByte)
          byteCounter.increment := True
          desc.offsetIncrement := True
          goto(DATA_TX_1)
        }
      } otherwise {
        when(io.phy.tx.eop) {
          when(ep.isochronous) {
            goto(UPDATE_SETUP)
          } otherwise {
            goto(HANDSHAKE_RX_0)
          }
        }
      }
    }

    DATA_TX_1 whenIsActive{
      dataTx.input.valid := True
      dataTx.input.last := transferFull
      dataTx.input.fragment := memory.internal.readRsp.payload.subdivideIn(8 bits)(byteSel)
      goto(DATA_TX_0)
    }

    HANDSHAKE_RX_0 onEntry{
      rxTimer.clear := True
    }
    HANDSHAKE_RX_0 whenIsActive{
      when(io.phy.rx.flow.valid){
        when(io.phy.rx.flow.payload(3 downto 0) =/= ~io.phy.rx.flow.payload(7 downto 4)){
          goto(IDLE)
        } elsewhen(io.phy.rx.flow.payload(3 downto 0) =/= UsbPid.ACK){
          goto(IDLE)
        } otherwise {
          goto(HANDSHAKE_RX_1)
        }
      }
      when(rxTimer.timeout || io.phy.rx.active.fall()){
        goto(IDLE)
      }
    }

    HANDSHAKE_RX_1 whenIsActive{
      when(!io.phy.rx.active){
        goto(UPDATE_SETUP)
      }
      when(io.phy.rx.flow.valid){
        goto(IDLE)
      }
    }

    val dataRxOverrun = Reg(Bool())
    DATA_RX onEntry{
      dataRxOverrun := False
    }
    DATA_RX whenIsActive{
      when(dataRx.data.valid){
        memory.internal.doWrite(desc.currentByte, dataRx.data.payload, !transferFull && !noUpdate)
        when(transferFull && !noUpdate){
          dataRxOverrun := True
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
      when(dataRx.hasError || dataRxOverrun) { //TODO Maybe dataRxOverrun should ACK ?
        goto(IDLE)
      } otherwise {
        when(!noUpdate){
          handshakePid := UsbPid.ACK
        }
        when(dataRx.pid(2 downto 0) =/= B"011"){
          goto(IDLE)
        } otherwise {
          when(!ep.stall && dataRx.pid.msb =/= ep.dataPhase) {
            noUpdate := True
            handshakePid := UsbPid.ACK //Ensure that a wrong data phase is ACKED (even in the case there is no descriptor)
          }
          when(ep.isochronous) {
            goto(UPDATE_SETUP)
          } otherwise {
            goto(HANDSHAKE_TX_0)
          }
        }
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
      memory.external.halt := True
      memory.internal.doRead(desc.addressByte | 4) //Fetch the next descriptor in a atomic manner to ease the software tail insertion

      when(!token.isSetup) {
        completion setWhen (!byteCounter.full || desc.completionOnFull && desc.full)
      }


      when(noUpdate) {
        goto(IDLE)
      } otherwise {
        goto(UPDATE_DESC)
      }
    }

    UPDATE_DESC whenIsActive{
      memory.external.halt := True
      desc.words(1) := memory.internal.readRsp.payload

      memory.internal.writeCmd.valid    := !token.isSetup
      memory.internal.writeCmd.address  := desc.addressWord
      memory.internal.writeCmd.mask     := 0xF
      memory.internal.writeCmd.data := 0
      memory.internal.writeCmd.data(0, p.lengthWidth bits) := desc.offset
      memory.internal.writeCmd.data(16, 4 bits) := (completion ? B(0) | B(15)) //TODO if more error condition, update condition of completion when(!desc.full){

      goto(UPDATE_EP)
    }

    UPDATE_EP whenIsActive{
      memory.external.halt := True
      memory.internal.writeCmd.valid    := True
      memory.internal.writeCmd.address  := ep.addressWord
      memory.internal.writeCmd.mask     := 0x3
      memory.internal.writeCmd.data(0, 4 bits) := B(!ep.dataPhase, ep.nack, ep.stall, ep.enable)
      memory.internal.writeCmd.data(4, 12 bits) := B(completion ? desc.next | ep.head).resized
      when(token.isSetup){
        memory.internal.writeCmd.data(Status.STALL) := False
        memory.internal.writeCmd.data(4, 12 bits) := 0
        regs.interrupts.ep0Setup := True
      }

      when(completion) {
        when(desc.data1OnCompletion){
          memory.internal.writeCmd.data(3) := True
        }
        when(desc.interrupt) {
          regs.interrupts.endpoints(token.endpoint.resized) := True
        }
        when(token.endpoint === 0) {
          when(regs.address.trigger && token.isIn) {
            regs.address.enable := True
          }
          regs.address.trigger := False
        }
        when(!desc.full){ //When a descriptor is completed but not full, unlink the linked list for the software to fix things
          memory.internal.writeCmd.data(4, 12 bits) := 0
        }
      }
      goto(IDLE)
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

    ACTIVE_INIT onEntry {
      regs.interrupts.reset := True
    }
    ACTIVE_INIT whenIsActive{
      regs.address.enable := False
      when(!io.phy.reset){
        goto(ACTIVE)
      }
    }

    ACTIVE onEntry {
      active.startFsm()
    }
    ACTIVE whenIsActive{
      when(io.phy.reset){
        goto(ACTIVE_INIT)
      }
    }
  }

  import UsbDeviceCtrl.Regs

  val mapping = new Area {
    ctrl.read(regs.frame   , Regs.FRAME)
    ctrl.write(regs.address.value, Regs.ADDRESS, 0)
    ctrl.write(regs.address.enable, Regs.ADDRESS, 8)
    ctrl.write(regs.address.trigger, Regs.ADDRESS, 9)
    ctrl.read(regs.interrupts.endpoints, Regs.INTERRUPT)
    ctrl.clearOnSet(regs.interrupts.endpoints, Regs.INTERRUPT)
    def mapInterrupt(flag : Bool, id : Int): Unit ={
      ctrl.read(flag, Regs.INTERRUPT, id)
      ctrl.clearOnSet(flag, Regs.INTERRUPT, id)
    }
    mapInterrupt(regs.interrupts.reset, 16)
    mapInterrupt(regs.interrupts.ep0Setup, 17)
    mapInterrupt(regs.interrupts.suspend, 18)
    mapInterrupt(regs.interrupts.resume, 19)
    mapInterrupt(regs.interrupts.disconnect, 20)
    ctrl.write(regs.halt.id, Regs.HALT, 0)
    ctrl.write(regs.halt.enable, Regs.HALT, 4)
    ctrl.readAndWrite(regs.halt.effective, Regs.HALT, 5)
    ctrl.setOnSet(regs.pullup, Regs.CONFIG, 0)
    ctrl.clearOnSet(regs.pullup, Regs.CONFIG, 1)
    ctrl.setOnSet(regs.interrupts.enable, Regs.CONFIG, 2)
    ctrl.clearOnSet(regs.interrupts.enable, Regs.CONFIG, 3)
    ctrl.read(U(p.addressWidth), Regs.ADDRESS_WIDTH)

    val memoryMapping = MaskMapping(0x0000, 0x8000)
    val readBuffer = memory.external.readRsp.toReg
    val readState = RegInit(U"00")
    val writeState = RegInit(U"0")
    memory.external.readCmd.valid := False
    memory.external.readCmd.payload := (ctrl.readAddress() >> 2).resized
    memory.external.writeCmd.valid := False
    memory.external.writeCmd.address := (ctrl.writeAddress() >> 2).resized
    memory.external.writeCmd.mask := 0xF
    ctrl.nonStopWrite(memory.external.writeCmd.data)
    ctrl.readPrimitive(readBuffer, memoryMapping, 0, null)

    ctrl.onReadPrimitive(memoryMapping, haltSensitive = false, documentation = null) {
      switch(readState){
        is(0){
          ctrl.readHalt()
          memory.external.readCmd.valid := True
          when(memory.external.readCmd.ready) {
            readState := 1
          }
        }
        is(1){
          ctrl.readHalt()
          when(memory.external.readRsp.fire) {
            readState := 2
          }
        }
      }
    }
    ctrl.onWritePrimitive(memoryMapping, haltSensitive = false, documentation = null){
      switch(writeState){
        is(0){
          ctrl.writeHalt()
          memory.external.writeCmd.valid := True
          when(memory.external.writeCmd.ready){
            writeState := 1
          }
        }
      }
    }

    ctrl.onReadPrimitive(memoryMapping, haltSensitive = true, documentation = null) {
      readState := 0
    }
    ctrl.onWritePrimitive(memoryMapping, haltSensitive = true, documentation = null){
      writeState := 0
    }
  }


  io.interrupt := RegNext(regs.interrupts.pending) init(False)
}


object UsbDeviceCtrlGen extends App {
  SpinalVerilog(new UsbDeviceCtrl(
    p = UsbDeviceCtrlParameter(
      addressWidth = 16
    ),
    bmbParameter = BmbParameter(
      addressWidth = 17,
      dataWidth = 32,
      sourceWidth = 0,
      contextWidth = 0,
      lengthWidth = 2
    )
  ))
}

object UsbDeviceCtrlSynt extends App{
  val rawrrr = new Rtl {
    override def getName(): String = "UsbDeviceCtrl"
    override def getRtlPath(): String = "UsbDeviceCtrl.v"
    SpinalVerilog(new UsbDeviceCtrl(
      p = UsbDeviceCtrlParameter(
        addressWidth = 12
      ),
      bmbParameter = BmbParameter(
        addressWidth = 17,
        dataWidth = 32,
        sourceWidth = 0,
        contextWidth = 0,
        lengthWidth = 2
      )
    ))
  }


  val rtls = List(rawrrr)

  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}


//TOOD test to many bytes in / out

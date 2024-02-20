package spinal.lib.com.usb.ohci

import spinal.core._
import spinal.core.sim.{SimPublic, SimSeqPimper}
import spinal.lib._
import spinal.lib.bus.bmb._
import spinal.lib.bus.wishbone.{Wishbone, WishboneConfig, WishboneToBmb}
import spinal.lib.com.eth.{Crc, CrcKind}
import spinal.lib.com.usb.{UsbDataRxFsm, UsbDataTxFsm, UsbTimer, UsbTokenTxFsm}
import spinal.lib.com.usb.ohci.UsbOhci.dmaParameter
import spinal.lib.com.usb.phy.UsbHubLsFs.CtrlCc
import spinal.lib.com.usb.phy.{UsbHubLsFs, UsbLsFsPhy, UsbLsFsPhyAbstractIo, UsbPhyFsNativeIo}
import spinal.lib.fsm._

import scala.collection.mutable.ArrayBuffer

case class OhciPortParameter(removable : Boolean = true, powerControlMask : Boolean = true)

case class UsbOhciParameter(noPowerSwitching : Boolean,
                            powerSwitchingMode : Boolean,
                            noOverCurrentProtection : Boolean,
                            //                            overCurrentProtectionMode : Boolean,
                            powerOnToPowerGoodTime : Int,
                            dataWidth : Int,
                            portsConfig : Seq[OhciPortParameter],
                            dmaLengthWidth : Int = 6){
  assert(dmaLengthWidth >= 5 && dmaLengthWidth <= 12)
  def dmaLength = 1 << dmaLengthWidth
  val portCount = portsConfig.size
}

object UsbOhci{
  def dmaParameter(p : UsbOhciParameter) = BmbParameter(
    addressWidth = 32,
    dataWidth = p.dataWidth,
    sourceWidth = 0,
    contextWidth = 0,
    lengthWidth = p.dmaLengthWidth,
    alignment = BmbParameter.BurstAlignement.LENGTH
  )

  def ctrlCapabilities(accessSource : BmbAccessCapabilities) = BmbSlaveFactory.getBmbCapabilities(
    accessSource,
    addressWidth = ctrlAddressWidth,
    dataWidth = 32
  )
  def ctrlAddressWidth = 12

  val HcRevision         = 0x0
  val HcControl          = 0x4
  val HcCommandStatus    = 0x8
  val HcInterruptStatus  = 0xC
  val HcInterruptEnable  = 0x10
  val HcInterruptDisable = 0x14
  val HcHCCA             = 0x18
  val HcPeriodCurrentED  = 0x1C
  val HcControlHeadED    = 0x20
  val HcControlCurrentED = 0x24
  val HcBulkHeadED       = 0x28
  val HcBulkCurrentED    = 0x2C
  val HcDoneHead         = 0x30
  val HcFmInterval       = 0x34
  val HcFmRemaining      = 0x38
  val HcFmNumber         = 0x3C
  val HcPeriodicStart    = 0x40
  val HcLSThreshold      = 0x44
  val HcRhDescriptorA    = 0x48
  val HcRhDescriptorB    = 0x4C
  val HcRhStatus         = 0x50
  val HcRhPortStatus     = 0x54

  val MasterInterruptEnable = 0x80000000l
  val WritebackDoneHead     = 0x2
  val StartofFrame          = 0x4


  object CC {
    val noError = Integer.parseInt("0000",2)
    val crc = Integer.parseInt("0001",2)
    val bitStuffing = Integer.parseInt("0010",2)
    val dataToggleMismatch = Integer.parseInt("0011",2)
    val stall = Integer.parseInt("0100",2)
    val deviceNotResponding = Integer.parseInt("0101",2)
    val pidCheckFailure = Integer.parseInt("0110",2)
    val unexpectedPid = Integer.parseInt("0111",2)
    val dataOverrun = Integer.parseInt("1000",2)
    val dataUnderrun = Integer.parseInt("1001",2)
    val bufferOverrun = Integer.parseInt("1100",2)
    val bufferUnderrun = Integer.parseInt("1101",2)
    val notAccessed = Integer.parseInt("1110",2)
  }

  object DP{
    val SETUP = 0
    val OUT = 1
    val IN = 2
  }
}

object UsbPid{
  val OUT = Integer.parseInt("0001",2)
  val IN = Integer.parseInt("1001",2)
  val SOF = Integer.parseInt("0101",2)
  val SETUP = Integer.parseInt("1101",2)
  val DATA0 = Integer.parseInt("0011",2)
  val DATA1 = Integer.parseInt("1011",2)
  val DATA2 = Integer.parseInt("0111",2)
  val MDATA = Integer.parseInt("1111",2)
  val ACK = Integer.parseInt("0010",2)
  val NAK = Integer.parseInt("1010",2)
  val STALL = Integer.parseInt("1110",2)
  val NYET = Integer.parseInt("0110",2)
  val PRE = Integer.parseInt("1100",2)
  val ERR = Integer.parseInt("1100",2)
  val SPLIT = Integer.parseInt("1000",2)
  val PING = Integer.parseInt("0100",2)

  val all = List(OUT, IN, SOF, SETUP, DATA0, DATA1, DATA2, MDATA, ACK, NAK, STALL, NYET, PRE, ERR, SPLIT, PING)
  val allButSetup = List(OUT, IN, SOF, DATA0, DATA1, DATA2, MDATA, ACK, NAK, STALL, NYET, PRE, ERR, SPLIT, PING)

  def anyBut(pid : Int) = all.filter(_ != pid).randomPick()
  def token(pid : Int) = pid | ((0xF ^ pid) << 4)
  def token(pid : Bits) = ~pid ## pid
}

case class UsbOhci(p : UsbOhciParameter, ctrlParameter : BmbParameter) extends Component{
  val io = new Bundle {
    val ctrl = slave(Bmb(ctrlParameter))
    val phy = master(UsbHubLsFs.Ctrl(p.portCount))
    val dma = master(Bmb(UsbOhci.dmaParameter(p)))
    val interrupt = out Bool()
    val interruptBios = out Bool()
  }
  io.phy.lowSpeed := False

  val unscheduleAll = Event
  unscheduleAll.valid := False
  unscheduleAll.ready := True

  val ioDma = Bmb(UsbOhci.dmaParameter(p))
  // Track the number of inflight memory requests, used to be sure everything is done before continuing
  val dmaCtx = new Area{
    val pendingCounter = Reg(UInt(4 bits)) init(0)
    pendingCounter := pendingCounter + U(ioDma.cmd.lastFire) - U(ioDma.rsp.lastFire)

    val pendingFull = pendingCounter.msb
    val pendingEmpty = pendingCounter === 0

    val beatCounter = Reg(UInt(p.dmaLengthWidth bits)) init(0)
    when(ioDma.cmd.fire){
      beatCounter := beatCounter + 1
      when(io.dma.cmd.last){
        beatCounter := 0
      }
    }

    unscheduleAll.ready clearWhen(!pendingEmpty)
  }

  io.dma.cmd << ioDma.cmd.haltWhen(dmaCtx.pendingFull || (unscheduleAll.valid && io.dma.cmd.first))
  io.dma.rsp >> ioDma.rsp


  ioDma.cmd.valid := False
  ioDma.cmd.payload.assignDontCare()
  ioDma.cmd.data.removeAssignments() := 0
  ioDma.cmd.mask.removeAssignments() := 0
  ioDma.rsp.ready := True

  val dmaRspMux = new Area{
    val vec = ioDma.rsp.data.subdivideIn(32 bits)
    val sel = UInt(log2Up(vec.length) bits)
    val data = vec(sel)
  }

  //Follow the DMA read response to setup internals registers via the usage of the load function
  val dmaReadCtx = new Area{
    val wordPerBeat = p.dataWidth/32
    val counterStates = (1 << p.dmaLengthWidth)/(p.dataWidth/8)
    val counter = Reg(UInt(log2Up(counterStates) bits)) init(0)
    when(ioDma.rsp.fire){
      counter := counter + 1
      when(ioDma.rsp.last){
        counter := 0
      }
    }
    def load[T <: Data](reg : T, word : Int, bit : Int): Unit = {
      when(ioDma.rsp.valid && counter === word/wordPerBeat){
        reg.assignFromBits(dmaRspMux.vec(word % wordPerBeat)(bit, widthOf(reg) bits))
      }
    }
  }

  //Follow the DMA write cmd to set the appropriated cmd.data values following the save function calls
  val dmaWriteCtx = new Area{
    val wordPerBeat = p.dataWidth/32
    val counterStates = (1 << p.dmaLengthWidth)/(p.dataWidth/8)
    val counter = Reg(UInt(log2Up(counterStates) bits)) init(0)
    when(ioDma.cmd.fire){
      counter := counter + 1
      when(ioDma.cmd.last){
        counter := 0
      }
    }
    def save[T <: Data](value : T, word : Int, bit : Int): Unit = {
      when(counter === word/wordPerBeat){
        val offset = (word % wordPerBeat) * 32 + bit
        ioDma.cmd.mask(offset/8, (widthOf(value)+7)/8 bits).setAll()
        ioDma.cmd.data(offset, widthOf(value) bits) := value.asBits
      }
    }
  }

  // Used as buffer for USB data <> DMA transfers
  val fifo = StreamFifo(Bits(p.dataWidth bits), 1024*2*8/p.dataWidth) //ramBurstCapacity * p.dmaLength*8/p.dmaLengthWidth
  fifo.io.push.valid := False
  fifo.io.push.payload.assignDontCare()
  fifo.io.pop.ready := False
  fifo.io.flush := False

  //Patch some unecessary check in the fifo
  fifo.rework{
    //    fifo.logic.empty.removeAssignments() := False //Used against data overflow
    fifo.logic.ptr.full.removeAssignments() := False
  }

  io.phy.tx.valid := False
  io.phy.tx.fragment.assignDontCare()
  io.phy.tx.last.assignDontCare()

  val ctrlHalt = False
  val ctrl = BmbSlaveFactory(io.ctrl)
  when(ctrlHalt) { ctrl.writeHalt() } //Ensure no race conditions between io.ctrl and HC

  val doUnschedule = RegInit(False) clearWhen(unscheduleAll.ready)
  val doSoftReset = RegInit(False) clearWhen(!doUnschedule)
  unscheduleAll.valid setWhen(doUnschedule)

  val softInitTasks = ArrayBuffer[() => Unit]()
  implicit class OhciDataPimper[T <: Data](self : T){
    def softInit(value : T) : T = {
      softInitTasks += (() => self := value)
      self
    }
  }
  implicit class OhciDataPimperBool(self : Bool) extends OhciDataPimper(self){
    def softInit(value : Boolean) : Bool = {
      softInitTasks += (() => self := Bool(value))
      self
    }
  }
  afterElaboration{
    when(doSoftReset || RegNext(False).init(True)){
      softInitTasks.foreach(_())
    }
  }





  object MainState extends SpinalEnum{
    val RESET, RESUME, OPERATIONAL, SUSPEND = newElement()
  }


  val reg = new Area {
    val hcRevision = new Area {
      val REV = ctrl.read(B(0x10), 0x00)
    }

    val hcControl = new Area {
      val CBSR = ctrl.createReadAndWrite(UInt(2 bits), 0x04, 0) softInit (0)
      val PLE = ctrl.createReadAndWrite(Bool(), 0x04, 2) softInit (False)
      val IE = ctrl.createReadAndWrite(Bool(), 0x04, 3) softInit (False)
      val CLE = ctrl.createReadAndWrite(Bool(), 0x04, 4) softInit (False)
      val BLE = ctrl.createReadAndWrite(Bool(), 0x04, 5) softInit (False)
      val HCFS = ctrl.read(MainState(), 0x04, 6)
      val IR =  ctrl.createReadAndWrite(Bool(), 0x04, 8) init(False)
      val RWC = ctrl.createReadAndWrite(Bool(), 0x04, 9) init(False)
      val RWE = ctrl.createReadAndWrite(Bool(), 0x04, 10) softInit (False)

      val HCFSWrite = ctrl.createAndDriveFlow(MainState(), 0x04, 6)
    }

    val hcCommandStatus = new Area {
      val startSoftReset = ctrl.setOnSet(False, 0x08, 0)
      val HCR = ctrl.read(doSoftReset, 0x08, 0)
      val CLF = ctrl.createReadAndSetOnSet(doSoftReset, 0x08, 1) softInit (False)
      val BLF = ctrl.createReadAndSetOnSet(Bool(), 0x08, 2) softInit (False)
      val OCR = ctrl.createReadAndSetOnSet(Bool(), 0x08, 3) softInit (False)
      val SOC = ctrl.createReadOnly(UInt(2 bits), 0x08, 16) softInit (0)
    }

    val hcInterrupt = new Area {
      val unmaskedPending = False
      def createInterrupt(id: Int, smi : Boolean = false) = new Area {
        val status = ctrl.createReadAndClearOnSet(Bool(), UsbOhci.HcInterruptStatus, id) softInit (False)
        val enable = Reg(Bool()) softInit(False)
        ctrl.readAndSetOnSet(enable, UsbOhci.HcInterruptEnable, id)
        ctrl.readAndClearOnSet(enable, UsbOhci.HcInterruptDisable, id)
        if(!smi) unmaskedPending setWhen(status && enable)
      }

      val MIE = Reg(Bool()) softInit(False)
      ctrl.readAndSetOnSet(MIE, 0x10, 31)
      ctrl.readAndClearOnSet(MIE, 0x14, 31)
      val SO = createInterrupt(0)
      val WDH = createInterrupt(1)
      val SF = createInterrupt(2)
      val RD = createInterrupt(3)
      val UE = createInterrupt(4)
      val FNO = createInterrupt(5)
      val RHSC = createInterrupt(6)
      val OC = createInterrupt(30, smi = true)
      OC.status.setWhen(hcCommandStatus.OCR)

      val doIrq = unmaskedPending && MIE
      io.interrupt := doIrq && !hcControl.IR
      io.interruptBios := doIrq && hcControl.IR || OC.status && OC.enable
    }

    def mapAddress(zeroWidth: Int, mapping: Int, driverWrite: Boolean) = new Area {
      val address = UInt(32 bits)
      val reg = Reg(UInt(32 - zeroWidth bits)) softInit (0)
      ctrl.read(reg, mapping, zeroWidth)
      if (driverWrite) ctrl.write(reg, mapping, zeroWidth)
      address := reg @@ U(0, zeroWidth bits)

      def load(that: Bits): Unit = {
        reg := that(31 downto zeroWidth).asUInt
      }
    }

    val hcHCCA = new Area {
      val HCCA = mapAddress(8, 0x18, true)
    }

    val hcPeriodCurrentED = new Area {
      val PCED = mapAddress(4, 0x1C, false)
      val isZero = PCED.reg === 0
    }

    val hcControlHeadED = new Area {
      val CHED = mapAddress(4, 0x20, true)
    }

    val hcControlCurrentED = new Area {
      val CCED = mapAddress(4, 0x24, true)
      val isZero = CCED.reg === 0
    }

    val hcBulkHeadED = new Area {
      val BHED = mapAddress(4, 0x28, true)
    }

    val hcBulkCurrentED = new Area {
      val BCED = mapAddress(4, 0x2C, true)
      val isZero = BCED.reg === 0
    }

    val hcDoneHead = new Area {
      val DH = mapAddress(4, 0x30, true)
    }

    val hcFmInterval = new Area {
      val FI = ctrl.createReadAndWrite(UInt(14 bits), 0x34, 0) softInit (11999)
      val FSMPS = ctrl.createReadAndWrite(UInt(15 bits), 0x34, 16) //softInit(???)
      val FIT = ctrl.createReadAndWrite(Bool(), 0x34, 31) softInit (False)
    }

    val hcFmRemaining = new Area {
      val FR = ctrl.createReadOnly(UInt(14 bits), 0x38, 0) softInit (0)
      val FRT = ctrl.createReadOnly(Bool(), 0x38, 31) softInit (False)
    }

    val hcFmNumber = new Area {
      val FN = ctrl.createReadOnly(UInt(16 bits), 0x3C, 0) addTag(SimPublic) softInit (0)
      val overflow = Reg(Bool()) init(False)
      val FNp1 = FN + 1
    }

    val hcPeriodicStart = new Area {
      val PS = ctrl.createReadAndWrite(UInt(14 bits), 0x40, 0) init (0)
    }

    val hcLSThreshold = new Area {
      val LST = ctrl.createReadAndWrite(UInt(12 bits), 0x44, 0) softInit (0x0628)
      val hit = hcFmRemaining.FR < LST
    }

    val hcRhDescriptorA = new Area {
      val NDP = ctrl.read(U(p.portCount, 8 bits), 0x48, 0)
      val PSM = ctrl.createReadAndWrite(Bool(), 0x48, 8) softInit (p.powerSwitchingMode)
      val NPS = ctrl.createReadAndWrite(Bool(), 0x48, 9) softInit (p.noPowerSwitching)
      val OCPM = ctrl.createReadAndWrite(Bool(), 0x48, 11) softInit (p.powerSwitchingMode)
      val NOCP = ctrl.createReadAndWrite(Bool(), 0x48, 12) softInit (p.noOverCurrentProtection)
      val POTPGT = ctrl.createReadAndWrite(UInt(8 bits), 0x48, 24) softInit (p.powerOnToPowerGoodTime)
    }

    val hcRhDescriptorB = new Area {
      val DR = ctrl.createReadAndWrite(Bits(p.portCount bits), 0x4C, 1) softInit (Cat(p.portsConfig.map(!_.removable).map(Bool(_))))
      val PPCM = ctrl.createReadAndWrite(Bits(p.portCount bits), 0x4C, 17) softInit (Cat(p.portsConfig.map(_.powerControlMask).map(Bool(_))))
    }

    val hcRhStatus = new Area { //TODO events to RHSC
      val OCI = ctrl.read(io.phy.overcurrent, 0x50, 1)
      val DRWE = ctrl.createReadOnly(Bool(), 0x50, 15) softInit (False)
      val CCIC = ctrl.createReadAndClearOnSet(Bool(), 0x50, 17) softInit (False) setWhen (OCI.edge(False))

      val clearGlobalPower = ctrl.setOnSet(False, 0x50, 0)
      val setRemoteWakeupEnable = ctrl.setOnSet(False, 0x50, 15)
      val setGlobalPower = ctrl.setOnSet(False, 0x50, 16)
      val clearRemoteWakeupEnable = ctrl.setOnSet(False, 0x50, 31)

      DRWE setWhen (setRemoteWakeupEnable) clearWhen (clearRemoteWakeupEnable)
    }

    val hcRhPortStatus = for (portId <- 0 to p.portCount - 1) yield new Area {

      import hcRhDescriptorB._

      val id = portId + 1
      val port = io.phy.ports(portId)
      val address = 0x54 + portId * 4

      def change(bitId: Int) = new Area {
        val set = Bool()
        val clear = False
        val reg = ctrl.createReadOnly(Bool(), address, bitId) softInit (False) clearWhen (clear) setWhen (set)
        hcInterrupt.RHSC.status setWhen(set)
        ctrl.setOnSet(clear, address, bitId)
      }

      val clearPortEnable = ctrl.setOnSet(False, address, 0)
      val setPortEnable = ctrl.setOnSet(False, address, 1)
      val setPortSuspend = ctrl.setOnSet(False, address, 2)
      val clearSuspendStatus = ctrl.setOnSet(False, address, 3)
      val setPortReset = ctrl.setOnSet(False, address, 4)
      val setPortPower = ctrl.setOnSet(False, address, 8)
      val clearPortPower = ctrl.setOnSet(False, address, 9)

      val resume, reset, suspend = Reg(Bool()) softInit(False)

      val connected = RegInit(False) setWhen (port.connect) clearWhen (port.disconnect)
      val PSS = ctrl.createReadOnly(Bool(), address, 2) softInit (False)
      val PPS = ctrl.createReadOnly(Bool(), address, 8) softInit (False)
      val CCS = ctrl.read((connected || DR(portId)) && PPS, address, 0) //MAYBUG DR => always one ???
      val PES = ctrl.createReadOnly(Bool(), address, 1) softInit  (False)
      val POCI = ctrl.read(port.overcurrent, address, 3)
      val PRS = ctrl.read(reset, address, 4)
      val LSDA = ctrl.read(port.lowSpeed, address, 9)

      val CSC = change(16)
      val PESC = change(17)
      val PSSC = change(18)
      val OCIC = change(19)
      val PRSC = change(20)

      PES clearWhen (clearPortEnable || PESC.set || !PPS) setWhen (PRSC.set || PSSC.set) setWhen (setPortEnable && CCS)
      PSS clearWhen (PSSC.set || PRSC.set || !PPS || hcControl.HCFS === MainState.RESUME) setWhen (setPortSuspend && CCS)
      suspend setWhen (setPortSuspend && CCS)
      resume setWhen (clearSuspendStatus && PSS)
      reset setWhen (setPortReset && CCS)

      when(hcRhDescriptorA.NPS) {
        PPS := True
      } otherwise {
        when(hcRhDescriptorA.PSM) {
          when(hcRhDescriptorB.PPCM(portId)) {
            PPS clearWhen (clearPortPower) setWhen (setPortPower)
          } otherwise {
            PPS clearWhen (hcRhStatus.clearGlobalPower) setWhen (hcRhStatus.setGlobalPower)
          }
        } otherwise {
          PPS clearWhen (hcRhStatus.clearGlobalPower) setWhen (hcRhStatus.setGlobalPower)
        }
      }

      if (!p.noOverCurrentProtection) {
        if (p.powerSwitchingMode) PPS clearWhen (port.overcurrent)
        if (!p.powerSwitchingMode) PPS clearWhen (io.phy.overcurrent)
      }

      CSC.set := CCS.edge(False) || (setPortEnable && !CCS) || (setPortSuspend && !CCS) || (setPortReset && !CCS)
      PESC.set := port.overcurrent
      PSSC.set := port.suspend.fire || port.remoteResume
      OCIC.set := POCI
      PRSC.set := port.reset.fire

      PSSC.clear setWhen (PRSC.set)

      //      port.enable := PES
      port.disable.valid := clearPortEnable
      port.removable := DR(portId)
      port.power := PPS //PPCM(portId)
      port.resume.valid := resume
      resume clearWhen (port.resume.fire)
      port.reset.valid := reset
      reset clearWhen (port.reset.fire)
      port.suspend.valid := suspend
      suspend.clearWhen(port.suspend.fire)


    }
  }

  // Track the progress of the current frame (1 ms)
  val frame = new Area {
    val run = False
    val reload = False
    val overflow = reg.hcFmRemaining.FR === 0
    val tick = False
    val section1 = reg.hcFmRemaining.FR > reg.hcPeriodicStart.PS
    val limitCounter = Reg(UInt(15 bits))
    val limitHit = limitCounter === 0
    val decrementTimer = Reg(UInt(3 bits))


    val decrementTimerOverflow = decrementTimer === 6
    decrementTimer := decrementTimer + 1
    when(decrementTimerOverflow) {
      decrementTimer := 0
    }

    when(run && io.phy.tick) {
      reg.hcFmRemaining.FR := reg.hcFmRemaining.FR - 1
      when(!limitHit && !decrementTimerOverflow) {
        limitCounter := limitCounter - 1
      }
      when(overflow) {
        tick := True
        reload := True
      }
    }
    when(reload) {
      reg.hcFmRemaining.FR := reg.hcFmInterval.FI
      reg.hcFmRemaining.FRT := reg.hcFmInterval.FIT
      reg.hcFmNumber.FN := reg.hcFmNumber.FNp1
      reg.hcFmNumber.overflow setWhen(reg.hcFmNumber.FNp1.msb ^ reg.hcFmNumber.FN.msb)
      limitCounter := reg.hcFmInterval.FSMPS
      decrementTimer := 0
    }
  }

  // FSM to emit tocken
  val token = new UsbTokenTxFsm(tx = io.phy.tx,
    eop = io.phy.txEop) {
    always{
      when(unscheduleAll.fire){ killFsm() }
    }
  }

  // FSM to emit a data packet (from dma)
  val dataTx = new UsbDataTxFsm(tx = io.phy.tx,
                                eop = io.phy.txEop) {
    always{
      when(unscheduleAll.fire){ killFsm() }
    }
  }

  val rxTimer = new Area {
    val lowSpeed = Bool()
    val counter = Reg(UInt(log2Up(24*8) bits))
    val clear = False
    when(io.phy.tick) {
      counter := counter + 1
    }
    when(clear) {
      counter := 0
    }

    def cycles(c: Int): Bool = {
      val ls = c * 8 - 1
      val fs = c - 1
      counter === (lowSpeed ? U(ls) | U(fs))
    }

    val rxTimeout = cycles(24)
    val ackTx = cycles(2)
    clear setWhen(io.phy.rx.active)
  }

  val rxPidOk = io.phy.rx.flow.data(3 downto 0) === ~io.phy.rx.flow.data(7 downto 4)


  val dataRx = new UsbDataRxFsm(
    rx       = io.phy.rx.flow.translateWith(io.phy.rx.flow.data),
    rxActive     = io.phy.rx.active,
    rxStuffing   = io.phy.rx.flow.valid && io.phy.rx.flow.stuffingError,
    timeoutClear = rxTimer.clear,
    timeoutEvent = rxTimer.rxTimeout
  ){
    always{
      when(unscheduleAll.fire){ killFsm() }
    }
  }

  val sof = new StateMachineSlave {
    val FRAME_TX, FRAME_NUMBER_CMD, FRAME_NUMBER_RSP = new State
    setEntry(FRAME_TX)

    val doInterruptDelay = Reg(Bool())

    onStart {
      token.startFsm()
    }
    FRAME_TX.whenIsActive {
      token.data := reg.hcFmNumber.FN.asBits.resized
      token.pid := 5
      doInterruptDelay := interruptDelay.done && !reg.hcInterrupt.WDH.status
      when(token.wantExit) {
        goto(FRAME_NUMBER_CMD)
      }
    }
    FRAME_NUMBER_CMD.whenIsActive {
      ioDma.cmd.valid := True
      ioDma.cmd.address := reg.hcHCCA.HCCA.address | 0x80
      ioDma.cmd.length := 7
      ioDma.cmd.last := dmaWriteCtx.counter === 7 * 8 / p.dataWidth
      ioDma.cmd.setWrite()
      dmaWriteCtx.save(U"x0000" @@ reg.hcFmNumber.FN, 0, 0)
      when(doInterruptDelay) {
        dmaWriteCtx.save(reg.hcDoneHead.DH.address(31 downto 1) ## reg.hcInterrupt.unmaskedPending, 1, 0)
      }
      when(ioDma.cmd.ready && ioDma.cmd.last) {
        goto(FRAME_NUMBER_RSP)
      }
    }

    FRAME_NUMBER_RSP.whenIsActive {
      when(ioDma.rsp.valid) {
        reg.hcInterrupt.SF.status := True
        reg.hcInterrupt.FNO.status setWhen(reg.hcFmNumber.overflow)
        reg.hcFmNumber.overflow := False
        interruptDelay.tick := True
        when(doInterruptDelay) {
          reg.hcInterrupt.WDH.status := True
          reg.hcDoneHead.DH.reg := 0
          interruptDelay.disable := True
        }
        exitFsm()
      }
    }

    always{
      when(unscheduleAll.fire){ killFsm() }
    }
  }

  val FlowType = new SpinalEnum {
    val BULK, CONTROL, PERIODIC = newElement()
  }

  val priority = new Area {
    val bulk = Reg(Bool())
    val counter = Reg(UInt(2 bits))

    val tick = False
    val skip = False
    when(tick) {
      counter := counter + 1
      when(bulk || counter === reg.hcControl.CBSR) {
        skip := True
      }
    }

    when(skip) {
      bulk := !bulk
      counter := 0
    }
  }

  val interruptDelay = new Area {
    val counter = RegInit(U"111")
    val tick = False
    val done = counter === 0
    val disabled = counter === 7
    val disable = False

    val load = Flow(UInt(3 bits))
    load.valid := False
    load.payload.assignDontCare()

    when(tick && !done && !disabled) {
      counter := counter - 1
    }

    when(load.valid && load.payload < counter) {
      counter := load.payload
    }

    when(disable) {
      counter := 7
    }
  }


  val endpoint = new StateMachineSlave {
    val ED_READ_CMD, ED_READ_RSP, ED_ANALYSE = new State
    val TD_READ_CMD, TD_READ_RSP, TD_READ_DELAY, TD_ANALYSE, TD_CHECK_TIME = new State
    val BUFFER_READ = new State
    val TOKEN = new State
    val DATA_TX, DATA_RX, DATA_RX_VALIDATE = new State
    val ACK_RX, ACK_TX_0, ACK_TX_1, ACK_TX_EOP = new State
    val DATA_RX_WAIT_DMA = new State
    val UPDATE_TD_PROCESS, UPDATE_TD_CMD = new State
    val UPDATE_ED_CMD, UPDATE_SYNC = new State
    val ABORD = new State

    setEntry(ED_READ_CMD)

    always{
      when(unscheduleAll.fire){ killFsm() }
    }

    val flowType = Reg(FlowType())

    val Status = new SpinalEnum {
      val OK, FRAME_TIME = newElement()
    }
    val status = Reg(Status)
    val dataPhase = Reg(Bool())

    val ED = new Area {
      val address = Reg(UInt(32 bits))

      val words = Vec(Reg(Bits(32 bits)), 4)

      val FA = words(0)(0, 7 bits)
      val EN = words(0)(7, 4 bits)
      val D = words(0)(11, 2 bits)
      val S = words(0)(13)
      val K = words(0)(14)
      val F = words(0)(15)
      val MPS = words(0)(16, 11 bits).asUInt
      val tailP = words(1)(4, 28 bits).asUInt
      val H = words(2)(0)
      val C = words(2)(1)
      val headP = words(2)(4, 28 bits).asUInt
      val nextED = words(3)(4, 28 bits).asUInt

      val tdEmpty = tailP === headP
      val isFs = !S
      val isLs = S
      val isoOut = D(0)

      def isIsochrone = F

      when(isStarted) {
        io.phy.lowSpeed := S
      }

      rxTimer.lowSpeed := S
    }

    val TD = new Area {
      assert(p.dataWidth <= 128)
      val address = ED.headP << 4

      val words = Vec(Reg(Bits(32 bits)), 4)

      val CC = words(0)(28, 4 bits) //4.3.3.1 Condition Code Description
      val EC = words(0)(26, 2 bits)
      val T = words(0)(24, 2 bits)
      val DI = words(0)(21, 3 bits).asUInt
      val DP = words(0)(19, 2 bits)
      val R = words(0)(18)

      val CBP = words(1)(0, 32 bits).asUInt
      val nextTD = words(2)(4, 28 bits)
      val BE = words(3)(0, 32 bits).asUInt

      //Isochrone specifics
      val FC = words(0)(24, 3 bits).asUInt
      val SF = words(0)(0, 16 bits).asUInt

      val isoRelativeFrameNumber = reg.hcFmNumber.FN - SF
      val tooEarly = isoRelativeFrameNumber.msb
      val isoFrameNumber = isoRelativeFrameNumber(FC.range)
      val isoOverrun = !tooEarly && isoRelativeFrameNumber > FC
      val isoOverrunReg = RegNext(isoOverrun)
      val isoLast = !isoOverrun && !tooEarly && isoFrameNumber === FC
      val isoBase, isoBaseNext = Reg(UInt(13 bits))
      val isoZero = RegNext(isoLast ? (isoBase > isoBaseNext) | (isoBase === isoBaseNext))

      val isoLastReg = RegNext(isoLast)
      val tooEarlyReg = RegNext(tooEarly)

      val isSinglePage = CBP(12, 20 bits) === BE(12, 20 bits)
      val firstOffset = ED.isIsochrone ? isoBase | CBP(0, 12 bits)
      val lastOffset = RegNext(ED.isIsochrone ? (isoBaseNext-U(!isoLast)) | (U(!isSinglePage) @@ BE(0, 12 bits)))

      val allowRounding = !ED.isIsochrone && R

      val retire = Reg(Bool())
      val upateCBP = Reg(Bool())
      val noUpdate = Reg(Bool())


      val dataPhaseUpdate = Reg(Bool())
      val TNext = dataPhaseUpdate ? (True ## (!dataPhase)) | T
      val dataPhaseNext = dataPhase ^ dataPhaseUpdate
      val dataPid = dataPhase ? B(UsbPid.DATA1) | B(UsbPid.DATA0)
      val dataPidWrong = dataPhase ? B(UsbPid.DATA0) | B(UsbPid.DATA1)

      val clear = False
      when(clear){
        retire := False
        dataPhaseUpdate := False
        upateCBP := False
        noUpdate := False
      }
    }

    val tockenType = (ED.D(0) =/= ED.D(1)) ? ED.D | TD.DP
    val isIn = tockenType === 2

    val applyNextED = False
    when(applyNextED) {
      switch(flowType) {
        is(FlowType.BULK) {
          reg.hcBulkCurrentED.BCED.reg := ED.nextED
        }
        is(FlowType.CONTROL) {
          reg.hcControlCurrentED.CCED.reg := ED.nextED
        }
        is(FlowType.PERIODIC) {
          reg.hcPeriodCurrentED.PCED.reg := ED.nextED
        }
      }
    }

    onStart {
      status := Status.OK
    }

    ED_READ_CMD.whenIsActive {
      ioDma.cmd.valid := True
      ioDma.cmd.address := ED.address
      ioDma.cmd.length := 15
      ioDma.cmd.last := True
      ioDma.cmd.setRead()
      when(ioDma.cmd.ready) {
        goto(ED_READ_RSP)
      }
    }

    ED_READ_RSP.whenIsActive {
      dmaReadCtx.load(ED.words(0), 0, 0)
      dmaReadCtx.load(ED.words(1), 1, 0)
      dmaReadCtx.load(ED.words(2), 2, 0)
      dmaReadCtx.load(ED.words(3), 3, 0)

      when(ioDma.rsp.valid && ioDma.rsp.last) {
        goto(ED_ANALYSE)
      }
    }

    ED_ANALYSE.whenIsActive {
      when(ED.H || ED.K || ED.tdEmpty) { //Skip
        applyNextED := True
        exitFsm()
      } otherwise {
        goto(TD_READ_CMD)
      }
    }

    TD_READ_CMD.whenIsActive {
      TD.clear := True

      //Fetch TD
      ioDma.cmd.valid := True
      ioDma.cmd.address := TD.address
      ioDma.cmd.length := (ED.isIsochrone ? U(31) | U(15)).resized
      ioDma.cmd.last := True
      ioDma.cmd.setRead()
      when(ioDma.cmd.ready) {
        goto(TD_READ_RSP)
      }
    }

    TD_READ_RSP whenIsActive {
      dmaReadCtx.load(TD.words(0), 0, 0)
      dmaReadCtx.load(TD.words(1), 1, 0)
      dmaReadCtx.load(TD.words(2), 2, 0)
      dmaReadCtx.load(TD.words(3), 3, 0)

      //Manage isochrone address loading
      for(i <- 0 until 8){
        when(TD.isoFrameNumber === i) {
          dmaReadCtx.load(TD.isoBase, 4+i/2, (i%2)*16)
          if(i != 7) dmaReadCtx.load(TD.isoBaseNext, 4+(i+1)/2, ((i+1)%2)*16)
        }
      }
      when(TD.isoLast){ TD.isoBaseNext := U(!TD.isSinglePage ## TD.BE(11 downto 0)) }

      when(ioDma.rsp.lastFire) {
        goto(TD_READ_DELAY)
      }
    }

    TD_READ_DELAY whenIsActive{
      goto(TD_ANALYSE)
    }

    val currentAddress = Reg(UInt(14 bits)) //One extra bit to allow overflow comparison
    val currentAddressFull = (currentAddress(12) ? TD.BE(31 downto 12) | TD.CBP(31 downto 12)) @@ currentAddress(11 downto 0)
    val currentAddressBmb = currentAddressFull & U(currentAddressFull.getWidth bits, default -> true, io.dma.p.access.wordRange -> false)
    val lastAddress = Reg(UInt(13 bits))
    val transactionSizeMinusOne = lastAddress - currentAddress
    val transactionSize = transactionSizeMinusOne + 1
    val zeroLength = Reg(Bool())
    val dataDone = zeroLength || currentAddress > lastAddress

    val dmaLogic = new StateMachine {
      val INIT, TO_USB, FROM_USB, VALIDATION, CALC_CMD, READ_CMD, WRITE_CMD = new State
      setEntry(INIT)
      disableAutoStart()

      always{
        when(unscheduleAll.fire){ killFsm() }
      }

      val validated = False
      val length = Reg(UInt(p.dmaLengthWidth bits))
      val lengthMax = ~currentAddress.resize(p.dmaLengthWidth)
      val lengthCalc = transactionSizeMinusOne.min(lengthMax).resize(widthOf(lengthMax))
      val beatCount = Bmb.transferBeatCountMinusOneBytesAligned(currentAddressFull, length, io.dma.p)
      val lengthBmb = (beatCount @@ U(io.dma.p.access.byteCount-1)).resize(p.dmaLengthWidth)

      val fromUsbCounter = Reg(UInt(11 bits))
      val overflow = Reg(Bool())
      val underflow = Reg(Bool())
      val underflowError = underflow && !TD.allowRounding

      // Implement internal buffer write
      when(isStarted && !isIn && ioDma.rsp.valid) {
        fifo.io.push.valid := True
        fifo.io.push.payload := ioDma.rsp.data
      }

      val selWidth = log2Up(p.dataWidth / 8)
      val byteCtx = new Area{
        val counter = Reg(UInt(13 bits))
        val last = counter === lastAddress
        val sel = counter.resize(selWidth bits)
        val increment = False
        when(increment){
          counter := counter + 1
        }
      }

      INIT whenIsActive {
        underflow := False
        overflow := False
        fifo.io.flush := True
        when(isIn) {
          goto(FROM_USB)
        } otherwise {
          goto(CALC_CMD)
        }
      }

      // Implement ioDma.cmd
      CALC_CMD whenIsActive {
        length := lengthCalc
        when(dataDone) {
          when(isIn){
            exitFsm()
          } otherwise {
            when(dmaCtx.pendingEmpty) {
              goto(TO_USB)
            }
          }
        }otherwise {
          val checkShift = log2Up(p.dmaLength * 8 / p.dataWidth)
          when(isIn){
            goto(WRITE_CMD)
          } otherwise {
            goto(READ_CMD)
          }
        }
      }

      READ_CMD whenIsActive {
        ioDma.cmd.last := True
        ioDma.cmd.setRead()
        ioDma.cmd.address := currentAddressBmb
        ioDma.cmd.length := lengthBmb
        ioDma.cmd.valid := True
        when(ioDma.cmd.ready) {
          currentAddress := currentAddress + length + 1
          goto(CALC_CMD)
        }
      }

      val headMask = (0 until (1 << selWidth)).map(_ >= currentAddress(0, selWidth bits)).asBits
      val lastMask = (0 until (1 << selWidth)).map(_ <= (currentAddress + length)(0, selWidth bits)).asBits
      val fullMask = B((1 << (1 << selWidth))-1)
      val beatLast = dmaCtx.beatCounter === beatCount
      WRITE_CMD whenIsActive{
        ioDma.cmd.last := beatLast
        ioDma.cmd.setWrite()
        ioDma.cmd.address := currentAddressBmb
        ioDma.cmd.length := lengthBmb
        ioDma.cmd.valid := True
        ioDma.cmd.data := fifo.io.pop.payload
        ioDma.cmd.mask := fullMask & (ioDma.cmd.first ? headMask | fullMask) & (ioDma.cmd.last ? lastMask | fullMask)
        when(ioDma.cmd.ready) {
          fifo.io.pop.ready := True
          when(beatLast) {
            currentAddress := currentAddress + length + 1
            goto(CALC_CMD)
          }
        }
      }

      TO_USB whenIsActive{
        dataTx.data.fragment := fifo.io.pop.payload.subdivideIn(8 bits).read(byteCtx.sel)
        dataTx.data.last := byteCtx.last
        dataTx.data.valid := True
        when(dataTx.data.ready) {
          byteCtx.increment := True
          when(byteCtx.sel.andR){
            fifo.io.pop.ready := True
          }
          when(byteCtx.last) {
            exitFsm()
          }
        }
      }

      val buffer = Reg(Bits(p.dataWidth bits))
      val push = RegNext(False) init(False)
      when(push){
        fifo.io.push.valid := True
        fifo.io.push.payload := buffer
      }

      FROM_USB onEntry {
        fromUsbCounter := 0
      }
      FROM_USB whenIsActive{
        when(dataRx.wantExit){
          push := byteCtx.sel.orR
          val u = fromUsbCounter < transactionSize
          underflow := u
          overflow  := !u && fromUsbCounter =/= transactionSize
          when(zeroLength){
            underflow := False
            overflow := fromUsbCounter =/= 0
          }
          when(u){
            lastAddress := (currentAddress + fromUsbCounter - 1).resized
          }
          goto(VALIDATION)
        }
        when(dataRx.data.valid) {
          fromUsbCounter := fromUsbCounter + U(!fromUsbCounter.msb)
          byteCtx.increment := True
          buffer.subdivideIn(8 bits)(byteCtx.sel) := dataRx.data.payload
          push setWhen (byteCtx.sel.andR)
        }
      }

      VALIDATION whenIsActive{
        when(fromUsbCounter === 0){
          exitFsm()
        } elsewhen(validated){
          goto(CALC_CMD)
        }
      }

      val fsmStopped = this.isStopped
    }


    TD_ANALYSE.whenIsActive {
      switch(flowType) {
        is(FlowType.CONTROL) {
          reg.hcCommandStatus.CLF := True
        }
        is(FlowType.BULK) {
          reg.hcCommandStatus.BLF := True
        }
      }

      dmaLogic.byteCtx.counter := TD.firstOffset.resized
      currentAddress := TD.firstOffset.resized
      lastAddress := (ED.isIsochrone ? TD.lastOffset | TD.lastOffset.min(TD.firstOffset +^ ED.MPS - 1)).resized

      zeroLength := ED.isIsochrone ?  TD.isoZero | TD.CBP === 0
      dataPhase := ED.isIsochrone ? False | (TD.T(1) ? TD.T(0) | ED.C)

      goto(TD_CHECK_TIME)

      when(ED.isIsochrone){
        when(TD.tooEarlyReg) {
          goto(UPDATE_SYNC)
        }
        when(TD.isoOverrunReg) {
          TD.retire := True
          goto(UPDATE_TD_CMD) //TODO untested yet (periodic iso schedule overrun
        }
      }
    }

    val byteCountCalc = lastAddress - currentAddress + 1
    val fsTimeCheck = zeroLength ? (frame.limitCounter === 0) | ((byteCountCalc << 3) >= frame.limitCounter)
    val timeCheck = (ED.isFs && fsTimeCheck) || (ED.isLs && reg.hcLSThreshold.hit)

    TD_CHECK_TIME whenIsActive{
      when(timeCheck) {
        status := Status.FRAME_TIME
        goto(ABORD)
      } otherwise {
        when(isIn || zeroLength){
          goto(TOKEN)
        } otherwise {
          dmaLogic.startFsm()
          goto(BUFFER_READ)
        }
      }
    }

    BUFFER_READ.whenIsActive {
      when(dmaLogic.isActive(dmaLogic.TO_USB)) {
        goto(TOKEN)
        when(timeCheck){
          status := Status.FRAME_TIME
          dmaLogic.killFsm()
          goto(ABORD)
        }
      }
    }

    ABORD whenIsActive {
      exitFsm()
    }


    TOKEN.onEntry {
      token.startFsm()
    }
    TOKEN.whenIsActive {
      token.data := ED.EN ## ED.FA
      switch(tockenType) {
        is(B"00") {
          token.pid := UsbPid.SETUP
        }
        is(B"01") {
          token.pid := UsbPid.OUT
        }
        is(B"10") {
          token.pid := UsbPid.IN
        }
      }
      when(token.wantExit) {
        when(isIn){
          goto(DATA_RX)
        } otherwise {
          goto(DATA_TX)
        }
      }
    }

    DATA_TX.onEntry {
      dataTx.startFsm()
    }
    DATA_TX.whenIsActive {
      dataTx.pid := dataPhase ## B"011"
      when(dataTx.wantExit) {
        when(ED.isIsochrone){
          TD.CC := UsbOhci.CC.noError
          goto(UPDATE_TD_PROCESS)
        } otherwise {
          goto(ACK_RX)
        }
      }
    }

    val ackRxFired = Reg(Bool())
    val ackRxActivated = Reg(Bool())
    val ackRxPidFailure = Reg(Bool())
    val ackRxStuffing = Reg(Bool())
    val ackRxPid = Reg(Bits(4 bits))
    ACK_RX.onEntry{
      ackRxFired := False
      ackRxActivated := False
      ackRxPidFailure := False
      ackRxStuffing := False
      rxTimer.clear := True
    }
    ACK_RX.whenIsActive {
      when(io.phy.rx.flow.valid){
        ackRxFired := True
        ackRxPid := io.phy.rx.flow.data(3 downto 0)
        ackRxStuffing setWhen(io.phy.rx.flow.stuffingError)
        when(!rxPidOk || ackRxFired){
          ackRxPidFailure := True
        }
      }
      ackRxActivated setWhen(io.phy.rx.active)
      when(!io.phy.rx.active && ackRxActivated){
        goto(UPDATE_TD_PROCESS)
        when(!ackRxFired) {
          TD.CC := UsbOhci.CC.pidCheckFailure
        } elsewhen(ackRxStuffing){
          TD.CC := UsbOhci.CC.bitStuffing
        } elsewhen(ackRxPidFailure) {
          TD.CC := UsbOhci.CC.pidCheckFailure
        } otherwise {
          switch(ackRxPid){
            is(UsbPid.ACK) { TD.CC := UsbOhci.CC.noError }
            is(UsbPid.NAK) { goto(UPDATE_SYNC)}
            is(UsbPid.STALL) { TD.CC := UsbOhci.CC.stall }
            default( TD.CC := UsbOhci.CC.unexpectedPid )
          }
        }
      }

      when(rxTimer.rxTimeout){
        TD.CC := UsbOhci.CC.deviceNotResponding
        goto(UPDATE_TD_PROCESS)
      }
    }

    DATA_RX.onEntry{
      dataRx.startFsm()
      dmaLogic.startFsm()
    }
    DATA_RX.whenIsActive {
      when(dataRx.wantExit) {
        goto(DATA_RX_VALIDATE)
      }
    }

    DATA_RX_VALIDATE whenIsActive{
      dmaLogic.validated := True

      goto(DATA_RX_WAIT_DMA)
      TD.CC := UsbOhci.CC.noError
      when(dataRx.notResponding) {
        TD.CC := UsbOhci.CC.deviceNotResponding
      } elsewhen(dataRx.stuffingError) {
        TD.CC := UsbOhci.CC.bitStuffing
      } elsewhen(dataRx.pidError){
        TD.CC := UsbOhci.CC.pidCheckFailure
      } otherwise {
        val pidOk = False
        when(ED.isIsochrone){
          switch(dataRx.pid){
            is(UsbPid.STALL, UsbPid.NAK) { TD.CC := UsbOhci.CC.stall } //Do not totaly follow the spec later for side effects
            is(UsbPid.DATA0, UsbPid.DATA1){ pidOk := True }
            default{TD.CC := UsbOhci.CC.unexpectedPid }
          }
        } otherwise {
          switch(dataRx.pid){
            is(UsbPid.NAK) { TD.noUpdate := True }
            is(UsbPid.STALL) { TD.CC := UsbOhci.CC.stall }
            is(UsbPid.DATA0, UsbPid.DATA1){
              when(dataRx.pid === TD.dataPidWrong) {
                TD.CC := UsbOhci.CC.dataToggleMismatch
                goto(ACK_TX_0)
              } otherwise {
                pidOk := True
              }
            }
            default{TD.CC := UsbOhci.CC.unexpectedPid }
          }
        }

        when(pidOk){
          when(dataRx.crcError){
            TD.CC := UsbOhci.CC.crc
          } otherwise{
            when(dmaLogic.underflowError){
              TD.CC := UsbOhci.CC.dataUnderrun
            } elsewhen(dmaLogic.overflow){
              TD.CC := UsbOhci.CC.dataOverrun
            }
            when(!ED.isIsochrone) {
              goto(ACK_TX_0)
            }
          }
        }
      }
    }

    ACK_TX_0 whenIsActive {
      when(rxTimer.ackTx){
        goto(ACK_TX_1)
      }
    }
    ACK_TX_1 whenIsActive{
      io.phy.tx.valid := True
      io.phy.tx.last := True
      io.phy.tx.fragment := UsbPid.token(UsbPid.ACK)
      when(io.phy.tx.ready){
        goto(ACK_TX_EOP)
      }
    }
    ACK_TX_EOP whenIsActive{
      when(io.phy.txEop) {
        goto(DATA_RX_WAIT_DMA)
      }
    }

    DATA_RX_WAIT_DMA whenIsActive{
      when(dmaLogic.isStopped){
        goto(UPDATE_TD_PROCESS)
      }
    }

    val tdUpdateAddress = (TD.retire && !(isIn && (TD.CC === UsbOhci.CC.noError || TD.CC === UsbOhci.CC.dataUnderrun) && dmaLogic.underflow)) ? U(0) | currentAddressFull
//    val tdUpdateAddress = TD.retire ? U(0) | currentAddressFull
    UPDATE_TD_PROCESS whenIsActive{
      import UsbOhci.CC._

      goto(UPDATE_TD_CMD)
      when(ED.isIsochrone) {
        TD.retire setWhen(TD.isoLastReg)
      } otherwise  {
        TD.EC := 0
        switch(TD.CC) { //Include stall
          default {
            TD.retire := True
          }
          is(noError) {
            TD.retire setWhen (dmaLogic.underflow || currentAddress > TD.lastOffset || zeroLength)
            TD.dataPhaseUpdate := True
            TD.upateCBP := True
          }
          is(dataUnderrun){
            TD.retire := True
            TD.dataPhaseUpdate := True
            TD.upateCBP := True
          }
          is(dataOverrun){
            TD.retire := True
            TD.dataPhaseUpdate := True
          }
          is(bitStuffing, crc, pidCheckFailure, deviceNotResponding, unexpectedPid, dataToggleMismatch) { //Transmission errors => may repeat
            TD.EC := (TD.EC.asUInt + 1).asBits
            when(TD.EC =/= 2) {
              TD.CC := UsbOhci.CC.noError
            } otherwise {
              TD.retire := True
            }
          }
        }

        when(TD.noUpdate) {
          goto(UPDATE_SYNC)
          TD.retire := False
        }
      }
    }

    UPDATE_TD_CMD.whenIsActive {
      ioDma.cmd.valid := True
      ioDma.cmd.address := TD.address
      ioDma.cmd.length := (ED.F ? U(31) | U(15)).resized
      ioDma.cmd.last := dmaWriteCtx.counter === (ED.isIsochrone ? U(31 * 8 / p.dataWidth) | U(15 * 8 / p.dataWidth))
      ioDma.cmd.setWrite()

      when(ED.isIsochrone){
        when(TD.isoOverrunReg) {
          dmaWriteCtx.save(U(UsbOhci.CC.dataOverrun, 4 bits) ## TD.words(0)(27) ## TD.FC, 0, 24)
        } otherwise {
          when(TD.isoLastReg) {
            dmaWriteCtx.save(U(UsbOhci.CC.noError, 4 bits) ## TD.words(0)(27) ## TD.FC, 0, 24)
          }
          val count = (ED.isoOut ? U(0) | currentAddress - TD.isoBase).resize(12 bits)
          val PSW = TD.CC ## count
          for(i <- 0 until 8){
            when(TD.isoFrameNumber === i) {
              dmaWriteCtx.save(PSW, 4 + i / 2, (i % 2) * 16)
            }
          }
        }
      } otherwise {
        dmaWriteCtx.save(TD.CC ## TD.EC ## TD.TNext, 0, 24)
        when(TD.upateCBP) {
          dmaWriteCtx.save(tdUpdateAddress, 1, 0)
        }
      }

      when(TD.retire) {
        dmaWriteCtx.save(reg.hcDoneHead.DH.address, 2, 0)
      }
      when(ioDma.cmd.ready && ioDma.cmd.last) {
        goto(UPDATE_ED_CMD)
      }
      ED.H := !ED.isIsochrone && TD.CC =/= UsbOhci.CC.noError
    }

    UPDATE_ED_CMD.whenIsActive {
      ioDma.cmd.valid := True
      ioDma.cmd.address := ED.address
      ioDma.cmd.length := 15
      ioDma.cmd.last := dmaWriteCtx.counter === 15 * 8 / p.dataWidth
      ioDma.cmd.setWrite()
      when(TD.retire) {
        dmaWriteCtx.save(TD.nextTD ## B"00" ## TD.dataPhaseNext ## ED.H, 2, 0)
      }
      when(ioDma.cmd.ready && ioDma.cmd.last) {
        goto(UPDATE_SYNC)
      }
    }

    UPDATE_SYNC.whenIsActive {
      when(dmaCtx.pendingEmpty) {
        when(!(ED.isIsochrone && TD.isoOverrunReg)) {
          applyNextED := True
        }
        when(flowType =/= FlowType.PERIODIC){
          priority.tick := True
        }

        when(TD.retire) {
          interruptDelay.load.valid := True
          interruptDelay.load.payload := TD.DI
          reg.hcDoneHead.DH.reg := ED.headP
        }
        exitFsm()
      }
    }
  }


  val operational = new StateMachineSlave {
    val SOF, ARBITER, END_POINT, PERIODIC_HEAD_CMD, PERIODIC_HEAD_RSP, WAIT_SOF = new State
    setEntry(WAIT_SOF)


    val periodicHeadFetched = Reg(Bool())
    val periodicDone = Reg(Bool())

    val allowBulk = Reg(Bool())
    val allowControl = Reg(Bool())
    val allowPeriodic = Reg(Bool())
    val allowIsochronous = Reg(Bool())
    val askExit = False

    always{
      when(unscheduleAll.fire){ killFsm() }
    }

    onStart {
      allowPeriodic := False // Avoid overrun false positive trigger
      interruptDelay.disable := True
    }

    SOF.onEntry {
      sof.startFsm()
    }
    SOF.whenIsActive {
      when(sof.wantExit) {
        when(allowPeriodic && !periodicDone) { //Overrun condition
          reg.hcInterrupt.SO.status := True
          reg.hcCommandStatus.SOC := reg.hcCommandStatus.SOC + 1
        }

        allowBulk := reg.hcControl.BLE
        allowControl := reg.hcControl.CLE
        allowPeriodic := reg.hcControl.PLE
        allowIsochronous := reg.hcControl.IE

        periodicDone := False
        periodicHeadFetched := False
        priority.bulk := False
        priority.counter := 0
        goto(ARBITER)
      }
    }

    ARBITER.whenIsActive {
      allowBulk setWhen (reg.hcControl.BLE)
      allowControl setWhen (reg.hcControl.CLE)

      when(askExit){
        exitFsm()
      } elsewhen(frame.limitHit) {
        goto(WAIT_SOF)
      } elsewhen (allowPeriodic && !periodicDone && !frame.section1) {
        when(!periodicHeadFetched) {
          goto(PERIODIC_HEAD_CMD)
        } otherwise {
          when(reg.hcPeriodCurrentED.isZero) {
            periodicDone := True
          } otherwise {
            endpoint.flowType := FlowType.PERIODIC
            endpoint.ED.address := reg.hcPeriodCurrentED.PCED.address
            endpoint.startFsm()
            goto(END_POINT)
          }
        }
      } otherwise {
        priority.skip := True
        when(priority.bulk) {
          when(allowBulk) {
            when(reg.hcBulkCurrentED.isZero) {
              when(reg.hcCommandStatus.BLF) {
                reg.hcBulkCurrentED.BCED.reg := reg.hcBulkHeadED.BHED.reg
                reg.hcCommandStatus.BLF := False
                priority.skip := False
              }
            } otherwise {
              endpoint.flowType := FlowType.BULK
              endpoint.ED.address := reg.hcBulkCurrentED.BCED.address
              endpoint.startFsm()
              priority.skip := False
              goto(END_POINT)
            }
          }
        } otherwise {
          when(allowControl) {
            when(reg.hcControlCurrentED.isZero) {
              when(reg.hcCommandStatus.CLF) {
                reg.hcControlCurrentED.CCED.reg := reg.hcControlHeadED.CHED.reg
                reg.hcCommandStatus.CLF := False
                priority.skip := False
              }
            } otherwise {
              endpoint.flowType := FlowType.CONTROL
              endpoint.ED.address := reg.hcControlCurrentED.CCED.address
              endpoint.startFsm()
              priority.skip := False
              goto(END_POINT)
            }
          }
        }
      }
    }

    END_POINT.whenIsActive {
      when(endpoint.wantExit) {
        switch(endpoint.status) {
          is(endpoint.Status.OK) {
            goto(ARBITER)
          }
          is(endpoint.Status.FRAME_TIME) {
            goto(WAIT_SOF)
          }
        }
      }
    }

    PERIODIC_HEAD_CMD.whenIsActive {
      ioDma.cmd.valid := True
      ioDma.cmd.address := reg.hcHCCA.HCCA.address | (reg.hcFmNumber.FN(4 downto 0) << 2).resized
      ioDma.cmd.length := 3
      ioDma.cmd.last := True
      ioDma.cmd.setRead()
      when(ioDma.cmd.ready) {
        goto(PERIODIC_HEAD_RSP)
      }
    }

    dmaRspMux.sel := reg.hcFmNumber.FN.resized
    PERIODIC_HEAD_RSP.whenIsActive {
      when(ioDma.rsp.valid) {
        periodicHeadFetched := True
        reg.hcPeriodCurrentED.PCED.load(dmaRspMux.data)
        goto(ARBITER)
      }
    }


    WAIT_SOF.whenIsActive {
      when(frame.tick) {
        goto(SOF)
      }
    }
  }

  val hc = new StateMachine {
    val RESET, RESUME, OPERATIONAL, SUSPEND, ANY_TO_RESET, ANY_TO_SUSPEND = new State
    setEntry(RESET)

    reg.hcControl.HCFS := MainState.RESET


    io.phy.usbReset   := reg.hcControl.HCFS === MainState.RESET
    io.phy.usbResume  := reg.hcControl.HCFS === MainState.RESUME

    val error = False
    RESET.whenIsActive {
      when(reg.hcControl.HCFSWrite.valid) {
        switch(reg.hcControl.HCFSWrite.payload) {
          is(MainState.OPERATIONAL) {
            goto(OPERATIONAL)
          }
          default {
            error := True
          }
        }
      }
    }


    OPERATIONAL.onEntry {
      operational.startFsm()
      frame.reload := True
    }
    OPERATIONAL.whenIsActive {
      reg.hcControl.HCFS := MainState.OPERATIONAL
      frame.run := True
    }


    RESUME.whenIsActive {
      reg.hcControl.HCFS := MainState.RESUME
      when(reg.hcControl.HCFSWrite.valid && reg.hcControl.HCFSWrite.payload === MainState.OPERATIONAL) {
        goto(OPERATIONAL)
      }
    }


    SUSPEND.whenIsActive {
      reg.hcControl.HCFS := MainState.SUSPEND

      when(reg.hcRhStatus.DRWE && reg.hcRhPortStatus.map(_.CSC.reg).orR) {
        reg.hcInterrupt.RD.status := True
        goto(RESUME)
      } elsewhen(reg.hcControl.HCFSWrite.valid && reg.hcControl.HCFSWrite.payload === MainState.OPERATIONAL) {
        goto(OPERATIONAL)
      }
    }

    ANY_TO_RESET onEntry{
      doUnschedule := True
    }
    ANY_TO_RESET whenIsActive{
      ctrlHalt := True
      reg.hcControl.HCFS := MainState.RESET
      when(!doUnschedule){
        goto(RESET)
      }
    }

    val operationalIsDone = operational.isStopped
    ANY_TO_SUSPEND onEntry {
      doUnschedule := True
    }
    ANY_TO_SUSPEND whenIsActive{
      ctrlHalt := True
      operational.askExit := True
      reg.hcControl.HCFS := MainState.SUSPEND
      when(!doUnschedule && !doSoftReset && operationalIsDone){
        goto(SUSPEND)
      }
    }

    always{
      // Handle HCD asking a usbRESET transition
      when(reg.hcControl.HCFSWrite.valid && reg.hcControl.HCFSWrite.payload === MainState.RESET) {
        goto(ANY_TO_RESET)
      }

      // Handle software reset
      when(reg.hcCommandStatus.startSoftReset){
        doSoftReset := True
        goto(ANY_TO_SUSPEND)
      }
    }
  }
}


/*
TODO
 suspend / resume
 protect port exiting reset in the middle of something else
 6.5.7 RootHubStatusChange Event
 Likewise, the Root Hub must wait 5 ms after the Host Controller enters U SB S USPEND before generating a local wakeup event and forcing a transition to U SB R ESUME
 !! Descheduling during a transmition will break the PHY !!
 IE => Setting this bit is guaranteed to take effect in the next Frame (not the current Frame).

test :
 */
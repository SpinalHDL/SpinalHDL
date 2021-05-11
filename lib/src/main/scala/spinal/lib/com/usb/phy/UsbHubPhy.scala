package spinal.lib.com.usb.phy

import spinal.core._
import spinal.lib._
import spinal.lib.com.usb.UsbTimer
import spinal.lib.fsm._
import spinal.lib.io.TriState



case class UsbPhyFsNativeIo() extends Bundle with IMasterSlave {
  val dp,dm = TriState(Bool)

  override def asMaster(): Unit = {
    master(dp,dm)
  }

  def stage() : UsbPhyFsNativeIo = {
    val ret = UsbPhyFsNativeIo().setCompositeName(this, "stage", true)
    ret.dp << dp.stage()
    ret.dm << dm.stage()
    ret
  }
}



case class UsbLsFsPhyAbstractIo() extends Bundle with IMasterSlave {
  val tx = new Bundle {
    val enable = Bool()
    val data = Bool()
    val se0 = Bool()
  }
  val rx = new Bundle {
    val dp = Bool()
    val dm = Bool()
//    val rcv = Bool()
  }

  val overcurrent = Bool()
  val power = Bool()

  override def asMaster(): Unit = {
    out(tx)
    in(rx)
    in(overcurrent)
    out(power)
  }

  def toNativeIo() : UsbPhyFsNativeIo = {
    val ret = UsbPhyFsNativeIo().setCompositeName(this, "native", true)
    ret.dp.writeEnable := tx.enable
    ret.dm.writeEnable := tx.enable
    ret.dp.write := !tx.se0 && tx.data
    ret.dm.write := !tx.se0 && !tx.data
    rx.dp := ret.dp.read
    rx.dm := ret.dm.read
    ret
  }
}

case class UsbLsFsPhyFilter(fsRatio : Int) extends Component {
  val io = new Bundle {
    val lowSpeed = in Bool()

    val usb = new Bundle {
      val dp = in Bool()
      val dm = in Bool()
    }

    val filtred = new Bundle {
      val dp, dm, d = out Bool()
      val se0 = out Bool()
      val sample = out Bool()
    }
  }
  val frontend = new Area{
    val valid = io.usb.dp =/= io.usb.dm
    val dp = io.usb.dp
    val dm = io.usb.dm
    val value = dp
    val valueOld = RegNextWhen(value, valid)
    val edge = value ^ valueOld
  }

  val timer = new Area{
    val clear = False
    val counter = Reg(UInt(log2Up(fsRatio*8) bits))
    val counterLimit = io.lowSpeed ? U(fsRatio*8-1) | U(fsRatio-1)
    counter := counter + 1
    when(counter === counterLimit  || clear){
      counter := 0
    }

    val sampleAt = io.lowSpeed ? U(fsRatio*8/2-1) | U(fsRatio/2-1)
    val sampleDo = counter === sampleAt

    when(frontend.valid & frontend.edge){
      clear := True
    }
  }

  io.filtred.dp := frontend.dp
  io.filtred.dm := frontend.dm
  io.filtred.d := frontend.value
  io.filtred.sample := timer.sampleDo
  io.filtred.se0 := !frontend.dp && !frontend.dm
}

case class UsbLsFsPhy(portCount : Int, fsRatio : Int, sim : Boolean = false) extends Component {
  val io = new Bundle {
    val ctrl = slave(UsbHubLsFs.Ctrl(portCount))
    val usb = Vec(master(UsbLsFsPhyAbstractIo()), portCount)
  }
    val lsRatio = fsRatio*8



  class Timeout(time : Double) extends  Area {
    val at = (12e6 * fsRatio * time toInt) - 1
    val counter = Reg(UInt((log2Up(at+1) bits))) init(0)
    val clear = False
    val hit = counter === at
    val hitLast = RegNext(hit)
    val event = hit && !hitLast
    counter := counter + U(!hit)
    when(clear) {
      counter := 0
    }
  }



  val txShared = new Area {
    val timer = new UsbTimer(counterTimeMax = 0.667e-6 * 8, fsRatio) {
      val oneCycle = cycles(1)
      val twoCycle = cycles(2)
      val fourCycle = cycles(5)

      lowSpeed := False
    }

    val rxToTxDelay = new UsbTimer(0.667e-6*4, fsRatio){
      lowSpeed.setAsReg()
      val twoCycle = cycles(4)
      val active = RegInit(False) clearWhen(twoCycle)
    }

    val encoder = new Area {
      val input = new Area{
        val valid = False
        val ready = False
        val data = Bool().assignDontCare()
        val lowSpeed = Bool().assignDontCare()
      }
      val output = new Area{
        val valid = False
        val se0 = False
        val lowSpeed = Bool().assignDontCare()
        val data = Bool().assignDontCare()
      }

      val counter = Reg(UInt(3 bits))
      val state = Reg(Bool)

      when(input.valid) {
        output.valid := input.valid
        output.lowSpeed := input.lowSpeed
        timer.lowSpeed := input.lowSpeed
        when(counter === 6) {
          output.data := !state
          when(timer.oneCycle) {
            counter := 0;
            state := !state
            timer.clear := True
          }
        } otherwise {
          when(input.data) {
            output.data := state
            when(timer.oneCycle) {
              counter := counter + 1;
              input.ready := True
            }
          } otherwise {
            output.data := !state
            when(timer.oneCycle) {
              counter := 0;
              input.ready := True
              state := !state
            }
          }
        }
      }

      when(!input.valid) {
        counter := 0
        state := True
      }

      when(input.ready){
        timer.clear := True
      }
    }

    val serialiser = new Area {
      val input = new Area{
        val valid = False
        val ready = False
        val data = Bits(8 bits) assignDontCare()
        val lowSpeed = Bool().assignDontCare()
      }

      val bitCounter = Reg(UInt(3 bits))

      when(input.valid){
        encoder.input.valid := True
        encoder.input.data := input.data(bitCounter)
        encoder.input.lowSpeed := input.lowSpeed
        when(encoder.input.ready){
          bitCounter := bitCounter + 1
          when(bitCounter === 7){
            input.ready := True
          }
        }
      }

      when(!input.valid || input.ready){
        bitCounter := 0
      }
    }

    val lowSpeedSof = new Area{
      val timer = Reg(UInt(log2Up(fsRatio*8) bits))
      val state = Reg(UInt(2 bits)) init(0)
      val increment = False
      val overrideEncoder = RegInit(False) clearWhen(encoder.output.valid.fall)
      state := state + U(increment)
      when(state === 0){
        when(io.ctrl.tx.valid && io.ctrl.tx.first && io.ctrl.tx.fragment === 0xA5){
          overrideEncoder := True
          increment := True
          timer := 0
        }
      } otherwise {
        timer := timer + 1
        when(timer === fsRatio*8-1){
          state := state + 1
        }
      }
      val valid = state =/= 0
      val data = False
      val se0 = state =/= 3
    }

    val frame = new StateMachine {
      val IDLE, TAKE_LINE, PREAMBLE_SYNC, PREAMBLE_PID, PREAMBLE_DELAY, SYNC, DATA, EOP_0, EOP_1, EOP_2 = new State
      setEntry(IDLE)

      val busy = this.isStarted


      val wasLowSpeed = Reg(Bool)

      def doByte(current : State, value : Bits, lowSpeed : Bool, next : State): Unit = current.whenIsActive{
        serialiser.input.valid := True
        serialiser.input.data := value
        serialiser.input.lowSpeed := lowSpeed
        when(serialiser.input.ready) {
          goto(next)
        }
      }

      IDLE whenIsActive {
        timer.clear := True
        wasLowSpeed := io.ctrl.lowSpeed
        when(io.ctrl.tx.valid && !rxToTxDelay.active) {
          goto(TAKE_LINE)
        }
      }

      //Avoid special signal reflection of the first bit of the frame
      TAKE_LINE whenIsActive{
        encoder.output.valid    := True
        encoder.output.data     := True
        encoder.output.lowSpeed := False
        timer.lowSpeed := False
        when(timer.oneCycle) {
          timer.clear := True
          when(io.ctrl.lowSpeed) {
            goto(PREAMBLE_SYNC)
          } otherwise {
            goto(SYNC)
          }
        }
      }

      doByte(PREAMBLE_SYNC , 0x80, False,           PREAMBLE_PID)
      doByte(PREAMBLE_PID  , 0x3C, False,           PREAMBLE_DELAY)

      PREAMBLE_DELAY whenIsActive{
        encoder.output.valid    := True
        encoder.output.data     := True
        encoder.output.lowSpeed := False
        timer.lowSpeed := False
        when(timer.fourCycle) {
          timer.clear := True
          goto(SYNC)
        }
      }

      doByte(SYNC, 0x80,wasLowSpeed, DATA)

      io.ctrl.tx.ready := False
      DATA whenIsActive {
        serialiser.input.valid := True
        serialiser.input.data := io.ctrl.tx.fragment
        serialiser.input.lowSpeed := wasLowSpeed
        when(serialiser.input.ready) {
          io.ctrl.tx.ready := True
          when(io.ctrl.tx.last) {
            goto(EOP_0)
          }
        }
      }

      EOP_0 whenIsActive {
        encoder.output.valid := True
        encoder.output.se0 := True
        encoder.output.lowSpeed := wasLowSpeed
        timer.lowSpeed := wasLowSpeed
        when(timer.twoCycle) {
          timer.clear := True
          goto(EOP_1)
        }
      }
      EOP_1 whenIsActive {
        encoder.output.valid := True
        encoder.output.data := True
        encoder.output.lowSpeed := wasLowSpeed
        timer.lowSpeed := wasLowSpeed
        when(timer.oneCycle) {
          timer.clear := True
          goto(EOP_2)
        }
      }
      EOP_2 whenIsActive {
        timer.lowSpeed := wasLowSpeed
        when(timer.twoCycle) {
          timer.clear := True
          goto(IDLE)
        }
      }
    }
  }

  //11.6.1 Receiver
  val upstreamRx = new StateMachine{
    val IDLE, SUSPEND = new State
    setEntry(IDLE)
    val timer = new UsbTimer(3e-3, fsRatio){
      val IDLE_EOI = trigger(3e-3)
    }

    val exitSuspend = txShared.encoder.output.valid //simplified maybe too much
    timer.clear setWhen (exitSuspend)

    IDLE whenIsActive{
      when(timer.IDLE_EOI){
        goto(SUSPEND)
      }
    }
    SUSPEND whenIsActive{
      when(exitSuspend){
        goto(IDLE)
      }
    }
  }

  val Rx_Suspend = upstreamRx.isActive(upstreamRx.SUSPEND)

//  val hcOverrides = new StateMachine{
//    val IDLE, RESET, RESUME, EOP_0, EOP_1 = new State()
//    setEntry(IDLE)
//
//    val tx = new Bundle {
//      val enable = False
//      val data = True // ^ lowSpeed
//      val se0 = False
//    }
//
//    val timer = new UsbTimer(3e-6, fsRatio){
//      val ONE_BIT = cycles(1)
//      val TWO_BIT = cycles(2)
//      this.lowSpeed := True
//    }
//
//    always{
//      when(io.ctrl.usbResume){ goto (RESUME) }
//      when(io.ctrl.usbReset){ goto (RESUME) }
//    }
//    RESUME whenIsActive{
//      tx.enable := True
//      tx.data := False
//      tx.se0 := False
//      when(!io.ctrl.usbResume){ goto (EOP_0) }
//    }
//
//    RESET whenIsActive{
//      tx.enable := True
//      tx.se0 := True
//      when(!io.ctrl.usbResume){ goto (IDLE) }
//    }
//
//    EOP_0 onEntry(timer.clear := True)
//    EOP_0 whenIsActive{
//      tx.enable := True
//      tx.se0 := True
//      when(timer.TWO_BIT){
//        goto(EOP_1)
//      }
//    }
//
//    EOP_1 onEntry(timer.clear := True)
//    EOP_1 whenIsActive{
//      tx.enable := True
//      tx.data := True
//      tx.se0 := False
//      when(timer.ONE_BIT){
//        goto(IDLE)
//      }
//    }
//  }


  io.ctrl.overcurrent := False

  io.ctrl.rx.valid := False
  io.ctrl.rx.active := False
  io.ctrl.rx.stuffingError := False
  io.ctrl.rx.data.assignDontCare()

  val resumeFromPort = False

  val ports = for((usb, ctrl) <- (io.usb, io.ctrl.ports).zipped) yield new Area{
//    val connected = Reg(Bool) init(False) //TODO
    val portLowSpeed = Reg(Bool)
//    val enable = Reg(Bool)

//    ctrl.connected := connected
    ctrl.lowSpeed := portLowSpeed
    ctrl.remoteResume := False //TODO

    val filter = UsbLsFsPhyFilter(fsRatio)
    filter.io.lowSpeed := io.ctrl.lowSpeed
    filter.io.usb.dp := usb.rx.dp
    filter.io.usb.dm := usb.rx.dm

    val rx = new Area{
      val enablePackets = False
//      val timer = new UsbTimer(counterTimeMax = 20e-3 * 1.1, fsRatio) {
//        val reset = trigger(10e-3 * 0.9)
//        val suspend = trigger(3e-3 * 0.9)
//        val resume = trigger(20e-3 * 0.9)
//      }

      val j = filter.io.filtred.dp === !portLowSpeed && filter.io.filtred.dm ===  portLowSpeed
      val k = filter.io.filtred.dp ===  portLowSpeed && filter.io.filtred.dm === !portLowSpeed

      usb.power := ctrl.power
      ctrl.overcurrent := usb.overcurrent

      val stuffingError = Reg(Bool)

      val waitSync = False
      val decoder = new Area{
        val state = Reg(Bool)

        val output = Flow(Bool)
        output.valid := False
        output.payload.assignDontCare()

        when(filter.io.filtred.sample){
          output.valid := True
          when(state ^ filter.io.filtred.d ^ portLowSpeed){
            output.payload := False
            state := !state
          } otherwise {
            output.payload := True
          }
        }

        when(waitSync){
          state := False
        }
      }

      val destuffer = new Area{
        val counter = Reg(UInt(3 bits))
        val unstuffNext = counter === 6

        val output = decoder.output.throwWhen(unstuffNext)

        when(decoder.output.valid){
          counter := counter + 1
          when(!decoder.output.payload || unstuffNext){
            counter := 0
            stuffingError setWhen(decoder.output.payload)
          }
        }

        when(waitSync){
          counter := 0
        }
      }

      val history = new Area{
        val updated = CombInit(destuffer.output.valid)
        val value = History(destuffer.output.payload, 0 to 7, when = updated).reverse.asBits
        val sync = new Area {
          val pattern = 0x2A ^ 0xFF
          val hit = updated && value === pattern
        }
      }

      val eop = new Area{
        val maxThreshold = io.ctrl.lowSpeed ? U(fsRatio*8*3)     | U(fsRatio*3)
        val minThreshold = io.ctrl.lowSpeed ? U(fsRatio*8*2*2/3) | U(fsRatio*2*2/3)
        val counter = Reg(UInt(log2Up(fsRatio*8*3+1) bits)) init(0)
        val maxHit = counter === maxThreshold
        val hit = False

        when(!filter.io.filtred.dp && !filter.io.filtred.dm){
          when(!maxHit) {
            counter := counter + 1
          }
        } otherwise {
          counter := 0
        }
        when(j){
          when(counter >= minThreshold && !maxHit){
            hit := True
          }
        }
      }

      val packet = new StateMachine {
        val IDLE, PACKET, ERRORED = new State
        setEntry(IDLE)

        val counter = Reg(UInt(3 bits))
        IDLE.onEntry(waitSync := True)
        IDLE.whenIsActive{
          waitSync := True
          counter := 0
          stuffingError := False
          when(history.sync.hit){
            goto(PACKET)
          }
        }
        PACKET whenIsActive {
          io.ctrl.rx.active := True
          io.ctrl.rx.data := history.value
          io.ctrl.rx.stuffingError := stuffingError
          when(destuffer.output.valid){
            counter := counter + 1
            when(counter === 7){
              io.ctrl.rx.valid := enablePackets
              when(stuffingError){
                goto(ERRORED)
              }
            }
          }
        }

        //Ensure that all activities on the bus are done before released the FSM
        val errorTimeout = new UsbTimer(20.0*0.667e-6,fsRatio){
          lowSpeed := io.ctrl.lowSpeed
          val trigger = cycles(20)
          val p,n = Reg(Bool)

          ERRORED.onEntry{
            clear := True
          }
          ERRORED whenIsActive{
            io.ctrl.rx.active := True
            p := filter.io.filtred.dp
            n := filter.io.filtred.dm
            when(p =/= filter.io.filtred.dp || n =/= filter.io.filtred.dm){
              clear := True
            }
            when(trigger){
              goto(IDLE)
            }
          }
        }


        always{
          when(eop.hit){
            txShared.rxToTxDelay.clear := True
            txShared.rxToTxDelay.active := True
            txShared.rxToTxDelay.lowSpeed := io.ctrl.lowSpeed
            goto(IDLE)
          }
          when(txShared.encoder.output.valid){
            goto(IDLE)
          }
        }
      }

      val disconnect = new Timeout(2.2e-6)
      disconnect.clear setWhen(!filter.io.filtred.se0 || usb.tx.enable)
      ctrl.disconnect := disconnect.event
    }

    val fsm = new StateMachine{
      // usb11.pdf 11.5 Downstream Ports
      val POWER_OFF, DISCONNECTED, DISABLED, RESETTING, RESETTING_DELAY, RESETTING_SYNC, ENABLED, SUSPENDED, RESUMING, SEND_EOP_0, SEND_EOP_1, RESTART_S, RESTART_E  = new State
      setEntry(POWER_OFF)

      val timer = new UsbTimer(50e-3, fsRatio){
        val DISCONNECTED_EOI = trigger(500e-6)
        val RESET_DELAY = trigger(50e-6)
        val RESET_EOI = trigger(if(sim)3e-3 else 50e-3)
        val RESUME_EOI = trigger(if(sim)2e-3 else 21e-3)
        val RESTART_EOI = trigger(100e-6)
        val ONE_BIT = cycles(1)
        val TWO_BIT = cycles(2)
        this.lowSpeed := portLowSpeed
      }

      ctrl.disable.ready := True //TODO
      ctrl.reset.ready := False
      ctrl.resume.ready := True  //TODO
      ctrl.suspend.ready := True
      ctrl.connect := False

      usb.tx.enable := False
      usb.tx.data  assignDontCare()
      usb.tx.se0   assignDontCare()

      def SE0 =  filter.io.filtred.se0
      def K   = !SE0 && (!filter.io.filtred.d ^ portLowSpeed)

      val resetInProgress = False
      val lowSpeedEop = Reg(Bool)

      always{
        when(!ctrl.power || io.ctrl.usbReset){
          goto(POWER_OFF)
        } elsewhen(rx.disconnect.event){
          goto(DISCONNECTED)
        } elsewhen(ctrl.disable.valid){
          goto(DISABLED)
        } elsewhen(ctrl.reset.valid){
          when(!resetInProgress) {
            when(filter.io.filtred.dm =/= filter.io.filtred.dp) {
              portLowSpeed := !filter.io.filtred.d
              goto(RESETTING)
            } //TODO otherwise set C_PORT_ENABLE
          }
        }
      }

      POWER_OFF whenIsActive{
        usb.tx.enable := True //Optional on root hub
        usb.tx.se0 := True
        when(ctrl.power){
          goto(DISCONNECTED)
        }
      }

      DISCONNECTED.onEntry{
        timer.clear := True
      }
      DISCONNECTED whenIsActive{
        when(!filter.io.filtred.dp && !filter.io.filtred.dm){
          timer.clear := True
        }
        when(timer.DISCONNECTED_EOI){
          ctrl.connect := True
          goto(DISABLED)
        }
      }

      //DISABLED is a empty state

      RESETTING.onEntry{
        timer.clear := True
      }
      RESETTING whenIsActive{
        resetInProgress := True
        usb.tx.enable := True
        usb.tx.se0 := True
        when(timer.RESET_EOI){
          goto(RESETTING_DELAY)
        }
      }

      //Ensure a proper reset ending
      RESETTING_DELAY.onEntry{
        timer.clear := True
      }
      RESETTING_DELAY whenIsActive{
        resetInProgress := True
        when(timer.RESET_DELAY){
          goto(RESETTING_SYNC)
        }
      }

      //Ensure that reset exit when nothing is on the bus
      RESETTING_SYNC whenIsActive{
        resetInProgress := True
        when(!txShared.encoder.output.valid){
          ctrl.reset.ready := True
          goto(ENABLED)
        }
      }

      val forceJ = portLowSpeed && !txShared.encoder.output.lowSpeed
      ENABLED whenIsActive{
        rx.enablePackets := True
        usb.tx.enable := txShared.encoder.output.valid
        usb.tx.data := (txShared.encoder.output.data || forceJ) ^ portLowSpeed
        usb.tx.se0 := (txShared.encoder.output.se0 && !forceJ)

        when(portLowSpeed && txShared.lowSpeedSof.overrideEncoder){
          usb.tx.enable := txShared.lowSpeedSof.valid
          usb.tx.data   := txShared.lowSpeedSof.data
          usb.tx.se0    := txShared.lowSpeedSof.se0
        }


        when(ctrl.suspend.valid){
          goto(SUSPENDED)
        } elsewhen(Rx_Suspend & (SE0 || K)){
          goto(RESTART_E)
        } elsewhen(io.ctrl.usbResume){
          goto(RESUMING)
        }
      }

      SUSPENDED whenIsActive{
        when(ctrl.resume.valid || (!Rx_Suspend & K)){
          goto(RESUMING)
        } elsewhen(Rx_Suspend & (SE0 || K)){
          goto(RESTART_S)
        }
      }

      RESUMING onEntry {timer.clear := True}
      RESUMING whenIsActive{
        usb.tx.enable := True
        usb.tx.data := portLowSpeed
        usb.tx.se0 := False

        when(timer.RESUME_EOI){
          lowSpeedEop := True
          goto(SEND_EOP_0)
        }
      }

      SEND_EOP_0 onEntry(timer.clear := True)
      SEND_EOP_0 whenIsActive{
        timer.lowSpeed setWhen(lowSpeedEop)
        usb.tx.enable := True
        usb.tx.se0 := True
        when(timer.TWO_BIT){
          goto(SEND_EOP_1)
        }
      }

      SEND_EOP_1 onEntry(timer.clear := True)
      SEND_EOP_1 whenIsActive{
        timer.lowSpeed setWhen(lowSpeedEop)
        usb.tx.enable := True
        usb.tx.data := !portLowSpeed
        usb.tx.se0 := False
        when(timer.ONE_BIT){
          goto(ENABLED)
        }
      }

      //11.9 Suspend and Resume
      RESTART_S onEntry(timer.clear := True)
      RESTART_S whenIsActive {
        when(K){
          resumeFromPort := True
          goto(RESUMING)
        }
        when(timer.RESTART_EOI){
          goto(DISCONNECTED) //TODO raise interrupts and stuff like this ?
        }
      }
      RESTART_E onEntry(timer.clear := True)
      RESTART_E whenIsActive {
        when(K){
          resumeFromPort := True
          goto(RESUMING)
        }
        when(timer.RESTART_EOI){
          goto(DISCONNECTED) //TODO raise interrupts and stuff like this ?
        }
      }

      rx.disconnect.clear setWhen(List(ENABLED, SUSPENDED, DISABLED).map(!isActive(_)).andR)
//      when(io.ctrl.resume){
//        for(usb <- io.usb) {
//          usb.tx.enable := True
//          usb.tx.data := True
//          usb.tx.se0 := False
//        }
//      }
//
//      when(io.ctrl.usbReset){
//        for(usb <- io.usb) {
//          usb.tx.enable := True
//          usb.tx.se0 := True
//        }
//      }
    }
  }


}
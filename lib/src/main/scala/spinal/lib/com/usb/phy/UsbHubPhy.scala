package spinal.lib.com.usb.phy

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.io.TriState



case class UsbPhyFsNativeIo() extends Bundle with IMasterSlave {
  val dp,dm = TriState(Bool)

  override def asMaster(): Unit = {
    master(dp,dm)
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
}

case class UsbLsFsPhyFilter(fsRatio : Int) extends Component {
  val io = new Bundle {
    val fullSpeed = in Bool()

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
    counter := counter + 1
    when(counter === fsRatio-1 || clear){
      counter := 0
    }

    val sampleAt = io.fullSpeed ? U(fsRatio/2) | U(fsRatio*8/2)
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

  class Timer(counterTimeMax : Double) extends  Area {
    val lowSpeed = Bool()
    val counter = Reg(UInt((log2Up(12e6 * fsRatio * counterTimeMax toInt) + 2) bits))
    val clear = False
    counter := counter + 1
    when(clear) {
      counter := 0
    }

    def trigger(t: Double): Bool = {
      val at = (12e6 * fsRatio * t toInt) - 1
      counter === at
    }

    def cycles(c: Int): Bool = {
      val ls = c * lsRatio - 1
      val fs = c * fsRatio - 1
      counter === (lowSpeed ? U(ls) | U(fs))
    }
  }

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
    val timer = new Timer(counterTimeMax = 20e-3 * 1.1) {
      val oneCycle = cycles(1)
      val twoCycle = cycles(2)
      val fourCycle = cycles(4)

      lowSpeed := False
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
      val IDLE, PREAMBLE_SYNC, PREAMBLE_PID, PREAMBLE_DELAY, SYNC, DATA, EOP_0, EOP_1, EOP_2 = new State
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
        when(io.ctrl.tx.valid) {
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
        when(timer.oneCycle) {
          timer.clear := True
          goto(IDLE)
        }
      }
    }
  }


  io.ctrl.overcurrent := False

  io.ctrl.rx.valid := False
  io.ctrl.rx.active := False
  io.ctrl.rx.error := False //TODO
  io.ctrl.rx.data := 0

  val ports = for((usb, ctrl) <- (io.usb, io.ctrl.ports).zipped) yield new Area{
//    val connected = Reg(Bool) init(False) //TODO
    val lowSpeed = Reg(Bool)
    val ls = lowSpeed
//    val enable = Reg(Bool)

//    ctrl.connected := connected
    ctrl.lowSpeed := lowSpeed
    ctrl.remoteResume := False //TODO

    val filter = UsbLsFsPhyFilter(fsRatio)
    filter.io.usb.dp := usb.rx.dp
    filter.io.usb.dm := usb.rx.dm

    val rx = new Area{
      val timer = new Timer(counterTimeMax = 20e-3 * 1.1) {
        val reset = trigger(10e-3 * 0.9)
        val suspend = trigger(3e-3 * 0.9)
        val resume = trigger(20e-3 * 0.9)
      }

      val j = filter.io.filtred.dp === !lowSpeed && filter.io.filtred.dm ===  lowSpeed
      val k = filter.io.filtred.dp ===  lowSpeed && filter.io.filtred.dm === !lowSpeed

      usb.power := ctrl.power
      ctrl.overcurrent := usb.overcurrent

      val decoder = new Area{
        val state = Reg(Bool) init(True) //TODO reset
        val clear = False
        when(clear){
          state := True
        }

        val output = Flow(Bool)
        output.valid := False
        output.payload.assignDontCare()

        when(filter.io.filtred.sample){
          output.valid := True
          when(state ^ filter.io.filtred.d ^ lowSpeed){
            output.payload := False
            state := !state
          } otherwise {
            output.payload := True
          }
        }
      }

      val destuffer = new Area{
        val counter = Reg(UInt(3 bits)) init(0)
        val unstuffNext = counter === 6

        val output = decoder.output.throwWhen(unstuffNext).stage()

        when(decoder.output.valid){
          counter := counter + 1
          when(!decoder.output.payload || unstuffNext){
            counter := 0
          }
        }
      }

      val history = new Area{
        val value = History(destuffer.output.payload, 0 to 7, when = destuffer.output.valid).asBits
        val updated = CombInit(destuffer.output.valid)
        val sync = new Area {
          val pattern = 0x80
          val hit = updated && value === pattern
        }
      }

  //    val resume = new Area{
  //      val minThreshold = io.ctrl.fullSpeed ? U(fsRatio*3) | U(fsRatio*8*3)
  //      val counter = Reg(UInt(log2Up(fsRatio*8*3+1) bits)) init(0)
  //      val maxHit = counter === maxThreshold
  //      val hit = False
  //
  //      when(!filter.io.filtred.dp && !filter.io.filtred.dm){
  //        when(!maxHit) {
  //          counter := counter + 1
  //        }
  //      } otherwise {
  //        counter := 0
  //      }
  //      when(filter.io.filtred.dp === io.ctrl.fullSpeed && filter.io.filtred.dm === !io.ctrl.fullSpeed){
  //        when(counter >= minThreshold && !maxHit){
  //          hit := True
  //        }
  //      }
  //    }

      val eop = new Area{
        val minThreshold = lowSpeed ? U(fsRatio*8*3)     | U(fsRatio*3)
        val maxThreshold = lowSpeed ? U(fsRatio*8*2*2/3) | U(fsRatio*2*2/3)
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

      decoder.clear setWhen(eop.hit)


      val packet = new StateMachine {
        val IDLE, PACKET = new State
        setEntry(IDLE)

        val counter = Reg(UInt(3 bits))

//        io.ctrl.rx.valid := False
//        io.ctrl.rx.active := False
//        io.ctrl.rx.error := False //TODO
//        io.ctrl.rx.data := history.value

        IDLE.whenIsActive{
          when(history.sync.hit){
            goto(PACKET)
            counter := 0
          }
        }
        PACKET whenIsActive {
          io.ctrl.rx.active := True
          when(destuffer.output.valid){
            counter := counter + 1
            when(counter === 7){
              io.ctrl.rx.valid := True
            }
          }
          when(eop.hit){
            goto(IDLE)
          }
        }
      }

      val disconnect = new Timeout(2.1e-6)
//      val connect = new Timeout(3e-6)
      disconnect.clear setWhen(!filter.io.filtred.se0 || usb.tx.enable)
//      connect.clear setWhen(filter.io.filtred.dm === filter.io.filtred.dp)

//      ctrl.connect := connect.event
      ctrl.disconnect := disconnect.event
    }

    val fsm = new StateMachine{
      // usb11.pdf 11.5 Downstream Ports
      val POWER_OFF, DISCONNECTED, DISABLED, RESETTING, RESETTING_DELAY, RESETTING_SYNC, ENABLED, SUSPENDED, RESUMING, SEND_EOP_0, SEND_EOP_1, RESTART_S, RESTART_E  = new State
      setEntry(POWER_OFF)

      val timer = new Timer(50e-3){
        val DISCONNECTED_EOI = trigger(500e-6)
        val RESET_DELAY = trigger(50e-6)
        val RESET_EOI = trigger(if(sim)3e-3 else 50e-3)
        val RESUME_EOI = trigger(if(sim)2e-3 else 21e-3)
        val ONE_BIT = cycles(1)
        val TWO_BIT = cycles(2)
        this.lowSpeed := ls
      }

      ctrl.disable.ready := True //TODO
      ctrl.reset.ready := False
      ctrl.resume.ready := False  //TODO
      ctrl.suspend.ready := False  //TODO
      ctrl.connect := False

      usb.tx.enable := False
      usb.tx.data  assignDontCare()
      usb.tx.se0   assignDontCare()

      val resetInProgress = False

      always{
        when(!ctrl.power){
          goto(POWER_OFF)
        } elsewhen(rx.disconnect.event){
          goto(DISCONNECTED)
        } elsewhen(ctrl.disable.valid){
          goto(DISABLED)
        } elsewhen(ctrl.reset.valid){
          when(!resetInProgress) {
            when(filter.io.filtred.dm =/= filter.io.filtred.dp) {
              lowSpeed := !filter.io.filtred.d
              goto(RESETTING)
            } //TODO otherwise set C_PORT_ENABLE
          }
        }
      }

      POWER_OFF whenIsActive{
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

      ENABLED whenIsActive{
        usb.tx.enable := txShared.encoder.output.valid && !(lowSpeed && !txShared.encoder.output.lowSpeed)
        usb.tx.data := txShared.encoder.output.data ^ lowSpeed
        usb.tx.se0 := txShared.encoder.output.se0

        when(lowSpeed && txShared.lowSpeedSof.overrideEncoder){
          usb.tx.enable := txShared.lowSpeedSof.valid
          usb.tx.data   := txShared.lowSpeedSof.data
          usb.tx.se0    := txShared.lowSpeedSof.se0
        }

        when(ctrl.suspend.valid){
          goto(SUSPENDED)
        }
      }

      SUSPENDED whenIsActive{
        when(ctrl.resume.valid){
          goto(RESUMING)
        }
      }

      RESUMING onEntry {timer.clear := True}
      RESUMING whenIsActive{
        usb.tx.enable := True
        usb.tx.data := lowSpeed
        usb.tx.se0 := False

        when(timer.RESUME_EOI){
          goto(SEND_EOP_0)
        }
      }

      SEND_EOP_0 onEntry(timer.clear := True)
      SEND_EOP_0 whenIsActive{
        usb.tx.enable := True
        usb.tx.se0 := True
        when(timer.TWO_BIT){
          goto(SEND_EOP_1)
        }
      }

      SEND_EOP_1 onEntry(timer.clear := True)
      SEND_EOP_1 whenIsActive{
        usb.tx.enable := True
        usb.tx.data := !lowSpeed
        usb.tx.se0 := False
        when(timer.ONE_BIT){
          goto(ENABLED)
        }
      }



      //
      //    DISCONNECTED.onEntry{
      //      connected := False
      //    }
      //    DISCONNECTED.whenIsActive{
      //      when(filter.io.filtred.dp =/= filter.io.filtred.dm){
      //        lowSpeed := filter.io.filtred.dm
      //        connected := True
      //      }
      //    }
      //
      //    CONNECTED.whenIsActive{
      //      when(!io.ctrl.transaction) {
      //        when(ctrl.reset.valid) {
      //          goto(RESET)
      //        }
      //        when(ctrl.suspend.valid) {
      //          goto(SUSPEND)
      //        }
      //        when(ctrl.resume.valid) {
      //          goto(RESUME)
      //        }
      //      }
      //    }
      //
      //    RESET.whenIsActive{
      //
      //    }
      //    SUSPEND.whenIsActive{
      //
      //    }
      //    RESUME.whenIsActive{
      //
      //    }

      rx.disconnect.clear setWhen(List(ENABLED, SUSPENDED, DISABLED).map(isActive(_)).orR) //TODO can't disconnect when port enabled ?

    }
  }
}
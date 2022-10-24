package spinal.lib.com.usb.phy

import spinal.core._
import spinal.lib._
import spinal.lib.com.usb.UsbTimer
import spinal.lib.com.usb.udc.UsbDeviceCtrl.PhyIo
import spinal.lib.fsm.{State, StateMachine}

case class UsbDevicePhyNative(sim : Boolean = false) extends Component{
  val io = new Bundle {
    val ctrl = slave(PhyIo())
    val usb = master(UsbLsFsPhyAbstractIo())
    val power = in Bool()
    val pullup = out Bool()
  }

  val fsRatioExact = (ClockDomain.current.frequency.getValue.toDouble/12e6)
  val fsRatio = fsRatioExact.round.toInt

  io.pullup := io.ctrl.pullup

  val timer = new UsbTimer(counterTimeMax = 0.084e-6 * 8, fsRatio) {
    val oneCycle = cycles(1)
    val twoCycle = cycles(2)
    val fourCycle = cycles(5)

    lowSpeed := False
  }

  val rxToTxDelay = new UsbTimer(0.084e-6*2, fsRatio){
    val twoCycle = cycles(2)
    val active = RegInit(False) clearWhen(twoCycle)
    lowSpeed := False
  }

  val tx = new Area {
    val encoder = new Area {
      val input = new Area {
        val valid = False
        val ready = False
        val data = Bool().assignDontCare()
      }
      val output = new Area {
        val valid = False
        val se0 = False
        val data = Bool().assignDontCare()
      }

      val counter = Reg(UInt(3 bits))
      val state = Reg(Bool())

      when(input.valid) {
        output.valid := input.valid

        when(input.data) {
          output.data := state
          when(timer.oneCycle) {
            counter := counter + 1;
            input.ready := True
            when(counter === 5){
              timer.clear := True
              input.ready := False
              state := !state
            }
            when(counter === 6){
              counter := 0;
            }
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

      when(!input.valid) {
        counter := 0
        state := True
      }

      when(input.ready) {
        timer.clear := True
      }
    }


    val serialiser = new Area {
      val input = Stream(Bits(8 bits))
      val bitCounter = Reg(UInt(3 bits))

      input.valid := False
      input.payload.assignDontCare()
      input.ready := False

      when(input.valid) {
        encoder.input.valid := True
        encoder.input.data := input.payload(bitCounter)
        when(encoder.input.ready) {
          bitCounter := bitCounter + 1
          when(bitCounter === 7) {
            input.ready := True
          }
        }
      }

      when(!input.valid || input.ready) {
        bitCounter := 0
      }
    }

    val frame = new StateMachine {
      val IDLE, TAKE_LINE, SYNC, DATA, EOP_0, EOP_1, EOP_2 = new State
      setEntry(IDLE)

      val busy = this.isStarted



      def doByte(current : State, value : Bits, next : State): Unit = current.whenIsActive{
        serialiser.input.valid := True
        serialiser.input.payload := value
        when(serialiser.input.ready) {
          goto(next)
        }
      }

      IDLE whenIsActive {
        timer.clear := True
        when(io.ctrl.tx.stream.valid && !rxToTxDelay.active) {
          goto(TAKE_LINE)
        }
      }

      //Avoid special signal reflection of the first bit of the frame
      TAKE_LINE whenIsActive{
        encoder.output.valid    := True
        encoder.output.data     := True
        timer.lowSpeed := False
        when(timer.oneCycle) {
          timer.clear := True
          goto(SYNC)
        }
      }

      doByte(SYNC, 0x80, DATA)

      io.ctrl.tx.stream.ready := False
      DATA whenIsActive {
        serialiser.input.valid := True
        serialiser.input.payload := io.ctrl.tx.stream.fragment
        when(serialiser.input.ready) {
          io.ctrl.tx.stream.ready := True
          when(io.ctrl.tx.stream.last) {
            goto(EOP_0)
          }
        }
      }

      io.ctrl.tx.eop := False
      EOP_0 whenIsActive {
        encoder.output.valid := True
        encoder.output.se0 := True
        when(timer.twoCycle) {
          timer.clear := True
          io.ctrl.tx.eop := True
          goto(EOP_1)
        }
      }
      EOP_1 whenIsActive {
        encoder.output.valid := True
        encoder.output.data := True
        when(timer.oneCycle) {
          timer.clear := True
          goto(EOP_2)
        }
      }
      EOP_2 whenIsActive {
        when(timer.twoCycle) {
          timer.clear := True
          goto(IDLE)
        }
      }
    }
  }

  val rx = new Area{
    val filter = UsbLsFsPhyFilter(fsRatio)
    filter.io.lowSpeed := False
    filter.io.usb.dp := io.usb.rx.dp
    filter.io.usb.dm := io.usb.rx.dm

    val j =  filter.io.filtred.dp && !filter.io.filtred.dm
    val k = !filter.io.filtred.dp &&  filter.io.filtred.dm

    val stuffingError = Reg(Bool())

    val waitSync = False
    val decoder = new Area{
      val state = Reg(Bool())

      val output = Flow(Bool)
      output.valid := False
      output.payload.assignDontCare()

      when(filter.io.filtred.sample){
        output.valid := True
        when(state ^ filter.io.filtred.d){
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
      val maxThreshold = fsRatio*3
      val minThreshold = fsRatio*2*2/3
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

      io.ctrl.rx.active := False
      io.ctrl.rx.stuffingError := False
      io.ctrl.rx.flow.valid := False
      io.ctrl.rx.flow.payload := history.value

      PACKET whenIsActive {
        io.ctrl.rx.active := True
        io.ctrl.rx.stuffingError := stuffingError
        when(destuffer.output.valid){
          counter := counter + 1
          when(counter === 7){
            io.ctrl.rx.flow.valid := True
            when(stuffingError){
              goto(ERRORED)
            }
          }
        }
      }

      //Ensure that all activities on the bus are done before released the FSM
      val errorTimeout = new UsbTimer(20.0*0.084e-6,fsRatio){
        lowSpeed := False

        val trigger = cycles(20)
        val p,n = Reg(Bool())

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
          rxToTxDelay.clear := True
          rxToTxDelay.active := True
          goto(IDLE)
        }
        when(tx.encoder.output.valid){
          goto(IDLE)
        }
      }
    }


    val timerLong = new UsbTimer(counterTimeMax = 21e-3*2, fsRatio) {
      val factor = if(sim) 0.005 else 1.0
      val resume = trigger(19.9e-3*factor)
      val reset = trigger(9.9e-3*factor)
      val suspend = trigger(2.9e-3*factor)
      val oneBit = trigger(83e-9)
      val threeBit = trigger(83e-9*3)
      val hadOne = RegInit(False) setWhen(oneBit) clearWhen(clear)
      val hadTree = RegInit(False) setWhen(threeBit) clearWhen(clear)
      lowSpeed := False
    }

    val detect = new Area{
      val current = filter.io.filtred.dm ## filter.io.filtred.dp
      val previous = RegNext(current) init(3)
      when(timerLong.counter.msb){
        timerLong.inc := False
      }
      when(current =/= previous){
        timerLong.clear := True
      }

      val isReset     = current === 0
      val isSuspend   = current === 1
      val resumeState = RegInit(False) clearWhen(current =/= 0 && current =/= previous) setWhen(timerLong.resume && current === 2)
      val isResume = resumeState && timerLong.hadOne && !timerLong.hadTree && current === 1

      val resetState = RegInit(False) setWhen(timerLong.reset && isReset) clearWhen(!isReset)
      val suspendState = RegInit(False) setWhen(timerLong.suspend && isSuspend) clearWhen(!isSuspend)
      io.ctrl.reset := resetState
      io.ctrl.suspend := suspendState
      io.ctrl.disconnect := False
      io.ctrl.resume.valid := RegNext(isResume) init(False)
    }
  }

  val tickTimer = new Area {
    val counter = CounterFreeRun(fsRatio)
    val tick = counter.willOverflow === True
    io.ctrl.tick := tick
  }

  io.usb.tx.enable := tx.encoder.output.valid
  io.usb.tx.se0 := tx.encoder.output.se0
  io.usb.tx.data := tx.encoder.output.data

  io.ctrl.power := io.power
}


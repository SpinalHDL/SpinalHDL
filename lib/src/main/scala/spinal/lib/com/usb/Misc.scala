package spinal.lib.com.usb

import spinal.core._
import spinal.lib._
import spinal.lib.com.eth._
import spinal.lib.fsm._



class UsbDataRxFsm(rx : Flow[Bits],
                   rxActive : Bool,
                   rxStuffing : Bool,
                   timeoutClear : Bool,
                   timeoutEvent : Bool) extends StateMachine{
  val IDLE, PID, DATA = new State
  setEntry(IDLE)

  disableAutoStart()

  val pid = Reg(Bits(4 bits))
  val data = Flow(Bits(8 bits))


  val history = History(rx.payload, 1 to 2, when = rx.valid)
  val crc16 = Crc(CrcKind.usb.crc16Check, 8)
  val valids = Reg(Bits(2 bits))
  val notResponding = Reg(Bool())
  val stuffingError = Reg(Bool())
  val pidError = Reg(Bool())
  val crcError = Reg(Bool())
  val hasError = List(notResponding, stuffingError, pidError, crcError).orR

  data.valid := False
  data.payload := history.last

  crc16.io.input.valid := False
  crc16.io.input.payload := rx.payload
  crc16.io.flush := False

  IDLE onEntry {
    timeoutClear := True
    notResponding := False
    stuffingError := False
    pidError := False
    crcError := False
  }
  IDLE whenIsActive{
    when(rxActive) {
      goto(PID)
    } elsewhen(timeoutEvent) {
      notResponding := True
      exitFsm()
    }
  }

  PID.whenIsActive {
    valids := 0
    crc16.io.flush := True
    pidError := True
    when(rx.valid){
      pid := rx.payload(3 downto 0)
      pidError := rx.payload(3 downto 0) =/= ~rx.payload(7 downto 4)
      goto(DATA)
    } elsewhen(!rxActive){
      exitFsm()
    }
  }

  DATA.whenIsActive {
    when(!rxActive){
      when(!valids.andR || crc16.io.result =/= 0x800D){
        crcError := True
      }
      exitFsm()
    } elsewhen (rx.valid) {
      crc16.io.input.valid := True
      valids := valids(0) ## True
      when(valids.andR){
        data.valid := True
      }
    }
  }

  always{
    when(isStarted) {
      when(rx.valid){
        when(rxStuffing){
          stuffingError := True
        }
      }
    }
  }
}

class UsbDataTxFsm(tx : Stream[Fragment[Bits]],
                   eop : Bool) extends StateMachine{
  disableAutoStart()

  val pid = Bits(4 bits).assignDontCare()
  val data = Stream(Fragment(Bits(8 bits)))

  val PID, DATA, CRC_0, CRC_1 = new State
  val EOP = (eop != null) generate new State

  setEntry(PID)

  val crc16 = Crc(CrcKind.usb.crc16, 8)


  data.valid := False
  data.payload.assignDontCare()
  data.ready := False

  crc16.io.input.valid := data.fire
  crc16.io.input.payload := data.fragment
  crc16.io.flush := False


  PID.whenIsActive {
    crc16.io.flush := True
    tx.valid := True
    tx.last := False
    tx.fragment := ~pid ## pid
    when(tx.ready) {
      when(data.valid) {
        goto(DATA)
      } otherwise {
        goto(CRC_0)
      }
    }
  }

  DATA.whenIsActive {
    tx.valid := True
    tx.last := False
    tx.fragment := data.fragment
    when(tx.ready) {
      data.ready := True
      when(data.last) {
        goto(CRC_0)
      }
    }
  }

  CRC_0.whenIsActive {
    tx.valid := True
    tx.fragment := crc16.io.result(7 downto 0)
    tx.last := False
    when(tx.ready) {
      goto(CRC_1)
    }
  }

  CRC_1.whenIsActive {
    tx.valid := True
    tx.fragment := crc16.io.result(15 downto 8)
    tx.last := True
    when(tx.ready) {
      if(eop != null) {
        goto(EOP)
      } else {
        exitFsm()
      }
    }
  }

  if(eop != null) EOP.whenIsActive{
    when(eop){
      exitFsm()
    }
  }
}


class UsbTokenRxFsm(rx: Flow[Bits],
                    rxActive: Bool,
                    rxStuffing: Bool,
                    timeoutClear: Bool,
                    timeoutEvent: Bool) extends StateMachineSlave{
  val PID, DATA_0, DATA_1, CHECK, ERROR = new State
  setEntry(PID)

  val pid = Reg(Bits(4 bits))
  val data = Reg(Bits(11 bits))
  val address = data(0, 7 bits)
  val endpoint = U(data(7, 4 bits))

  val ok = Reg(Bool())

  val crc5 = Crc(CrcKind.usb.crc5Check, 8)
  crc5.io.flush := False
  crc5.io.input.valid := False
  crc5.io.input.payload := rx.payload

  onStart{
    ok := False
    crc5.io.flush := True
    timeoutClear := True
  }

  PID whenIsActive{
    when(rx.valid){
      pid := rx.payload(3 downto 0)
      when(rx.payload(3 downto 0) === ~rx.payload(7 downto 4)){
        goto(DATA_0)
      } otherwise{
        goto(ERROR)
      }
    }
  }

  DATA_0 whenIsActive{
    when(rx.valid){
      data(0, 8 bits) := rx.payload
      crc5.io.input.valid := True
      goto(DATA_1)
    }
  }

  DATA_1 whenIsActive{
    when(rx.valid){
      data(8, 3 bits) := rx.payload(2 downto 0)
      crc5.io.input.valid := True
      goto(CHECK)
    }
  }

  CHECK whenIsActive{
    when(!rxActive){
      when(crc5.io.result === 0x0C){
        ok := True
      }
      exitFsm()
    }
    when(rx.valid){
      goto(ERROR)
    }
  }

  ERROR whenIsActive{
    when(!rxActive){
      exitFsm()
    }
  }

  always{
    when(isStarted) {
      when(timeoutEvent) {
        exitFsm()
      }
    }
  }
}



class UsbTokenTxFsm(tx : Stream[Fragment[Bits]],
                    eop : Bool) extends StateMachineSlave{
  val pid = Bits(4 bits).assignDontCare()
  val data = Bits(11 bits).assignDontCare()

  val INIT, PID, B1, B2, EOP = new State
  setEntry(INIT)


  val crc5 = Crc(CrcKind.usb.crc5, 11)
  crc5.io.flush := False
  onStart {
    crc5.io.flush := True
  }

  crc5.io.input.valid := False
  crc5.io.input.payload := data
  INIT.whenIsActive {
    crc5.io.input.valid := True
    goto(PID)
  }

  PID.whenIsActive {
    tx.valid := True
    tx.last := False
    tx.fragment := ~pid ## pid
    when(tx.ready) {
      goto(B1)
    }
  }

  B1.whenIsActive {
    tx.valid := True
    tx.last := False
    tx.fragment := data(0, 8 bits)
    when(tx.ready) {
      goto(B2)
    }
  }

  B2.whenIsActive {
    tx.valid := True
    tx.fragment := crc5.io.result ## data(8, 3 bits)
    tx.last := True
    when(tx.ready) {
      goto(EOP)
    }
  }

  EOP.whenIsActive{
    when(eop) {
      exitFsm()
    }
  }
}
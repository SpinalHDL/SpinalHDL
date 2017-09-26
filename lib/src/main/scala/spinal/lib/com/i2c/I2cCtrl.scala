package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactory, BusSlaveFactoryAddressWrapper}
import spinal.lib.fsm.{State, StateMachine}

object I2cCtrl{
  case class I2cAddress() extends Bundle{
    val enable  = Bool
    val value = Bits(10 bits)
    val is10Bit = Bool
  }
  def driveI2cSlaveIo(io : I2cSlaveIo,busCtrl: BusSlaveFactory, baseAddress: BigInt)(generics : I2cSlaveMemoryMappedGenerics) = new Area{
    import generics._
    import io._


    val busCtrlWithOffset = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)
    val frameReset = False
    val i2cBuffer = I2c()
    i2cBuffer <> i2c

    val rxData = new Area{
      val event = RegNext(False) init(False)
      val listen = RegInit(False)
      val valid = RegInit(False)
      val value = Reg(Bits(8 bits))

      busCtrlWithOffset.write(listen, address = 8, bitOffset = 15)
      busCtrlWithOffset.read(valid, address = 8, bitOffset = 8)
      busCtrlWithOffset.read(value, address = 8, bitOffset = 0)
      valid clearWhen(busCtrlWithOffset.isReading(8))
    }

    val rxAck = new Area{
      val listen = RegInit(False)
      val valid = RegInit(False)
      val value = Reg(Bool)

      busCtrlWithOffset.write(listen, address = 12, bitOffset = 15)
      busCtrlWithOffset.read(valid, address = 12, bitOffset = 8)
      busCtrlWithOffset.read(value, address = 12, bitOffset = 0)
      valid clearWhen(busCtrlWithOffset.isReading(12))
    }

    val txData = new Area {
      val valid  = RegInit(True)
      val repeat = RegInit(True)
      val enable = RegInit(False)
      val disableOnDataConflict = Reg(Bool)
      val value  = Reg(Bits(8 bits))
      val forceDisable = False
    }

    val txAck = new Area {
      val valid  = RegInit(True)
      val repeat = RegInit(True)
      val enable = RegInit(False)
      val disableOnDataConflict = Reg(Bool)
      val value  = Reg(Bool)
      val forceAck = False
    }

    val addressFilter = if(genAddressFilter) new Area{
      val addresses = Vec(Reg(I2cAddress()))

      val state = RegInit(U"00")
      val byte0, byte1 = Reg(Bits(8 bits))
      val byte0Is10Bit = byte0(7 downto 3) === 0x1E
      when(rxData.event){
        switch(state){
          is(0){
            byte0 := rxData.value
            state := 1
          }
          is(1){
            byte1 := rxData.value
            state := 2
          }
        }
      }
      when(frameReset){
        state := 0
      }

      val hits = addresses.map(address => address.enable && Mux(!address.is10Bit,(byte0 >> 1) === address.value(6 downto 0) && state =/= 0, (byte0(2 downto 1) ## byte1) === address.value && state === 2))
      txAck.forceAck :=  byte0Is10Bit && state === 1 && addresses.map(address => address.enable && address.is10Bit && byte0(2 downto 1) === address.value(9 downto 8)).orR
    } else null

    val masterLogic = if(genMaster) new Area{
      val start = busCtrlWithOffset.createReadAndWrite(Bool, 64, 4) init(False)
      val stop = busCtrlWithOffset.createReadAndWrite(Bool, 64, 5) init(False)
      val drop = busCtrlWithOffset.createReadAndWrite(Bool, 64, 6) init(False)
      val timer = new Area{
        val value = Reg(UInt(masterGenerics.timerWidth bits))
        val tLow = busCtrlWithOffset.createWriteOnly(value, address = 80)
        val tHigh = busCtrlWithOffset.createWriteOnly(value, address = 84)
        val tBuf = busCtrlWithOffset.createWriteOnly(value, address = 88)
        val done = value === 0
        value := value - done.asUInt
      }

      val txReady = Bool //Say if the tx buffer is ready to continue

      val fsm = new StateMachine{
        val IDLE, BUSY, START, LOW, HIGH, RESTART, STOP1, STOP2, TBUF = new State
        setEntry(IDLE)

        busCtrlWithOffset.read(this.isActive(IDLE), 64, 0)

        IDLE.onEntry{
          start := False
          stop := False
          drop := False
        }
        IDLE.whenIsActive{
          when(start && !internals.inFrame){
            goto(START)
          }
        }

        START.onEntry{
          timer.value := timer.tHigh
        }
        START.whenIsActive{
          i2cBuffer.sda.write := False
          when(timer.done || !internals.sclRead){
            start := False
            goto(LOW)
          }
        }

        LOW.onEntry{
          timer.value := timer.tLow
        }
        LOW.whenIsActive{
          when(timer.done){
            when(stop) {
              txData.forceDisable := True
              goto(STOP1)
            }.elsewhen(start){
              txData.forceDisable := True
              goto(RESTART)
            }.elsewhen(internals.sclRead && txReady) {
              goto(HIGH)
            }
          } otherwise {
            i2cBuffer.scl.write := False
          }
        }

        HIGH.onEntry{
          timer.value := timer.tHigh
        }
        HIGH.whenIsActive{
          when(timer.done || !internals.sclRead){
            goto(LOW)
          }
        }

        RESTART.onEntry{
          timer.value := timer.tHigh
        }
        RESTART.whenIsActive{
          when(!internals.sclRead){ //Check for slave clock stretching
            timer.value := timer.tHigh
          }
          when(timer.done){
            goto(START)
          }
        }

        STOP1.onEntry{
          timer.value := timer.tHigh
        }
        STOP1.whenIsActive{
          i2cBuffer.scl.write := False
          i2cBuffer.sda.write := False
          when(timer.done){
            goto(STOP2)
          }
        }

        STOP2.onEntry{
          timer.value := timer.tHigh
        }
        STOP2.whenIsActive{
          i2cBuffer.sda.write := False
          when(timer.done){
            stop := False
            goto(TBUF)
          }
        }

        TBUF.onEntry{
          timer.value := timer.tBuf
        }
        TBUF.whenIsActive{
          when(timer.done){
            goto(IDLE)
          }
        }
        always{
          when(drop) {
            goto(TBUF)
          }
        }
      }
    } else null

    val dataCounter = RegInit(U"000")
    val inAckState = RegInit(False)
    val wasntAck = RegInit(False)

    if(genMaster) masterLogic.txReady :=  inAckState ? txAck.valid | txData.valid
    when(!inAckState) {
      bus.rsp.valid  := txData.valid && !(rxData.valid && rxData.listen) && bus.cmd.kind === I2cSlaveCmdMode.DRIVE
      bus.rsp.enable := txData.enable
      bus.rsp.data   := txData.value(7 - dataCounter)
      when(txData.forceDisable){
        bus.rsp.valid := True
        bus.rsp.enable := False
      }
    } otherwise {
      bus.rsp.valid  := txAck.valid && !(rxAck.valid && rxAck.listen) && bus.cmd.kind === I2cSlaveCmdMode.DRIVE
      bus.rsp.enable := txAck.enable
      bus.rsp.data   := txAck.value
      when(txAck.forceAck){
        bus.rsp.valid := True
        bus.rsp.enable := True
        bus.rsp.data := False
      }
    }
    when(wasntAck){
      bus.rsp.valid  := bus.cmd.kind === I2cSlaveCmdMode.DRIVE
      bus.rsp.enable := False
    }


    switch(bus.cmd.kind){
      is(I2cSlaveCmdMode.START){
        frameReset := True
      }
      is(I2cSlaveCmdMode.RESTART){
        frameReset := True
      }
      is(I2cSlaveCmdMode.STOP){
        frameReset := True
      }
      is(I2cSlaveCmdMode.DROP){
        rxData.listen := False
        rxAck.listen := False
        frameReset := True
      }
      is(I2cSlaveCmdMode.DRIVE){
        when(!inAckState) {
          when(txData.valid && dataCounter === 7) {
            when(!txData.repeat) {
              txData.valid := False
            }
          }
        } otherwise {
          when(txAck.valid) {
            when(!txAck.repeat) {
              txAck.valid := False
            }
          }
        }
      }
      is(I2cSlaveCmdMode.READ){
        when(!inAckState) {
          rxData.value(7 - dataCounter) := bus.cmd.data
          dataCounter := dataCounter + 1

          when(bus.rsp.data =/= bus.cmd.data){
            txData.enable clearWhen(txData.disableOnDataConflict)
            txAck.enable clearWhen(txAck.disableOnDataConflict)
          }
          when(dataCounter === U"111") {
            rxData.event := True
            rxData.valid setWhen(rxData.listen)
            inAckState := True
          }

        } otherwise {
          rxAck.valid setWhen(rxAck.listen)
          rxAck.value := bus.cmd.data
          inAckState := False
          wasntAck := bus.cmd.data
        }
      }
    }

    when(frameReset){
      inAckState := False
      dataCounter := 0
      wasntAck := False
    }

    when(bus.cmd.kind === I2cSlaveCmdMode.STOP || bus.cmd.kind === I2cSlaveCmdMode.DROP){
      txData.valid := True
      txData.enable := False
      txData.repeat := True
      txAck.valid := True
      txAck.enable := False
      txAck.repeat := True
    }

    val interruptCtrl = new Area{
      val rxDataEnable = busCtrlWithOffset.createReadAndWrite(Bool, address = 32, bitOffset = 0)
      val rxAckEnable = busCtrlWithOffset.createReadAndWrite(Bool, address = 32, bitOffset = 1)
      val txDataEnable = busCtrlWithOffset.createReadAndWrite(Bool, address = 32, bitOffset = 2)
      val txAckEnable = busCtrlWithOffset.createReadAndWrite(Bool, address = 32, bitOffset = 3)

      def i2CSlaveEvent(enableBitId : Int, flagBitId : Int, busCmd : I2cSlaveCmdMode.E) = new Area{
        val enable = busCtrlWithOffset.createReadAndWrite(Bool, address = 32, bitOffset = enableBitId)
        val flag = busCtrlWithOffset.createReadAndWrite(Bool, address = 32, bitOffset = flagBitId) setWhen(bus.cmd.kind === busCmd) clearWhen(!enable)
      }

      val start = i2CSlaveEvent(4,8,I2cSlaveCmdMode.START)
      val restart = i2CSlaveEvent(5,9,I2cSlaveCmdMode.RESTART)
      val end = i2CSlaveEvent(6,10,I2cSlaveCmdMode.STOP)
      val drop = i2CSlaveEvent(7,11,I2cSlaveCmdMode.DROP)

      val interrupt = (rxDataEnable && rxData.valid) || (rxAckEnable && rxAck.valid) ||
        (txDataEnable && !txData.valid) || (txAckEnable && !txAck.valid) ||
        (start.flag || restart.flag || end.flag || drop.flag)
    }



    busCtrlWithOffset.write(0, 0 -> txData.value, 8 -> txData.valid, 10 -> txData.repeat, 12 -> txData.disableOnDataConflict)
    busCtrlWithOffset.readAndWrite(txData.enable, address = 0, bitOffset = 9)

    busCtrlWithOffset.write(4, 0 -> txAck.value, 8 -> txAck.valid, 10 -> txAck.repeat, 12 -> txAck.disableOnDataConflict)
    busCtrlWithOffset.readAndWrite(txAck.enable, address = 4, bitOffset = 9)

    busCtrlWithOffset.drive(config.samplingClockDivider, 40) init(0)
    busCtrlWithOffset.drive(config.timeout, 44) randBoot()
    busCtrlWithOffset.drive(config.tsuDat, 48) randBoot()
  }
}

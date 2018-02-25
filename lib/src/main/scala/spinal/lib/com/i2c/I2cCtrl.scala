package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactory, BusSlaveFactoryAddressWrapper}
import spinal.lib.fsm.{EntryPoint, State, StateMachine}

object I2cCtrl{
  case class I2cAddress() extends Bundle{
    val enable  = Bool
    val value = Bits(10 bits)
    val is10Bit = Bool
  }

  /*
   * To create an I2c controller which is master/slave ready, you basicaly only need an low level I2C bus driver (I2cSlave) and add on the top
   * of it some clock generation logic/status/buffers. This is what driveI2cSlaveIo do.
   *
   * This controller logic can be instanciated as an I2C slave controller only, or as an I2C master/Slave controller.
   * There are a list of features :
   * - Slave only / Master Slave
   * - Slave address filtering (7/10 bits)
   * - Multimaster
   * - Timeout
   *
   * Master initialisation :
   * 1) Configure samplingClockDivider/timeout/tsuDat at x28/x2C/x30
   * 2) Configure tLow/tHigh/tBuf at address x50/x54/x58 (WO).   tBuf is the idle time after a STOP transmition
   *
   * Master write:
   * 1) Do a START by setting the start bit of the masterStatus register
   *   Multimaster => You have to check that the master had catch the bus via the busy flag of the masterStatus register
   * 2) Provide the address by feeding the txData/txAck register, then wait the txAck emptiness
   *   Multimaster => Don't forget the txData disableOnDataConflict flag, then to know of the arbitration had pass, check the txData enable flag
   *   Conflict => If you address negotiation had conflicted with another master, you have to set the drop flag of the masterStatus register
   *   ack check => Should had enable the listen flag of the rxAck register
   * 3) Provide the data by feeding the txData/txAck register, then wait the txAck emptiness
   * 4) Do a STOP by setting the stop bit of the masterStatus register
   *
   * Master read :
   *   Note : (multimaster operations and ack checking are the thame than the onf of 'master write')
   * 1) Do a START by setting the start bit of the masterStatus register
   * 2) Provide the address by feeding the txData/txAck register, then wait the txAck emptiness
   *   Multimaster => Same as master write
   * 3) Enable rxData then provide the data by feeding the txData/txAck register, then wait the txAck emptiness to read the rxData
   * 4) Do a STOP by setting the stop bit of the masterStatus register
   *
   *
   * Slave initialisation :
   * 1) Configure samplingClockDivider/timeout/tsuDat at x28/x2C/x30
   *
   * Slave write without hardware address filtering :
   * 1) Wait the start interrupt, clear start flag
   * 2) Disable the rxAck register (! real time !)
   * 3) Wait the rxData interrupt and check the rxData value.
   *    - Address KO, put the rxAck in an NACK repeat mode and disable the rxData listen, then return to 1)
   *    - Address OK, set the rxAck to ACK
   * 4) wait for rxData interrupt, then set rxAck to NACK to end the transfer
   *
   *
   * Slave read without hardware address filtering :
   * 1) Wait the start interrupt, clear start flag
   * 2) Disable the rxAck register (! real time !)
   * 3) Wait the rxData interrupt and check the rxData value.
   *    - Address KO, put the rxAck in an NACK repeat mode and disable the rxData listen, then return to 1)
   *    - Address OK, disable the txData, set the rxAck to ACK
   * 4) set the txData, set the rxAck to disable
   *
   * Slave address filtering
   * 1) Set the address filtring registers
   * 2) wait for a txAck interrupt, then check for the hits flags into the filteringStatus register to identify which filter had hit
   * 3) Continue has the 3) address OK of Slave read/write without hardware address filtering. You can get the RW i2c frame bit in the hitContext register
   *
   *
   * Register mapping :
   *
   * txData -> 0x00
   * - value -> W[7:0]
   * - valid -> RW[8]
   * - enable -> RW[9]
   * - repeat -> W[10]
   * - disableOnDataConflict -> W[11]
   *
   * txAck -> 0x04
   * - value -> W[0]
   * - valid -> RW[8]
   * - enable -> RW[9]
   * - repeat -> W[10]
   * - disableOnDataConflict -> W[11]
   *
   * rxData -> 0x08
   * - value -> R[7:0]
   * - valid -> R[8]
   * - listen -> W[9], cleared when read
   *
   * rxAck -> 0x0C
   * - value -> R[0]
   * - valid -> R[8]
   * - listen -> W[9], cleared when read
   * 
   * interrupt -> 0x20
   * - rxDataEnable -> RW[0]
   * - rxAckEnable -> RW[1]
   * - txDataEnable -> RW[2]
   * - txAckEnable -> RW[3]
   *
   * - startEnable -> RW[4]
   * - restartEnable -> RW[5]
   * - endEnable -> RW[6]
   * - dropEnable -> RW[7]
   *
   * - startFlag -> RW[8] cleared when set
   * - restartFlag -> RW[9] cleared when set
   * - endFlag -> RW[10] cleared when set
   * - dropFlag -> RW[11] cleared when set
   *
   * - clockGenBusyEnable -> RW[16]
   *
   * samplingClockDivider -> W 0x28
   * timeout -> W 0x2C
   * tsuDat -> W 0x30
   *
   * masterStatus -> 0x40
   * - isBusy -> R[0]
   * - start -> W[4]
   * - stop -> W[5]
   * - drop -> W[6]
   *
   * tLow -> W 0x50
   * tHigh -> W 0x54
   * tBuf -> W 0x58
   *
   * filteringStatus -> 0x80
   * - hit_0 -> R[0]
   * - hit_1 -> R[1]
   * - hit_N -> R[N]
   *
   * hitContext -> 0x84
   * - rw -> R[0]
   *
   * filteringConfig_0 -> 0x88
   * - value -> W[9:0]
   * - is10Bit -> W[14]
   * - enable -> W[15]
   *
   * filteringConfig_N -> 0x88 + 4*N
   *
  **/

  def driveI2cSlaveIo(io: I2cSlaveIo, busCtrl: BusSlaveFactory, baseAddress: BigInt)(generics: I2cSlaveMemoryMappedGenerics) = new Area {

    import generics._
    import io._


    val busCtrlWithOffset = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)
    val frameReset = False

    val i2cBuffer = I2c()
    i2cBuffer <> i2c

    val rxData = new Area  {
      val event  = RegInit(False)
      val listen = RegInit(False)
      val valid  = RegInit(False)
      val value  = Reg(Bits(8 bits))

      busCtrlWithOffset.write(listen, address = 0x08, bitOffset = 9)
      busCtrlWithOffset.read(valid,   address = 0x08, bitOffset = 8)
      busCtrlWithOffset.read(value,   address = 0x08, bitOffset = 0)

      valid clearWhen(busCtrlWithOffset.isReading(0x08))
    }

    val rxAck = new Area {
      val listen = RegInit(False)
      val valid  = RegInit(False)
      val value  = Reg(Bool)

      busCtrlWithOffset.write(listen, address = 0x0C, bitOffset = 9)
      busCtrlWithOffset.read(valid,   address = 0x0C, bitOffset = 8)
      busCtrlWithOffset.read(value,   address = 0x0C, bitOffset = 0)

      valid clearWhen(busCtrlWithOffset.isReading(0x0C))
    }

    val txData = new Area {
      val valid        = RegInit(True)
      val repeat       = RegInit(True)
      val enable       = RegInit(False)
      val value        = Reg(Bits(8 bits))
      val forceDisable = False
      val disableOnDataConflict = Reg(Bool)

      busCtrlWithOffset.write(0x00, 0 -> value, 10 -> repeat, 11 -> disableOnDataConflict)
      busCtrlWithOffset.readAndWrite(valid,  address = 0x00, bitOffset = 8)
      busCtrlWithOffset.readAndWrite(enable, address = 0x00, bitOffset = 9)
    }

    val txAck = new Area {
      val valid    = RegInit(True)
      val repeat   = RegInit(True)
      val enable   = RegInit(False)
      val value    = Reg(Bool)
      val forceAck = False
      val disableOnDataConflict = Reg(Bool)

      busCtrlWithOffset.write(0x04, 0 -> value, 10 -> repeat, 11 -> disableOnDataConflict)
      busCtrlWithOffset.readAndWrite(valid,  address = 0x04, bitOffset = 8)
      busCtrlWithOffset.readAndWrite(enable, address = 0x04, bitOffset = 9)
    }

    /**
      * Address filtering
      */
    val addressFilter = if(genAddressFilter) new Area {

      val addresses = Vec(Reg(I2cAddress()), addressFilterCount)

      for((address, idx) <- addresses.zipWithIndex){
        address.enable init(False)

        busCtrlWithOffset.write(address.value,   0x88 + 4 * idx,  0)
        busCtrlWithOffset.write(address.is10Bit, 0x88 + 4 * idx, 14)
        busCtrlWithOffset.write(address.enable,  0x88 + 4 * idx, 15)
      }

      val state        = RegInit(U"00")
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
      txAck.forceAck setWhen(byte0Is10Bit && state === 1 && addresses.map(address => address.enable && address.is10Bit && byte0(2 downto 1) === address.value(9 downto 8)).orR)
      busCtrlWithOffset.read(hits.asBits, 0x80, 0)
      busCtrlWithOffset.read(byte0.lsb,   0x84, 0)

      when(hits.orR.rise()){
        txAck.valid := False
      }
    } else null


    /**
      * Master logic
      */
    val masterLogic = if(genMaster) new Area {

      val start = busCtrlWithOffset.createReadAndWrite(Bool, 0x40, 4) init(False)
      val stop  = busCtrlWithOffset.createReadAndWrite(Bool, 0x40, 5) init(False)
      val drop  = busCtrlWithOffset.createReadAndWrite(Bool, 0x40, 6) init(False)


      val timer = new Area {

        val value = Reg(UInt(masterGenerics.timerWidth bits))

        val tLow  = busCtrlWithOffset.createWriteOnly(value, address = 0x50)
        val tHigh = busCtrlWithOffset.createWriteOnly(value, address = 0x54)
        val tBuf  = busCtrlWithOffset.createWriteOnly(value, address = 0x58)

        val done = value === 0

        value := value - (!done).asUInt
      }


      val txReady = Bool //Say if the tx buffer is ready to continue

      val fsm = new StateMachine {

        always{
          when(drop) {
            goto(TBUF)
          }
        }

        val IDLE: State = new State with EntryPoint {
          onEntry {
            start := False
            stop  := False
            drop  := False
          }
          whenIsActive {
            when(start && !internals.inFrame) {
              goto(START)
            }
          }
        }

        val START: State = new State {
          onEntry {
            timer.value := timer.tHigh
          }
          whenIsActive {
            i2cBuffer.sda.write := False
            when(timer.done || !internals.sclRead) {
              start := False
              goto(LOW)
            }
          }
        }

        val LOW: State = new State {
          onEntry {
            timer.value := timer.tLow
          }
          whenIsActive {
            when(timer.done) {
              //            val couldBeEnd =  !inAckState && dataCounter === 0 && !txData.valid
              when(stop) {
                txData.forceDisable := True
                goto(STOP1)
              } elsewhen (start) {
                txData.forceDisable := True
                goto(RESTART)
              } elsewhen (internals.sclRead && txReady) {
                goto(HIGH)
              }
            } otherwise {
              i2cBuffer.scl.write := False
            }
          }
        }

        val HIGH: State = new State {
          onEntry {
            timer.value := timer.tHigh
          }
          whenIsActive {
            when(timer.done || !internals.sclRead) {
              goto(LOW)
            }
          }
        }

        val RESTART: State = new State {
          onEntry {
            timer.value := timer.tHigh
          }
          whenIsActive {
            when(!internals.sclRead) { //Check for slave clock stretching
              timer.value := timer.tHigh
            }
            when(timer.done) {
              goto(START)
            }
          }
        }

        val STOP1: State = new State {
          onEntry {
            timer.value := timer.tHigh
          }
          whenIsActive {
            i2cBuffer.scl.write := False
            i2cBuffer.sda.write := False
            when(timer.done) {
              goto(STOP2)
            }
          }
        }

        val STOP2: State = new State {
          onEntry {
            timer.value := timer.tHigh
          }
          whenIsActive {
            i2cBuffer.sda.write := False
            when(timer.done) {
              stop := False
              goto(TBUF)
            }
          }
        }

        val TBUF: State = new State {
          onEntry {
            timer.value := timer.tBuf
          }
          whenIsActive {
            when(timer.done) {
              goto(IDLE)
            }
          }
        }

        val isBusy = !this.isActive(IDLE)

        busCtrlWithOffset.read(isBusy, 0x40, 0)
      }
    } else null

    val dataCounter = RegInit(U"000")
    val inAckState  = RegInit(False)
    val wasntAck    = RegInit(False)

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
        bus.rsp.valid  := True
        bus.rsp.enable := True
        bus.rsp.data   := False
      }
    }

    val isMasterMode = if(masterLogic != null) masterLogic.fsm.isBusy else False

    when(wasntAck && !isMasterMode){
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
        rxAck.listen  := False
        frameReset    := True
      }
      is(I2cSlaveCmdMode.DRIVE){
        when(!inAckState) {
          when(txData.valid && dataCounter === 7) {
            when(!txData.repeat) {
              txData.valid := False
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
            txAck.enable  clearWhen(txAck.disableOnDataConflict)
          }
          when(dataCounter === U"111"){
            rxData.valid setWhen(rxData.listen)
            rxData.event := True
            inAckState   := True
          }

        } otherwise {
          rxAck.valid setWhen(rxAck.listen)
          rxAck.value := bus.cmd.data
          inAckState  := False
          wasntAck    := bus.cmd.data

          when(txAck.valid) {
            when(!txAck.repeat) {
              txAck.valid := False
            }
          }
        }
      }
    }

    when(frameReset){
      inAckState  := False
      dataCounter := 0
      wasntAck    := False
    }

    when(bus.cmd.kind === I2cSlaveCmdMode.STOP || bus.cmd.kind === I2cSlaveCmdMode.DROP){
      txData.valid  := True
      txData.enable := False
      txData.repeat := True

      txAck.valid   := True
      txAck.enable  := False
      txAck.repeat  := True

      rxData.listen := False

      rxAck.listen  := False
    }

    /**
      * Interrupt Controller
      */
    val interruptCtrl = new Area {

      val rxDataEnable = busCtrlWithOffset.createReadAndWrite(Bool, address = 0x20, bitOffset = 0)  init(False)
      val rxAckEnable  = busCtrlWithOffset.createReadAndWrite(Bool, address = 0x20, bitOffset = 1)  init(False)
      val txDataEnable = busCtrlWithOffset.createReadAndWrite(Bool, address = 0x20, bitOffset = 2)  init(False)
      val txAckEnable  = busCtrlWithOffset.createReadAndWrite(Bool, address = 0x20, bitOffset = 3)  init(False)

      def i2CSlaveEvent(enableBitId : Int, flagBitId : Int, busCmd : I2cSlaveCmdMode.E) = new Area{
        val enable = busCtrlWithOffset.createReadAndWrite(Bool, address = 0x20, bitOffset = enableBitId) init(False)
        val flag   = busCtrlWithOffset.readAndClearOnSet(RegInit(False) setWhen(bus.cmd.kind === busCmd) clearWhen(!enable),  address = 0x20, bitOffset = flagBitId)
      }

      val start   = i2CSlaveEvent(4,  8, I2cSlaveCmdMode.START)
      val restart = i2CSlaveEvent(5,  9, I2cSlaveCmdMode.RESTART)
      val end     = i2CSlaveEvent(6, 10, I2cSlaveCmdMode.STOP)
      val drop    = i2CSlaveEvent(7, 11, I2cSlaveCmdMode.DROP)

      val interrupt = (rxDataEnable && rxData.valid) || (rxAckEnable && rxAck.valid)   ||
                      (txDataEnable && !txData.valid) || (txAckEnable && !txAck.valid) ||
                      (start.flag || restart.flag || end.flag || drop.flag)

      val clockGen = if(genMaster) new Area{
        val busyEnable = busCtrlWithOffset.createReadAndWrite(Bool, address = 0x20, bitOffset = 16) init(False)
        interrupt setWhen((busyEnable && masterLogic.fsm.isBusy))
      } else null
    }


    busCtrlWithOffset.drive(config.samplingClockDivider, 0x28) init(0)
    busCtrlWithOffset.drive(config.timeout,  0x2C) randBoot()
    busCtrlWithOffset.drive(config.tsuData , 0x30) randBoot()
  }
}

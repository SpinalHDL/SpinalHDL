/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Lib                          **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/  MIT Licence                      **
**                                                                           **
** Permission is hereby granted, free of charge, to any person obtaining a   **
** copy of this software and associated documentation files (the "Software"),**
** to deal in the Software without restriction, including without limitation **
** the rights to use, copy, modify, merge, publish, distribute, sublicense,  **
** and/or sell copies of the Software, and to permit persons to whom the     **
** Software is furnished to do so, subject to the following conditions:      **
**                                                                           **
** The above copyright notice and this permission notice shall be included   **
** in all copies or substantial portions of the Software.                    **
**                                                                           **
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS   **
** OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF                **
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.    **
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY      **
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT **
** OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR  **
** THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                **
\*                                                                           */
package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactory, BusSlaveFactoryAddressWrapper}
import spinal.lib.fsm.{EntryPoint, State, StateMachine}



object I2cCtrl {

  case class I2cAddress() extends Bundle {
    val enable  = Bool()
    val value   = Bits(10 bits)
    val is10Bit = Bool()
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
   * - valid -> R[8], cleared when read
   * - listen -> W[9]
   *
   * rxAck -> 0x0C
   * - value -> R[0]
   * - valid -> R[8], cleared when read
   * - listen -> W[9]
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
   * - recoverEnable -> RW[8]
   *
   * - clockGenExitEnable -> RW[15]
   * - clockGenEnterEnable -> RW[16]
   * - filterEnable -> RW[17]
   *
   *
   * interrupt clears -> 0x24
   * - startFlag -> RW[4] interrupt flag, clear on set
   * - restartFlag -> RW[5] interrupt flag, clear on set
   * - stopFlag -> RW[6] interrupt flag, clear on set
   * - dropFlag -> RW[7] interrupt flag, clear on set
   *
   * - clockGenExitFlag -> RW[15] interrupt flag, clear on set
   * - clockGenEnterFlag -> RW[16] interrupt flag, , clear on set
   * - filterFlag -> RW[17] interrupt flag, clear on set
   *
   * samplingClockDivider -> W 0x28
   * timeout -> W 0x2C
   * tsuDat -> W 0x30
   *
   * masterStatus -> 0x40
   * - isBusy -> R[0]
   * - start -> RW[4], set on set, order a start
   * - stop -> RW[5] , set on set, order a stop
   * - drop -> RW[6] , set on set, order a drop
   * - recover -> RW[7] , set on set, order a drop
   * - startDropped -> RW[9]  clear on set, indicate if timeout came durring start
   * - stopDropped  -> RW[10] clear on set, indicate if timeout came durring stop
   * - recoverDropped  -> RW[10] clear on set, indicate if timeout came durring recover
   *
   * tLow -> W 0x50
   * tHigh -> W 0x54
   * tBuf -> W 0x58
   *
   * slaveStatus -> 0x44
   * - inFrame -> R[0]
   * - sda -> R[1]
   * - scl -> R[2]
   *
   * slaveOverride -> 0x48
   * - sda -> RW[1] : Force the SDA pin low when cleared
   * - scl -> RW[2] : Force the SCL pin low when cleared
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



    val rxData = new Area {
      val event  = RegNext(False) init(False)
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
      val value  = Reg(Bool())

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
      val disableOnDataConflict = Reg(Bool())

      busCtrlWithOffset.write(0x00, 0 -> value, 10 -> repeat, 11 -> disableOnDataConflict)
      busCtrlWithOffset.readAndWrite(valid,  address = 0x00, bitOffset = 8)
      busCtrlWithOffset.readAndWrite(enable, address = 0x00, bitOffset = 9)
    }


    val txAck = new Area {
      val valid    = RegInit(True)
      val repeat   = RegInit(True)
      val enable   = RegInit(False)
      val value    = Reg(Bool())
      val forceAck = False
      val disableOnDataConflict = Reg(Bool())

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
    val masterLogic = genMaster generate new Area {

      val start = busCtrlWithOffset.createReadAndSetOnSet(Bool(), 0x40, 4) init(False)
      val stop  = busCtrlWithOffset.createReadAndSetOnSet(Bool(), 0x40, 5) init(False)
      val drop  = busCtrlWithOffset.createReadAndSetOnSet(Bool(), 0x40, 6) init(False)
      val recover  = busCtrlWithOffset.createReadAndSetOnSet(Bool(), 0x40, 7) init(False)

      val timer = new Area {

        val value = Reg(UInt(masterGenerics.timerWidth bits))

        val tLow  = busCtrlWithOffset.createWriteOnly(value, address = 0x50)
        val tHigh = busCtrlWithOffset.createWriteOnly(value, address = 0x54)
        val tBuf  = busCtrlWithOffset.createWriteOnly(value, address = 0x58)

        val done = value === 0

        value := value - (!done).asUInt
      }


      val txReady = Bool() //Say if the tx buffer is ready to continue

      val fsm = new StateMachine {
        val dropped = new Area{
          val start = RegInit(False)
          val stop  = RegInit(False)
          val recover  = RegInit(False)
          val trigger = False
        }
        always {
          when(drop || (!isActive(IDLE) && (bus.cmd.kind === I2cSlaveCmdMode.DROP || io.timeout))) {
            start := False
            stop := False
            drop := False
            recover := False
            dropped.start setWhen(start)
            dropped.stop  setWhen(stop)
            dropped.trigger := True
            goto(TBUF)
          }
        }


        val inFrameLate = Reg(Bool()) setWhen(!internals.sclRead) clearWhen(!internals.inFrame) //Allow to catch up a start sequance until SCL is low
        val outOfSync = !internals.inFrame && (!internals.sdaRead || !internals.sclRead)
        val IDLE: State = new State with EntryPoint {
          whenIsActive {
            when(internals.inFrame.fall(False)){
              goto(TBUF)
            } elsewhen(start && !inFrameLate){
              txData.valid := False
              goto(START1)
            } elsewhen(recover){
              goto(LOW)
            }
          }
        }

        val START1: State = new State {
          whenIsActive {
            when(!outOfSync) {
              goto(START2)
            }
          }
        }

        val START2: State = new State {
          onEntry {
            timer.value := timer.tHigh
          }
          whenIsActive {
            i2cBuffer.sda.write := False
            when(timer.done || !internals.sclRead) {
              goto(START3)
            }
          }
        }

        val START3: State = new State {
          onEntry {
            timer.value := timer.tLow
          }
          whenIsActive {
            i2cBuffer.sda.write := False
            i2cBuffer.scl.write := False
            when(timer.done) {
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
              when(stop && !inAckState || recover && internals.sdaRead) {
                i2cBuffer.scl.write := False
                txData.forceDisable := True
                goto(STOP1)
              } elsewhen (start && !inAckState) {
                i2cBuffer.scl.write := False
                txData.forceDisable := True
                goto(RESTART)
              } otherwise {
                when (internals.sclRead) {
                  goto(HIGH)
                }
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
          whenIsActive {
            when(!internals.sclRead) { //Check for slave clock stretching
              timer.value := timer.tHigh
            }elsewhen(timer.done) {
              goto(START1)
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
          whenIsActive {
            i2cBuffer.sda.write := False
            when(!internals.sclRead){
              timer.value := timer.tHigh
            } elsewhen(timer.done) {
              goto(STOP3)
            }
          }
        }

        val STOP3: State = new State {
          whenIsActive {
            when(internals.sdaRead) {
              stop := False
              recover := False
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

        val isBusy = !this.isActive(IDLE) && !this.isActive(TBUF)

        busCtrlWithOffset.read(isBusy, 0x40, 0)
        busCtrlWithOffset.read(dropped.start, 0x40, 9)
        busCtrlWithOffset.read(dropped.stop, 0x40, 10)
        busCtrlWithOffset.read(dropped.recover, 0x40, 11)
        busCtrlWithOffset.clearOnSet(dropped.start, 0x40, 9)
        busCtrlWithOffset.clearOnSet(dropped.stop, 0x40, 10)
        busCtrlWithOffset.clearOnSet(dropped.recover, 0x40, 11)
      }
    }


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
        frameReset    := True
      }
      is(I2cSlaveCmdMode.READ){
        when(!inAckState) {
          rxData.value(7 - dataCounter) := bus.cmd.data
          dataCounter := dataCounter + 1

          when(bus.rsp.data =/= bus.cmd.data){
            txData.enable clearWhen(txData.disableOnDataConflict)
            txAck.enable  clearWhen(txAck.disableOnDataConflict)
          }
          when(dataCounter === 7){
            rxData.valid setWhen(rxData.listen)
            rxData.event := True
            inAckState   := True
            when(txData.valid && !txData.repeat) {
              txData.valid := False
            }
          }
        } otherwise {
          rxAck.valid setWhen(rxAck.listen)
          rxAck.value := bus.cmd.data
          inAckState  := False
          wasntAck    := bus.cmd.data

          when(txAck.valid && !txAck.repeat) {
            txAck.valid := False
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
      txData.forceDisable := False
      txData.disableOnDataConflict := False

      txAck.valid   := True
      txAck.enable  := False
      txAck.repeat  := True
      txAck.disableOnDataConflict := False

      rxData.listen := False
      rxAck.listen  := False
    }


    /**
      * Interrupt Controller
      */
    val interruptCtrl = new Area {

      val rxDataEnable = busCtrlWithOffset.createReadAndWrite(Bool(), address = 0x20, bitOffset = 0)  init(False)
      val rxAckEnable  = busCtrlWithOffset.createReadAndWrite(Bool(), address = 0x20, bitOffset = 1)  init(False)
      val txDataEnable = busCtrlWithOffset.createReadAndWrite(Bool(), address = 0x20, bitOffset = 2)  init(False)
      val txAckEnable  = busCtrlWithOffset.createReadAndWrite(Bool(), address = 0x20, bitOffset = 3)  init(False)


      val interrupt = (rxDataEnable && rxData.valid) || (rxAckEnable && rxAck.valid)   ||
                      (txDataEnable && !txData.valid) || (txAckEnable && !txAck.valid)

      def i2CSlaveEvent(bitId: Int, cond : Bool) = new Area {
        val enable = busCtrlWithOffset.createReadAndWrite(Bool(), address = 0x20, bitOffset = bitId) init(False)
        val flag   = busCtrlWithOffset.read(RegInit(False) setWhen(cond) clearWhen(!enable),  address = 0x24, bitOffset = bitId)

        busCtrlWithOffset.clearOnSet(flag, 0x24, bitId)
        interrupt.setWhen(flag)
      }

      val start   = i2CSlaveEvent(4, bus.cmd.kind === I2cSlaveCmdMode.START)
      val restart = i2CSlaveEvent(5, bus.cmd.kind === I2cSlaveCmdMode.RESTART)
      val end     = i2CSlaveEvent(6, bus.cmd.kind === I2cSlaveCmdMode.STOP)
      val drop    = i2CSlaveEvent(7, bus.cmd.kind === I2cSlaveCmdMode.DROP || genMaster.mux(masterLogic.fsm.dropped.trigger, False))

      val filterGen = genAddressFilter generate i2CSlaveEvent(17, addressFilter.hits.orR.rise())

      val clockGenExit = genMaster generate i2CSlaveEvent(15, masterLogic.fsm.isBusy.fall())
      val clockGenEnter = genMaster generate i2CSlaveEvent(16, masterLogic.fsm.isBusy.rise())
    }


    busCtrlWithOffset.drive(config.samplingClockDivider, 0x28) init(0)
    busCtrlWithOffset.drive(config.timeout,  0x2C) randBoot()
    busCtrlWithOffset.drive(config.tsuData , 0x30) randBoot()

    val timeoutClear = RegNext(False)
    config.timeoutClear := timeoutClear
    busCtrlWithOffset.onWrite(0x2C)(timeoutClear := True)
    if(genMaster) config.timeoutClear setWhen(!internals.inFrame && !masterLogic.fsm.isBusy)

    busCtrlWithOffset.read(
      0x44,
      0 -> internals.inFrame,
      1 -> internals.sdaRead,
      2 -> internals.sclRead
    )


    if(genMaster) masterLogic.fsm.build()
    val slaveOverride = new Area{
      val sda = busCtrlWithOffset.createReadAndWrite(Bool(), 0x48, 1) init(True)
      val scl = busCtrlWithOffset.createReadAndWrite(Bool(), 0x48, 2) init(True)
      i2cBuffer.sda.write clearWhen(!sda)
      i2cBuffer.scl.write clearWhen(!scl)
    }
  }
}

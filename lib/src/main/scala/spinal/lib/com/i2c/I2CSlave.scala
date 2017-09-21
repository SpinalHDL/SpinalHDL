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
import spinal.lib.bus.misc.{BusSlaveFactoryAddressWrapper, BusSlaveFactory}
import spinal.lib.fsm.{State, StateMachine}


/**
  * Generics for the I2C Slave
  *
  * @param samplingWindowSize              : deepth sampling
  * @param samplingClockDividerWidth : Width of the clock divider
  */
case class I2cSlaveGenerics(samplingWindowSize        : Int = 3,
                            samplingClockDividerWidth : BitCount = 10 bits,
                            tsuDatWidth               : BitCount = 6 bits, //Data set-up time width
                            timeoutWidth              : BitCount = 20 bits){}


/**
  * Run-time configuration for the I2CSlave
  */
case class I2cSlaveConfig(g: I2cSlaveGenerics) extends Bundle {

  val samplingClockDivider = UInt(g.samplingClockDividerWidth)
  val timeout              = UInt(g.timeoutWidth)
  val tsuDat               = UInt(g.tsuDatWidth)


  def setFrequencySampling(frequencySampling : HertzNumber, clkFrequency : HertzNumber = ClockDomain.current.frequency.getValue): Unit = {
    samplingClockDivider := (clkFrequency / frequencySampling).toInt
  }
  def setTimeoutPeriod(period : TimeNumber, clkFrequency : HertzNumber = ClockDomain.current.frequency.getValue): Unit = {
    timeout := (period*clkFrequency).toInt
  }
}

/**
 * Mode used to manage the slave
 */
object I2cSlaveCmdMode extends SpinalEnum{
  val NONE, START, RESTART, STOP, DROP, DRIVE, READ = newElement()
}


/**
 * Define the command interface
 */
case class I2cSlaveCmd() extends Bundle{
  val kind = I2cSlaveCmdMode()
  val data  = Bool
}

case class I2cSlaveRsp() extends Bundle{
  val valid = Bool
  val enable = Bool
  val data = Bool
}

case class I2cSlaveBus() extends Bundle with IMasterSlave{
  val cmd = I2cSlaveCmd()
  val rsp = I2cSlaveRsp()

  override def asMaster(): Unit = {
    out(cmd)
    in(rsp)
  }
}


case class I2cSlaveMemoryMappedGenerics(ctrlGenerics : I2cSlaveGenerics,
                                         masterGenerics : I2cMasterMemoryMappedGenerics = null){
  def genMaster = masterGenerics != null
}


case class I2cMasterMemoryMappedGenerics( timerWidth : Int)



case class I2cSlaveIo(g : I2cSlaveGenerics) extends Bundle {
  val i2c = master(I2c())
  val config = in(I2cSlaveConfig(g))
  val bus = master(I2cSlaveBus())
  val internals = out(new Bundle {
    val inFrame = Bool
    val sdaRead, sclRead = Bool
  })

  def driveFrom(busCtrl: BusSlaveFactory, baseAddress: BigInt)(generics : I2cSlaveMemoryMappedGenerics) = new Area{
    import generics._

    val busCtrlWithOffset = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)

    val i2cBuffer = I2c()
    i2cBuffer <> i2c

    val rxData = new Area{
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
    }

    val txAck = new Area {
      val valid  = RegInit(True)
      val repeat = RegInit(True)
      val enable = RegInit(False)
      val disableOnDataConflict = Reg(Bool)
      val value  = Reg(Bool)
    }

    val masterLogic = if(genMaster) new Area{
      object State extends SpinalEnum{
        val IDLE, BUSY, START, LOW, HIGH, STOP = newElement()
      }
      val state = Reg(State) init(State.IDLE)

      busCtrlWithOffset.read(state =/= State.IDLE, 64, 0)
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

     // val sclWrite, sdaWrite = True
      val txReady = Bool //Say if the tx buffer is ready to continue

      val fsm = new StateMachine{
        val IDLE, BUSY, START, LOW, HIGH, RESTART, STOP1, STOP2, TBUF = new State
        setEntry(IDLE)

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
              stop := False
              goto(STOP1)
            }.elsewhen(start){
              start := False
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
    val frameReset = False
    val wasntAck = RegInit(False)

    if(genMaster) masterLogic.txReady :=  inAckState ? txAck.valid | txData.valid
    when(!inAckState) {
      bus.rsp.valid  := txData.valid && !(rxData.valid && rxData.listen) && bus.cmd.kind === I2cSlaveCmdMode.DRIVE
      bus.rsp.enable := txData.enable
      bus.rsp.data   := txData.value(7 - dataCounter)
    } otherwise {
      bus.rsp.valid  := txAck.valid && !(rxAck.valid && rxAck.listen) && bus.cmd.kind === I2cSlaveCmdMode.DRIVE
      bus.rsp.enable := txAck.enable
      bus.rsp.data   := txAck.value
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

      def eventBuild(enableBitId : Int, flagBitId : Int, busCmd : I2cSlaveCmdMode.E) = new Area{
        val enable = busCtrlWithOffset.createReadAndWrite(Bool, address = 32, bitOffset = enableBitId)
        val flag = busCtrlWithOffset.createReadAndWrite(Bool, address = 32, bitOffset = flagBitId) setWhen(bus.cmd.kind === busCmd) clearWhen(!enable)
      }
      val start = eventBuild(4,8,I2cSlaveCmdMode.START)
      val restart = eventBuild(5,9,I2cSlaveCmdMode.RESTART)
      val end = eventBuild(6,10,I2cSlaveCmdMode.STOP)
      val drop = eventBuild(7,11,I2cSlaveCmdMode.DROP)


      val interrupt = (rxDataEnable && rxData.valid) || (rxAckEnable && rxAck.valid) ||
                      (txDataEnable && !txData.valid) || (txAckEnable && !txAck.valid) ||
                      (start.flag || restart.flag || end.flag || drop.flag)

//      val master = if(genMaster) new Area{
//        val onBusEnable = busCtrlWithOffset.createReadAndWrite(Bool, address = 32, bitOffset = 16)
//        val onBusEnable = busCtrlWithOffset.createReadAndWrite(Bool, address = 32, bitOffset = 16)
//
//      } else null
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


/**
  * I2C Slave IO Layer :
  *
  *  This component manages the low level of the I2C protocol. (START, STOP, Send & Receive bit data)
  *
  *
  *
  *          ________                       ________
  *         |        |<------- I2C ------->|        |---> CMD
  *         | Master |                     |  Slave |
  *         |________|                     |________|<--- RSP
  *
  *
  * 3 bit frame =>  :
  *              |       |       |       |       |       |       |       |       |
  *   CMD    :   START   DRIVE   READ    DRIVE   READ    DRIVE   READ    DRIVE   STOP
  *   RSP    :   |       |RSP    |       |RSP    |       |RSP   |
  */
class I2cSlave(g : I2cSlaveGenerics) extends Component{

  import spinal.lib.com.i2c.{I2cSlaveCmdMode => CmdMode}

  /**
    * Interface of the I2C Hal slave
    */
  val io = I2cSlaveIo(g)

  /**
    * Filter SDA and SCL input
    */
  val filter = new I2cIoFilter(i2c               = io.i2c,
                               clockDivider      = io.config.samplingClockDivider,
                               samplingSize      = g.samplingWindowSize,
                               clockDividerWidth = g.samplingClockDividerWidth)

  /**
    * Detect the rising and falling edge of the scl signal
    */
  val sclEdge = new I2cEdgeDetector(filter.scl)
  val sdaEdge = new I2cEdgeDetector(filter.sda)


  /**
    * Detect the start/restart and the stop sequences
    */
  val detector = new Area{
    val start = filter.scl && sdaEdge.falling
    val stop  = filter.scl && sdaEdge.rising
  }

  val tsuDat = new Area{
    val counter = Reg(UInt(g.tsuDatWidth)) init(0)
    val done = counter === 0
    val reset = False
    when(!done) {
      counter := counter - 1
    }
    when(reset){
      counter := io.config.tsuDat
    }
  }


  /**
    * Slave controller
    */
  val ctrl = new Area{
    val inFrame, inFrameData = Reg(Bool) init(False)
    val sdaWrite, sclWrite = True

    //Create a bus RSP buffer
    case class Rsp() extends Bundle{
      val enable = Bool
      val data = Bool
    }
    val rspBufferIn = Stream(Rsp())
    val rspBuffer = rspBufferIn.stage() //Store rsp transaction
    val rspAhead = rspBuffer.valid ? rspBuffer.asFlow | rspBufferIn.asFlow
    rspBufferIn.valid := io.bus.rsp.valid
    rspBufferIn.enable := io.bus.rsp.enable
    rspBufferIn.data := io.bus.rsp.data
    rspBuffer.ready := False

    // default value
    io.bus.cmd.kind := CmdMode.NONE
    io.bus.cmd.data  := filter.sda

    // Send & Receive bit data
    when(inFrame) {
      when(sclEdge.rising) {
        io.bus.cmd.kind := CmdMode.READ
      }

      when(sclEdge.falling) {
        inFrameData  := True
        rspBuffer.ready := True //Flush
      }
    }

    when(inFrameData) {
      when(!rspBuffer.valid){
        io.bus.cmd.kind := CmdMode.DRIVE
      }

      when(!rspAhead.valid  || (rspAhead.enable && !tsuDat.done)) {
        sclWrite := False
      }

      tsuDat.reset := !rspAhead.valid

      when(rspAhead.valid && rspAhead.enable){
        sdaWrite := rspAhead.data
      }
    }

    when(detector.start){
      io.bus.cmd.kind   := inFrame ? CmdMode.RESTART | CmdMode.START
      inFrame := True
      inFrameData := False
    }
  }

  val timeout = new Area{
    val counter = Reg(UInt(g.timeoutWidth)) init(0)
    val tick = counter === 0
    counter := counter - 1
    when(sclEdge.toogle || !ctrl.inFrame){
      counter := io.config.timeout
      tick := False
    }
  }

  when(detector.stop || timeout.tick){
    when(ctrl.inFrame) {
      io.bus.cmd.kind := timeout.tick ? CmdMode.DROP | CmdMode.STOP
    }
    ctrl.inFrame     := False
    ctrl.inFrameData := False
  }

  io.internals.inFrame := ctrl.inFrame
  io.internals.sdaRead := filter.sda
  io.internals.sclRead := filter.scl


  /*
   * Drive SCL & SDA signals
   */
  io.i2c.scl.write := RegNext(ctrl.sclWrite)
  io.i2c.sda.write := RegNext(ctrl.sdaWrite)
}
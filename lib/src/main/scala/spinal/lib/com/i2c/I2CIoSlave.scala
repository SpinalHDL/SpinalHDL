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
import spinal.lib.bus.misc.BusSlaveFactory


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




case class I2cSlaveIo(g : I2cSlaveGenerics) extends Bundle {
  val i2c = master(I2c())
  val config = in(I2cSlaveConfig(g))
  val bus = master(I2cSlaveBus())

  def driveFrom(busCtrl: BusSlaveFactory, baseAddress: BigInt) = new Area{
    val rxData = new Area{
      val listen = RegInit(False)
      val listenOnStart = Reg(Bool)
      val valid = RegInit(False)
      val value = Reg(Bits(8 bits))

      busCtrl.read(valid, address = baseAddress + 0, bitOffset = 16)
      busCtrl.read(value, address = baseAddress + 0, bitOffset = 0)
      valid clearWhen(busCtrl.isReading(baseAddress + 0))
    }

    val rxAck = new Area{
      val listen = RegInit(False)
      val listenOnStart = Reg(Bool)
      val valid = RegInit(False)
      val value = Reg(Bool)

      busCtrl.read(valid, address = baseAddress + 4, bitOffset = 16)
      busCtrl.read(value, address = baseAddress + 4, bitOffset = 0)
      value clearWhen(busCtrl.isReading(baseAddress + 0))
    }

    val txData = new Area {
      val valid  = RegInit(False)
      val startDrop = Reg(Bool)
      val repeat = Reg(Bool)
      val enable = Reg(Bool)
      val dropOnConflict = Reg(Bool)
      val value  = Reg(Bits(8 bits))


      busCtrl.write(value, address = baseAddress + 0, bitOffset = 0)
      busCtrl.write(valid, address = baseAddress + 0, bitOffset = 8)
      busCtrl.write(enable, address = baseAddress + 0, bitOffset = 9)
      busCtrl.write(repeat, address = baseAddress + 0, bitOffset = 10)
      busCtrl.write(startDrop, address = baseAddress + 0, bitOffset = 11)
      busCtrl.write(dropOnConflict, address = baseAddress + 0, bitOffset = 12)

//      when(bus.cmd.kind === I2cIoSlaveCmdMode.START || bus.cmd.kind === I2cIoSlaveCmdMode.RESTART){
//        when(startDrop){
//          valid := False
//        }
//      }

    }

    val txAck = new Area {
      val valid  = Reg(Bool)
      val startDrop = Reg(Bool)
      val repeat = Reg(Bool)
      val enable = Reg(Bool)
      val value  = Reg(Bool)

      busCtrl.write(value, address = baseAddress + 4, bitOffset = 0)
      busCtrl.write(valid, address = baseAddress + 4, bitOffset = 8)
      busCtrl.write(enable, address = baseAddress + 4, bitOffset = 9)
      busCtrl.write(repeat, address = baseAddress + 4, bitOffset = 10)
      busCtrl.write(startDrop, address = baseAddress + 4, bitOffset = 11)


      //      when(bus.cmd.kind === I2cIoSlaveCmdMode.START || bus.cmd.kind === I2cIoSlaveCmdMode.RESTART){
//        when(startDrop){
//          valid := False
//        }
//      }
    }


    val dataCounter = RegInit(U"000")
    val inAckState = RegInit(False)
    val frameReset = False

    when(!inAckState) {
      bus.rsp.valid  := txData.valid && !rxData.valid
      bus.rsp.enable := txData.enable
      bus.rsp.data   := txData.value(7 - dataCounter)
    } otherwise {
      bus.rsp.valid  := txAck.valid && !rxAck.valid
      bus.rsp.enable := txAck.enable
      bus.rsp.data   := txAck.value
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
          when(txData.enable && txData.dropOnConflict){
            txData.enable clearWhen(bus.rsp.data =/= bus.cmd.data)
          }
          when(dataCounter === U"111") {
            rxData.valid setWhen(rxData.listen)
            inAckState := True
          }

        } otherwise {
          rxAck.valid setWhen(rxAck.listen)
          rxAck.value := bus.cmd.data
          inAckState := False
        }
      }
    }

    when(frameReset){
      inAckState := False
      dataCounter := 0
    }


    when(bus.cmd.kind === I2cSlaveCmdMode.START || bus.cmd.kind === I2cSlaveCmdMode.RESTART){
      when(txData.startDrop){
        txData.valid := False
      }
      when(txAck.startDrop){
        txAck.valid := False
      }
      when(rxAck.listenOnStart){ //TODO bus access conflict
        rxData.listen := True
      }
      when(rxAck.listenOnStart){
        rxData.listen := True
      }
    }

    val interrupt = new Area{
      val rxDataEnable = busCtrl.createReadAndWrite(Bool, address = baseAddress + 8, bitOffset = 0)
      val rxAckEnable = busCtrl.createReadAndWrite(Bool, address = baseAddress + 8, bitOffset = 1)
      val txDataEnable = busCtrl.createReadAndWrite(Bool, address = baseAddress + 8, bitOffset = 2)
      val txAckEnable = busCtrl.createReadAndWrite(Bool, address = baseAddress + 4, bitOffset = 3)

      def eventBuild(enableBitId : Int, flagBitId : Int, busCmd : I2cSlaveCmdMode.E) = new Area{
        val enable = busCtrl.createReadAndWrite(Bool, address = baseAddress + 8, bitOffset = enableBitId)
        val flag = busCtrl.createReadAndWrite(Bool, address = baseAddress + 8, bitOffset = flagBitId) setWhen(bus.cmd.kind === busCmd) clearWhen(!enable)
      }
      val start = eventBuild(4,8,I2cSlaveCmdMode.START)
      val restart = eventBuild(5,9,I2cSlaveCmdMode.RESTART)
      val end = eventBuild(6,10,I2cSlaveCmdMode.STOP)
      val drop = eventBuild(7,11,I2cSlaveCmdMode.DROP)


      val interrupt = (rxDataEnable && rxData.valid) || (rxAckEnable && rxAck.valid) ||
                      (txDataEnable && !txData.valid) || (txAckEnable && !txAck.valid) ||
                      (start.flag || restart.flag || end.flag || drop.flag)
    }

  }
}


/**
  * I2C Slave IO Layer :
  *
  *  This component manages the low level of the I2C protocol. (START, STOP, Send & Receive bit data)
  *
  *
  * DIAGRAM NOT UP TO DATE
  *          ________                       ________
  *         |        |<------- I2C ------->|        |
  *         | Master |                     |  Slave |
  *         |________|      RSP Stream --->|________|---> CMD Flow
  *
  * DIAGRAM NOT UP TO DATE
  * Write sequence :
  *
  *   RSP    :           DATA    DATA   DATA    DATA    DATA
  *   Master :   | START | DATA  |       | DATA  | DATA  | STOP |
  *   Slave  :   |       |       | DATA  |       |       |      |
  *   CMD    :        START    DATA    DATA    DATA    DATA    STOP
  *
  * DIAGRAM NOT UP TO DATE
  * Restart sequence :
  *
  *   RSP    :           DATA   DATA   DATA    DATA   DATA
  *   Master :   | START |      | DATA | START | DATA  | STOP |
  *   Slave  :   |       | DATA |      |       |       |      |
  *   CMD    :       START   DATA    DATA    START   DATA   STOP
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
    val rspBufferIn = Stream(I2cSlaveRsp())
    val rspBuffer = rspBufferIn.stage() //Store rsp transaction
    val rspAhead = rspBuffer.valid ? rspBuffer.asFlow | rspBufferIn.asFlow
    rspBufferIn.valid := io.bus.rsp.valid
    rspBufferIn.payload := io.bus.rsp
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
        tsuDat.reset := True
      }
    }

    when(inFrameData) {
      when(!rspBuffer.valid){
        tsuDat.reset := True
        io.bus.cmd.kind := CmdMode.DRIVE
      }

      when(!rspAhead.valid  || (rspAhead.enable && !tsuDat.done)) {
        sclWrite := False
      }
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
    io.bus.cmd.kind := timeout.tick ? CmdMode.DROP | CmdMode.STOP
    ctrl.inFrame     := False
    ctrl.inFrameData := False
  }

  /*
   * Drive SCL & SDA signals
   */
  io.i2c.scl.write := RegNext(ctrl.sclWrite)
  io.i2c.sda.write := RegNext(ctrl.sdaWrite)
}
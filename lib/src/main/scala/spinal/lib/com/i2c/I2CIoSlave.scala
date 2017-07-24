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


/**
  * Generics for the I2C Slave
  *
  * @param samplingWindowSize              : deepth sampling
  * @param samplingClockDividerWidth : Width of the clock divider
  */
case class I2cIoSlaveGenerics(samplingWindowSize        : Int = 3,
                              samplingClockDividerWidth : BitCount = 10 bits,
                              tsuDatWidth               : BitCount = 6 bits, //Data set-up time width
                              timeoutWidth              : BitCount = 20 bits){}


/**
  * Run-time configuration for the I2CSlave
  */
case class I2cIoSlaveConfig(g: I2cIoSlaveGenerics) extends Bundle {

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
object I2cIoSlaveCmdMode extends SpinalEnum{
  val START, DRIVE, READ, STOP = newElement()
}


/**
 * Define the command interface
 */
case class I2cIoSlaveCmd() extends Bundle{
  val mode = I2cIoSlaveCmdMode()
  val data  = Bool
}

/**
 * Define the response interface
 *  If you want to read data, set data = True
 *
 *  For the slave : (FREEZE) is done with the response stream.
 */
case class I2cIoSlaveRsp() extends Bundle{
  val enable = Bool
  val data = Bool
}

/**
  * I2C Slave IO Layer :
  *
  *  This component manages the low level of the I2C protocol. (START, STOP, Send & Receive bit data)
  *
  *          ________                       ________
  *         |        |<------- I2C ------->|        |
  *         | Master |                     |  Slave |
  *         |________|      RSP Stream --->|________|---> CMD Flow
  *
  * Write sequence :
  *
  *   RSP    :           DATA    DATA   DATA    DATA    DATA
  *   Master :   | START | DATA  |       | DATA  | DATA  | STOP |
  *   Slave  :   |       |       | DATA  |       |       |      |
  *   CMD    :        START    DATA    DATA    DATA    DATA    STOP
  *
  *
  * Restart sequence :
  *
  *   RSP    :           DATA   DATA   DATA    DATA   DATA
  *   Master :   | START |      | DATA | START | DATA  | STOP |
  *   Slave  :   |       | DATA |      |       |       |      |
  *   CMD    :       START   DATA    DATA    START   DATA   STOP
  */
class I2cIoSlave(g : I2cIoSlaveGenerics) extends Component{

  import spinal.lib.com.i2c.{I2cIoSlaveCmdMode => CmdMode}

  /**
    * Interface of the I2C Hal slave
    */
  val io = new Bundle{
    val i2c    = master( I2c() )
    val config = in( I2cIoSlaveConfig(g) )
    val cmd    = master Flow(I2cIoSlaveCmd())
    val rsp    = slave Flow(I2cIoSlaveRsp())
  }

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
    val rspBuffer = io.rsp.toStream.stage()
    rspBuffer.ready := False

    // default value
    io.cmd.valid := False
    io.cmd.data  := filter.sda
    io.cmd.mode.assignDontCare()

    // Send & Receive bit data
    when(inFrame) {
      when(sclEdge.rising) {
        io.cmd.valid := True
        io.cmd.mode := CmdMode.READ
      }

      when(sclEdge.falling) {
        io.cmd.valid := True
        io.cmd.mode  := CmdMode.DRIVE
        inFrameData  := True
        rspBuffer.ready := True //Flush
        tsuDat.reset := True
      }
    }

    when(inFrameData) {
      tsuDat.reset setWhen(!rspBuffer.valid)
      when(!rspBuffer.valid  || (rspBuffer.enable && !tsuDat.done)) {
        sclWrite := False
      }
      when(rspBuffer.valid && rspBuffer.enable){
        sdaWrite := io.rsp.data
      }
    }

    when(detector.start){
      io.cmd.valid  := True
      io.cmd.mode   := CmdMode.START
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
    io.cmd.valid  := True
    io.cmd.mode   := CmdMode.STOP
    ctrl.inFrame     := False
    ctrl.inFrameData := False
  }

  /*
   * Drive SCL & SDA signals
   */
  io.i2c.scl.write := RegNext(ctrl.sclWrite)
  io.i2c.sda.write := RegNext(ctrl.sdaWrite)
}
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
  * @param samplingSize              : deepth sampling
  * @param clockDividerSamplingWidth : Width of the clock divider
  */
case class I2CSlaveIoLayerGenerics(samplingSize             : Int = 3,
                                   clockDividerSamplingWidth: BitCount = 10 bits ){}


/**
  * Run-time configuration for the I2CSlave
  */
case class I2CSlaveIoLayerConfig(g: I2CSlaveIoLayerGenerics) extends Bundle {

  val clockDividerSampling = UInt(g.clockDividerSamplingWidth)

  def setFrequencySampling(frequencySampling : HertzNumber, clkFrequency : HertzNumber = ClockDomain.current.frequency.getValue): Unit = {
    clockDividerSampling := (clkFrequency / frequencySampling).toInt
  }
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
class I2CSlaveIoLayer(g : I2CSlaveIoLayerGenerics) extends Component{

  import spinal.lib.com.i2c.{I2CIoLayerCmdMode => CmdMode}

  /**
    * Interface of the I2C Hal slave
    */
  val io = new Bundle{
    val i2c    = slave( I2C() )
    val config = in( I2CSlaveIoLayerConfig(g) )
    val cmd    = master Flow  ( I2CIoLayerCmd() )
    val rsp    = slave  Stream( I2CIoLayerRsp() )
  }

  /**
    * Filter SDA and SCL input
    */
  val sampler = new I2CFilterInput(i2c_sda           = io.i2c.sda.read,
                                   i2c_scl           = io.i2c.scl.read,
                                   clockDivider      = io.config.clockDividerSampling,
                                   samplingSize      = g.samplingSize,
                                   clockDividerWidth = g.clockDividerSamplingWidth)

  /**
    * Detect the rising and falling edge of the scl signal
    */
  val sclEdge = new I2CSCLEdgeDetector(sampler.scl)


  /**
    * Detect the start/restart and the stop sequences
    */
  val detector = new Area{
    val sda_prev = RegNext(sampler.sda)  init(True)

    val sclHighLevel = sampler.scl && sclEdge.scl_prev

    val start = sclHighLevel && !sampler.sda && sda_prev
    val stop  = sclHighLevel && sampler.sda  && !sda_prev
  }


  /**
    * Slave controller
    */
  val ctrlSlave = new Area{

    val bitReceived   = Reg(Bool)
    val onTransaction = Reg(Bool) init(False)
    val transactionWillStart = Reg(Bool) init(False)
    val wr_sda = True
    val wr_scl = True

    // default value
    io.rsp.ready := False
    io.cmd.data  := bitReceived
    io.cmd.valid := False
    io.cmd.mode  := CmdMode.STOP


    // Detect a start condition
    when(detector.start){
      io.rsp.ready  := io.rsp.valid
      io.cmd.valid  := True
      io.cmd.mode   := CmdMode.START
      transactionWillStart := True
    }

    // After detecting the start wait the falling edge of
    //  the clock before starting to read/send bit data
    when(transactionWillStart && sclEdge.falling){
      onTransaction := True
    }

    // Detect the stop condition
    when(detector.stop){
      io.cmd.valid  := True
      io.cmd.mode   := CmdMode.STOP
      io.rsp.ready  := io.rsp.valid
      onTransaction := False
      transactionWillStart := False
    }


    // Send & Receive bit data
    when(onTransaction){
      // Freeze the bus if no response received
      wr_scl := io.rsp.valid

      // Write data
      wr_sda := io.rsp.data

      // Always write
      when(sclEdge.rising) {
        bitReceived  := sampler.sda
        io.cmd.valid := True
        io.cmd.mode  := CmdMode.DATA
        when(io.rsp.valid){
          io.rsp.ready := True
        }
      }
    }
  }

  /*
   * Drive SCL & SDA signals
   */
  io.i2c.scl.write := RegNext(ctrlSlave.wr_scl)
  io.i2c.sda.write := RegNext(ctrlSlave.wr_sda)
}
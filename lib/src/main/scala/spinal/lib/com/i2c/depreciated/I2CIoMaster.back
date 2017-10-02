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
import spinal.lib.fsm._


/**
  * Global configuration of the I2C Master
  *
  * @param samplingSize              : Number of sampler to generate a bit
  * @param samplingClockDividerWidth : Width of the clockDivider value
  * @param timerClockDividerWidth      : Width of the clockDivider value
  */
case class I2cIoMasterGenerics( samplingSize              : Int = 3,
                                samplingClockDividerWidth : BitCount = 10 bits,
                                timerClockDividerWidth    : BitCount = 20 bits,
                                timeoutBaudRatioLog2      : Int = 9){} // 9 => 1.28 ms timeout for 400 Khz i2c   (1/400Khz*2^9)


/**
  * Runtime configuration of the I2C master
  */
case class I2cIoMasterConfig(g: I2cIoMasterGenerics) extends Bundle {

  val samplingClockDivider = UInt(g.samplingClockDividerWidth)
  val timerClockDivider      = UInt (g.timerClockDividerWidth)

  def setTimerFrequency(sclFrequency : HertzNumber, clkFrequency : HertzNumber = ClockDomain.current.frequency.getValue) : Unit = {
    timerClockDivider := (clkFrequency / sclFrequency * 2).toInt
  }

  def setSamplingFrequency(frequencySampling : HertzNumber, clkFrequency : HertzNumber = ClockDomain.current.frequency.getValue): Unit = {
    samplingClockDivider := (clkFrequency / frequencySampling).toInt
  }
}


/**
 * Mode used to manage the slave
 */
object I2cIoMasterCmdMode extends SpinalEnum{
  val START, DATA, STOP, DROP = newElement()
}


/**
 * Define the command interface
 */
case class I2cIoMasterCmd() extends Bundle{
  val mode = I2cIoMasterCmdMode()
  val data  = Bool
}

/**
 * Define the response interface
 *  If you want to read data, set data = True
 *
 *  For the slave : (FREEZE) is done with the response stream.
 */
case class I2cIoMasterRsp() extends Bundle{
  val data = Bool
}

/**
  * I2C Master IO Layer
  *                ________                       _______
  *               |        |<------- I2C ------->|       |
  *               | Master |                     | Slave |
  *  CMD Stream ->|________|-> RSP Flow          |_______|
  *
  * Sequence :
  *
  *   CMD    : START   DATA     DATA   DATA   DATA    STOP
  *   Master :   | START | DATA  |      | DATA  |      | STOP |
  *   Slave  :   |       |       | DATA |       | DATA |      |
  *   RSP    :                 DATA    DATA    DATA   DATA
  */


case class I2cIoMaster(g: I2cIoMasterGenerics) extends Component {
  import spinal.lib.com.i2c.{I2cIoMasterCmdMode => CmdMode}

  val io = new Bundle{
    val i2c    = master(I2c())
    val config = in(I2cIoMasterConfig(g))
    val cmd    = slave  Stream(I2cIoMasterCmd())
    val rsp    = master Flow  (I2cIoMasterRsp())
  }


  /**
   * Filter SDA and SCL input
   */
  val filter = new I2cIoFilter(i2c  = io.i2c,
                               clockDivider      = io.config.samplingClockDivider,
                               samplingSize      = g.samplingSize,
                               clockDividerWidth = g.samplingClockDividerWidth)


  /**
   * Detect the rising and falling edge of the scl signal
   */
  val sclEdge = new I2cEdgeDetector(filter.scl)
  val sdaEdge = new I2cEdgeDetector(filter.sda)


  /**
   * Detect the start/restart and the stop sequence
   */
  val detector = new Area{
    val start = filter.scl && sdaEdge.falling
    val stop  = filter.scl && sdaEdge.rising
  }

  val timer = new Area{
    val counter = Reg(UInt(g.timerClockDividerWidth)) init(0)
    val tick = counter === 0
    val reset = tick  || !io.cmd.valid
    counter := counter - 1
    when(reset){
      counter := io.config.timerClockDivider
    }
  }


  val arbitration = new Area{
    val timeout = new Area{
      val counter = Reg(UInt(g.timerClockDividerWidth.value + g.timeoutBaudRatioLog2 bits)) init(0)
      val tick = counter === 0
      val reset = False setWhen(sclEdge.toogle || sdaEdge.toogle)
      counter := counter - 1
      when(reset){
        counter := io.config.timerClockDivider << g.timeoutBaudRatioLog2
      }
      when(detector.stop){
        counter := io.config.timerClockDivider.resized  //Apply tBuf
      }
    }

    val taken = RegInit(False)
    val losed = RegInit(False)
    when(timeout.tick){
      losed := False
      taken := False
    }
    when(detector.start && !taken){
      losed := True
    }
    when(!losed){
      timeout.reset := True
    }
  }

  /**
   * Main state machine of the Master HAL
   */
  val state = RegInit(U"00")
  val sclWrite, sdaWrite = True
  val keepSda = False setWhen(arbitration.taken && !io.cmd.valid)
  sclWrite.clearWhen(arbitration.taken)
  when(keepSda){
    sdaWrite := io.i2c.sda.write
  }
  timer.reset.setWhen(arbitration.taken && sclWrite && !filter.scl)

  io.cmd.ready := False
  io.rsp.valid := False
  io.rsp.data  := filter.sda
  when(io.cmd.valid) {
    when(arbitration.losed){
      timer.reset := True
    } otherwise {
      switch(io.cmd.mode) {
        is(CmdMode.START) {
          arbitration.losed := False
          when(!arbitration.taken) {
            //Normal start
            sdaWrite := False
            when(timer.tick || !filter.scl) {
              arbitration.taken := True
              io.cmd.ready := True
            }
          } otherwise {
            //Restart
            switch(state) {
              is(0) {
                when(timer.tick) {
                  state := 1
                }
              }
              is(1) {
                sclWrite := True
                when(timer.tick) {
                  state := 2
                }
              }
              is(2) {
                sdaWrite := False
                sclWrite := True
                when(timer.tick) {
                  io.cmd.ready := True
                }
              }
            }
          }
        }
        is(CmdMode.DATA) {
          switch(state) {
            is(0) {
              when(!filter.scl) {
                sdaWrite := io.cmd.data
              } otherwise {
                keepSda := True
              }
              when(timer.tick) {
                state := 1
              }
            }
            is(1) {
              sclWrite := True
              sdaWrite := io.cmd.data
              when(filter.scl) {
                state := 2
                io.rsp.valid := True
              }
            }
            is(2) {
              sclWrite := True
              sdaWrite := io.cmd.data
              when(timer.tick) {
                io.cmd.ready := True
              }
            }
          }
        }
        is(CmdMode.STOP) {
          switch(state) {
            is(0) {
              sdaWrite clearWhen (filter.scl === False)
              when(timer.tick) {
                state := 1
              }
            }
            is(1) {
              sclWrite := True
              sdaWrite := False
              when(timer.tick) {
                io.cmd.ready := True
                arbitration.taken := False
                arbitration.losed := True
              }
            }
          }
        }
        is(CmdMode.DROP) {
          arbitration.taken := False
          arbitration.losed := True
          sclWrite := True
          io.cmd.ready := True
        }
      }
    }
  }

  when(io.cmd.ready){
    state := 0
  }

  io.i2c.scl.write := RegNext(sclWrite) init(True)
  io.i2c.sda.write := RegNext(sdaWrite) init(True)
}
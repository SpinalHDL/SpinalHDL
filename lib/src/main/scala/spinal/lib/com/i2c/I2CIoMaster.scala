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
case class I2CIoMasterGenerics( samplingSize              : Int = 3,
                                samplingClockDividerWidth : BitCount = 10 bits,
                                timerClockDividerWidth    : BitCount = 20 bits,
                                timeoutBaudRatioLog2      : Int = 9){} // 9 => 1.28 ms timeout for 400 Khz i2c   (1/400Khz*2^9)


/**
  * Runtime configuration of the I2C master
  */
case class I2CIoMasterConfig(g: I2CIoMasterGenerics) extends Bundle {

  val samplingClockDivider = UInt(g.samplingClockDividerWidth)
  val timerClockDivider      = UInt (g.timerClockDividerWidth)

  def setTimerFrequency(sclFrequency : HertzNumber, clkFrequency : HertzNumber = ClockDomain.current.frequency.getValue) : Unit = {
    timerClockDivider := (clkFrequency / sclFrequency * 2).toInt
  }

  def setFrequencySampling(frequencySampling : HertzNumber, clkFrequency : HertzNumber = ClockDomain.current.frequency.getValue): Unit = {
    samplingClockDivider := (clkFrequency / frequencySampling).toInt
  }
}


/**
 * Mode used to manage the slave
 */
object I2CIoMasterCmdMode extends SpinalEnum{
  val START, DATA, STOP, DROP = newElement()
}


/**
 * Define the command interface
 */
case class I2CIoMasterCmd() extends Bundle{
  val mode = I2CIoMasterCmdMode()
  val data  = Bool
}

/**
 * Define the response interface
 *  If you want to read data, set data = True
 *
 *  For the slave : (FREEZE) is done with the response stream.
 */
case class I2CIoMasterRsp() extends Bundle{
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


class I2CIoMaster(g: I2CIoMasterGenerics) extends Component {
  import spinal.lib.com.i2c.{I2CIoMasterCmdMode => CmdMode}

  val io = new Bundle{
    val i2c    = master( I2C() )
    val config = in( I2CIoMasterConfig(g) )
    val cmd    = slave  Stream( I2CIoMasterCmd()  )
    val rsp    = master Flow  ( I2CIoMasterRsp() )
  }


  /**
   * Filter SDA and SCL input
   */
  val filter = new I2CIoFilter(i2c               = io.i2c,
    clockDivider      = io.config.samplingClockDivider,
    samplingSize      = g.samplingSize,
    clockDividerWidth = g.samplingClockDividerWidth)


  /**
   * Detect the rising and falling edge of the scl signal
   */
  val sclEdge = new I2CEdgeDetector(filter.scl)
  val sdaEdge = new I2CEdgeDetector(filter.sda)


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

  val timeout = new Area{
    val counter = Reg(UInt(g.timerClockDividerWidth.value << g.timeoutBaudRatioLog2 bits)) init(0)
    val tick = counter === 0
    val reset = False setWhen(sclEdge.toogle)
    counter := counter - 1
    when(reset){
      counter := io.config.timerClockDivider << g.timeoutBaudRatioLog2
    }
    when(detector.stop){
      counter := io.config.timerClockDivider  //Apply tBuf
    }
  }

  val arbitration = new Area{
    val taken = RegInit(False)
    val losed = RegInit(False) setWhen(detector.start && !taken)
    when(timeout.tick){
      losed := False
      taken := False
    }
    when(!losed && !taken){
      timeout.reset := True
    }
  }

  /**
   * Main state machine of the Master HAL
   */
  val state = RegInit(U"00")
  val startSuccessor = Reg(Bool) clearWhen(io.cmd.valid) //Keep the SDA low after a START until the next CMD
  val sclWrite, sdaWrite = True
  sclWrite.clearWhen(arbitration.taken)
  sdaWrite.clearWhen(startSuccessor)
  when(io.cmd.valid && !arbitration.losed) {
    switch(io.cmd.mode) {
      is(CmdMode.START){
        arbitration.losed := False
        when(!arbitration.taken) { //Normal start
          sdaWrite := False
          when(timer.tick) {
            arbitration.taken := True
            startSuccessor := True
            io.cmd.ready := True
          }
        } otherwise{             //Restart
          switch(state){
            is(0){
              when(timer.tick){
                state := 1
              }
            }
            is(1){
              sclWrite := True
              when(timer.tick){
                state := 2
              }
            }
            is(2){
              sdaWrite := False
              sclWrite := True
              when(timer.tick){
                io.cmd.ready := True
                startSuccessor := True
              }
            }
          }
        }
      }
      is(CmdMode.DATA){
        when(!filter.scl) {
          sdaWrite := io.cmd.data
        }
        switch(state){
          is(0){
            when(timer.tick){
              state := 1
            }
          }
          is(1){
            sclWrite := True
            when(timer.tick){
              io.cmd.ready := True
            }
          }
        }
      }
      is(CmdMode.STOP){
        switch(state){
          is(0){
            sdaWrite clearWhen(filter.scl === False)
            when(timer.tick){
              state := 1
            }
          }
          is(1){
            sclWrite := True
            sdaWrite := False
            when(timer.tick){
              io.cmd.ready := True
              arbitration.taken := False
              arbitration.losed := True
            }
          }
        }
      }
      is(CmdMode.DROP){
        arbitration.losed := True
        arbitration.taken := False
        io.cmd.ready := True
      }
    }
  }

  when(io.cmd.ready){
    state := 0
  }



  io.i2c.scl.write := RegNext(sclWrite) init(True)
  io.i2c.sda.write := RegNext(sdaWrite) init(True)
}
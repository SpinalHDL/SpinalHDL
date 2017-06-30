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
  * @param clockDividerSamplingWidth : Width of the clockDivider value
  * @param clockDividerSCLWidth      : Width of the clockDivider value
  */
case class I2CIoMasterGenerics(samplingSize              : Int = 3,
                               clockDividerSamplingWidth : BitCount = 10 bits,
                               clockDividerSCLWidth      : BitCount = 20 bits){}


/**
  * Runtime configuration of the I2C master
  */
case class I2CIoMasterConfig(g: I2CIoMasterGenerics) extends Bundle {

  val clockDividerSampling = UInt(g.clockDividerSamplingWidth)
  val clockDividerSCL      = UInt (g.clockDividerSCLWidth)

  def setSCLFrequency(sclFrequency : HertzNumber, clkFrequency : HertzNumber = ClockDomain.current.frequency.getValue) : Unit = {
    clockDividerSCL := (clkFrequency / sclFrequency * 2).toInt
  }

  def setFrequencySampling(frequencySampling : HertzNumber, clkFrequency : HertzNumber = ClockDomain.current.frequency.getValue): Unit = {
    clockDividerSampling := (clkFrequency / frequencySampling).toInt
  }
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

  import spinal.lib.com.i2c.{I2CIoCmdMode => CmdMode}


  val io = new Bundle{
    val i2c    = master( I2C() )
    val config = in( I2CIoMasterConfig(g) )
    val cmd    = slave  Stream( I2CIoCmd()  )
    val rsp    = master Flow  ( I2CIoRsp() )
  }


  /**
    * Filter SDA and SCL input
    */
  val sampler = new I2CIoFilter(i2c_sda           = io.i2c.sda.read,
                                   i2c_scl           = io.i2c.scl.read,
                                   clockDivider      = io.config.clockDividerSampling,
                                   samplingSize      = g.samplingSize,
                                   clockDividerWidth = g.clockDividerSamplingWidth)


  /**
    * Detect the rising and falling edge of the scl signal
    */
  val sclEdge = new I2CEdgeDetector(sampler.scl)


  /**
    * Generate and manage the scl clock
    */
  val sclGenerator = new Area{

    val cntValue        = Reg(UInt(g.clockDividerSCLWidth)) init(0)
    val scl             = RegInit(True)
    val scl_en          = Bool // set in the state machine "smMaster"
    val masterFreeze    = Bool // set in the state machine "smMaster"
    val stopDetected    = Bool // set in the area "detector"

    // detect if the slave freeze the bus
    val slaveFreeze = scl && !sampler.scl

    // start / stop the counter clock
    when(scl_en && !masterFreeze && !slaveFreeze){

      cntValue := cntValue + 1
      when(cntValue >= io.config.clockDividerSCL ){ cntValue := 0 }

    }.elsewhen(stopDetected){
      cntValue := 0
    }

    // SCL generation
    when(cntValue === io.config.clockDividerSCL){ scl := !scl }

    // Used to indicate when to generate the start/restart/stop sequence
    val triggerStartStop = scl && cntValue >= (io.config.clockDividerSCL >> 1)
  }


  /**
    * Detect the start/restart and the stop sequence
    */
  val detector = new Area{

    val sda_prev = RegNext(sampler.sda)

    val sclLevelHigh = sclGenerator.triggerStartStop && sampler.scl

    val start = sclLevelHigh && !sampler.sda  && sda_prev
    val stop  = sclLevelHigh && sampler.sda   && !sda_prev

    sclGenerator.stopDetected := stop
  }


  /**
    * when start is detected, block all others master
    */
  val busState = new Area{
    val busy = Reg(Bool) init(False) setWhen(detector.start) clearWhen(detector.stop)
  }


  /**
    * Main state machine of the Master HAL
    */
  val smMaster = new StateMachine{

    val dataReceived = Reg(Bool) randBoot()
    val getBus       = Reg(Bool) init(False)

    val wr_sda = True

    // default value
    sclGenerator.masterFreeze := False
    sclGenerator.scl_en       := False
    io.cmd.ready := False
    io.rsp.valid := False
    io.rsp.data  := dataReceived

    always{

      when(io.cmd.valid && io.cmd.mode === CmdMode.START && (!busState.busy | getBus) ){
        io.cmd.ready := True
        getBus       := True
        goto(sStart)
      }

    }

    val sIdle: State = new State with EntryPoint {
      whenIsActive {
        getBus := False
      }
    }

    val sStart: State = new State {
      whenIsActive{
        sclGenerator.scl_en := True
        wr_sda := !sclGenerator.triggerStartStop

        // end of the stop sequence
        when(sclEdge.falling){ goto(sData) }
      }
    }

    val sData: State = new State{

      whenIsActive{
        sclGenerator.scl_en := True
        sclGenerator.masterFreeze := !io.cmd.valid

        // write data and check collision
        when(io.cmd.mode === CmdMode.DATA){
          wr_sda := io.cmd.data
        }

        // Read data
        when(sclEdge.rising){
          dataReceived := sampler.sda
          io.rsp.valid := True
          io.rsp.data  := sampler.sda
        }

        when(sclEdge.falling){
          io.cmd.ready := (io.cmd.mode === CmdMode.DATA)
        }

        // check if a stop command occurs
        when(io.cmd.valid && io.cmd.mode === CmdMode.STOP){
          goto(sStop)
        }

      }
    }

    val sStop: State = new State {
      whenIsActive{
        sclGenerator.scl_en := True
        wr_sda := False
        when(sclGenerator.triggerStartStop){
          io.cmd.ready := True
          goto(sIdle)
        }
      }
    }
  }

  io.i2c.sda.write := RegNext(smMaster.wr_sda)  randBoot()
  io.i2c.scl.write := RegNext(sclGenerator.scl) randBoot()
}
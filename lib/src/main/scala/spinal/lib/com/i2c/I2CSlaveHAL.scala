/******************************************************************************
  * I2C Slave HAL
  *          ________                       ________
  *         |        |<------- I2C ------->|        |
  *         | Master |                     |  Slave |
  *         |________|      RSP Stream --->|________|---> CMD Flow
  *
  * Write sequence :
  *
  *   RSP    :           NONE     ACK  NONE    ACK   NONE   NONE
  *   Master :   | START | DATA  |     | DATA  |     | STOP |
  *   Slave  :   |       |       | ACK |       | ACK |      |
  *   CMD    :       START    DATA   ACK    DATA   ACK   STOP
  *
  * Read sequence :
  *
  *   RSP    :           DATA   NONE    DATA    NONE     NONE   NONE
  *   Master :   | START |      |  ACK  |       |  NACK  | STOP |
  *   Slave  :   |       | DATA |       |  DATA |        |      |
  *   CMD    :       START   DATA     ACK    DATA     NACK    STOP
  *
  * Restart sequence :
  *
  *   RSP    :           DATA   NONE   NONE    NONE    ACK          NONE
  *   Master :   | START |      | NACK | START | DATA  |     | STOP |
  *   Slave  :   |       | DATA |      |       |       | ACK |      |
  *   CMD    :       START   DATA   NACK    START   DATA         STOP
  */
package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// @TODO check after reset no dectector start or stop fire

/**
  * Generics for the I2C Slave
  *
  * @param dataWidth         : Width of the data send/read
  * @param samplingSize      : deepth smapling
  * @param clockDividerSamplingWidth : Width of the clock divider
  */
case class I2CSlaveHALGenerics(dataWidth                 : BitCount = 8 bits,
                               samplingSize              : Int = 3,
                               clockDividerSamplingWidth : BitCount = 10 bits ){}


/**
  * Run-time configuration for the I2CSlave
  */
case class I2CSlaveHALConfig(g : I2CSlaveHALGenerics) extends Bundle {

  val clockDividerSampling = UInt(g.clockDividerSamplingWidth )

  def setFrequencySampling(frequencySampling : HertzNumber, clkFrequency : HertzNumber = ClockDomain.current.frequency.getValue): Unit = {
    clockDividerSampling := (clkFrequency / frequencySampling).toInt
  }
}


/**
  * Mode used to manage the slave
  */
object I2CSlaveHALCmdMode extends SpinalEnum{
  val START, NACK, ACK, STOP, DATA = newElement()
}


/**
  * Define the command interface
  */
case class I2CSlaveHALCmd(g : I2CSlaveHALGenerics) extends Bundle{
  val mode  = I2CSlaveHALCmdMode()
  val data  = Bits(g.dataWidth )
}


/**
  *    DATA  : Send a data to the master
  *    NONE  : None operation / NACK / Read data from the master
  *    ACK   : ACK
  *   (FREEZE) is done with the response stream.
  */
object I2CSlaveHALRspMode extends SpinalEnum{
  val DATA, NONE, ACK = newElement()
}


/**
  * Define the response interface
  */
case class I2CSlaveHALRsp(g : I2CSlaveHALGenerics) extends Bundle{
  val mode  = I2CSlaveHALRspMode()
  val data  = Bits(g.dataWidth )
}


/**
  * Interface I2C Slave HAL
  */
case class I2CSlaveHALio(g : I2CSlaveHALGenerics) extends Bundle{
  val i2c    = slave( I2C() )
  val config = in( I2CSlaveHALConfig(g) )
  val cmd    = master Flow  ( I2CSlaveHALCmd(g) )
  val rsp    = slave  Stream( I2CSlaveHALRsp(g) )
}


/**
  * Definition of the component I2C Slave HAL
  */
class I2CSlaveHAL(g : I2CSlaveHALGenerics) extends Component{

  import spinal.lib.com.i2c.{I2CSlaveHALRspMode => RspMode}
  import spinal.lib.com.i2c.{I2CSlaveHALCmdMode => CmdMode}

  val io = I2CSlaveHALio(g)

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
  val sclEdge = new SCLEdgeDetector(sampler.scl)


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
    * Counter of bit write/read
    */
  val bitCounter = new I2CBitCounter(sclEdge.falling, g.dataWidth)


  /**
    * Main state machine
    */
  val smSlave = new StateMachine{

    val dataReceived = Reg(Bits(g.dataWidth)) randBoot()
    val wr_sda       = True
    val wr_scl       = True

    // default value
    io.rsp.ready := False
    io.cmd.data  := dataReceived
    io.cmd.valid := False
    io.cmd.mode  := CmdMode.ACK


    always{

      // Stop detected
      when(detector.stop)  {
        io.cmd.valid := True
        io.cmd.mode  := CmdMode.STOP
        when(io.rsp.valid){ io.rsp.ready := True }
        goto(sIdle)
      }

      // Start detected
      when(detector.start) {
        io.rsp.ready := io.rsp.valid
        goto(sStart)
      }
    }

    val sIdle : State = new State with EntryPoint {
      whenIsActive{
        // nothing to do
      }
    }

    val sStart : State = new State {
      onEntry{
        io.cmd.valid := True
        io.cmd.mode  := CmdMode.START
      }
      whenIsActive{
        when(sclEdge.falling){ goto(sData) }
      }
    }

    val sData : State = new State {
      onEntry{
        bitCounter.clear()
      }
      whenIsActive{
        // Freeze the bus if no response received
        wr_scl := io.rsp.valid

        // Read data
        when(io.rsp.mode === RspMode.DATA){
          // index change at each falling edge
          wr_sda := io.rsp.data(bitCounter.index )
        }

        // Always write
        when(sclEdge.rising) {
          dataReceived(bitCounter.index) := sampler.sda
        }

        // End of data sequence ?
        when(bitCounter.isDone){
          io.rsp.ready := True
          io.cmd.valid := True
          io.cmd.mode  := CmdMode.DATA
          goto(sACK)
        }
      }
    }

    val sACK : State = new State{
      whenIsActive{
        // Freeze the bus if no response received
        wr_scl := io.rsp.valid

        // Read ack
        wr_sda := !(io.rsp.mode === RspMode.ACK)

        // Write ack
        when(sclEdge.rising){
          io.cmd.valid := True
          io.cmd.mode  := sampler.sda ? CmdMode.NACK | CmdMode.ACK
        }

        // end of the ACK sequence
        when(sclEdge.falling){
          io.rsp.ready := True
          goto(sData)
        }
      }
    }
  }

  io.i2c.scl.write := RegNext(smSlave.wr_scl) randBoot()
  io.i2c.sda.write := RegNext(smSlave.wr_sda) randBoot()
}

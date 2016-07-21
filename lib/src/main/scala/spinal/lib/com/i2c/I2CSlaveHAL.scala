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

// @TODO check after reset no dectecotr.start or stop fire

/**
  * Generics for the I2C Slave
  *
  * @param dataWidth         : Width of the data send/read
  * @param samplingSize      : deepth smapling
  * @param clockDividerSamplingWidth : Width of the clock divider
  */
case class I2CSlaveHALGenerics(dataWidth                 : Int  = 8,
                               samplingSize              : Int = 3,
                               clockDividerSamplingWidth : Int = 10 ){}


/**
  * Run-time configuration for the I2CSlave
  */
case class I2CSlaveHALConfig(g : I2CSlaveHALGenerics) extends Bundle {

  val clockDividerSampling = UInt(g.clockDividerSamplingWidth bit)

  def setClockDividerSampling(divider : Int): Unit = {
    clockDividerSampling := divider
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
  val data  = Bits(g.dataWidth bits)
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
  val data  = Bits(g.dataWidth bits)
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
    * Clock divider used to sample sda and scl
    */
  val samplingClockDivider = new Area{

    val counter = Reg(UInt(g.clockDividerSamplingWidth bits)) init(0)
    val tick    = counter === 0

    counter := counter - 1
    when(tick){ counter := io.config.clockDividerSampling }
  }


  /**
    * Sample sda and scl
    */
  val sampler = new Area{

    val scl     = BufferCC(io.i2c.scl.read)
    val sda     = BufferCC(io.i2c.sda.read)

    val sdaSamples = History(that=sda, length=g.samplingSize, when=samplingClockDivider.tick, init=True)
    val sclSamples = History(that=scl, length=g.samplingSize, when=samplingClockDivider.tick, init=True)

    val rd_sda        = RegNext(MajorityVote(sdaSamples))
    val rd_scl        = RegNext(MajorityVote(sclSamples))
  }


  /**
    * Falling and rising edge detection of the scl signal
    */
  val sclSampling = new Area{

    val scl_prev = RegNext(sampler.rd_scl) init(True)

    val risingEdge  = sampler.rd_scl  && !scl_prev
    val fallingEdge = !sampler.rd_scl && scl_prev
  }


  /**
    * Detect the start/restart and the stop sequences
    */
  val detector = new Area{

    val sda_cur  = sampler.rd_sda
    val sda_prev = RegNext(sda_cur)  init(True)

    val sclHighLevel = sampler.rd_scl && sclSampling.scl_prev

    val start = sclHighLevel && !sda_cur && sda_prev
    val stop  = sclHighLevel && sda_cur  && !sda_prev
  }


  /**
    * For counting the number of bit send/received (MSB is send first )
    */
  val bitCounter = new Area {

    val index  = Reg(UInt(log2Up(g.dataWidth) bits)) randBoot()
    val isDone = False

    def clear() : Unit = index := g.dataWidth-1

    when(sclSampling.fallingEdge) {
      index  := index - 1
      isDone := index === 0
    }
  }


  /**
    * Main state machine
    */
  val smSlave = new StateMachine{

    val dataReceived = Reg(Bits(g.dataWidth bits)) randBoot()
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
        when(sclSampling.fallingEdge){ goto(sData) }
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
        when(sclSampling.risingEdge) {
          dataReceived(bitCounter.index) := sampler.rd_sda
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
        when(sclSampling.risingEdge){
          io.cmd.valid := True
          io.cmd.mode  := sampler.rd_sda ? CmdMode.NACK | CmdMode.ACK
        }

        // end of the ACK sequence
        when(sclSampling.fallingEdge){
          io.rsp.ready := True
          goto(sData)
        }
      }
    }
  }

  io.i2c.scl.write := RegNext(smSlave.wr_scl) randBoot()
  io.i2c.sda.write := RegNext(smSlave.wr_sda) randBoot()
}

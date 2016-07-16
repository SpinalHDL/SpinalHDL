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


// @TODO oversampling the input's signal...

/**
  * Global configuration of the I2C Slave
  *
  * @param dataWidth    : Width of the data send/read
  */
case class I2CSlaveHALGenerics(dataWidth            : Int    = 8,
                               clockDividerSampling : BigInt = 100 ){}


/**
  *    START : START sequence detected
  *    NACK  : NACK received
  *    ACK   : ACK received
  *    STOP  : STOP sequence detected
  *    DATA  : DATA received
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
  *    NONE  : NO operation / NACK
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
  val i2c  = slave( I2C() )
  val cmd  = master Flow  ( I2CSlaveHALCmd(g) )
  val rsp  = slave  Stream( I2CSlaveHALRsp(g) )
}


/**
  * Definition of the component I2C Slave HAL
  */
class I2CSlaveHAL(g : I2CSlaveHALGenerics) extends Component{

  import spinal.lib.com.i2c.{I2CSlaveHALRspMode => RspMode}
  import spinal.lib.com.i2c.{I2CSlaveHALCmdMode => CmdMode}


  val io = I2CSlaveHALio(g)


  /**
    * Synchronize input's signals of the I2C
    */
  val ccIO = new Area{
    val rd_scl = BufferCC(io.i2c.scl.read)
    val rd_sda = BufferCC(io.i2c.sda.read)
  }


  // @TODO in progress... sampling input..
  val samplingClockDivider = new Area{
    val counter = Reg(UInt(log2Up(g.clockDividerSampling) bits)) init(0)
    val tick    = counter === 0

    counter := counter - 1
    when(tick){
      counter := g.clockDividerSampling
    }
  }

  val filterInput = new Area{
    val sdaSample = RegNextWhen(ccIO.rd_sda, samplingClockDivider.tick)
    val sclSample = RegNextWhen(ccIO.rd_scl, samplingClockDivider.tick)
  }


  /**
    * Rising and falling edge of the scl signal detection
    */
  val sclSampling = new Area{

    val risingEdge  = False
    val fallingEdge = False

    val scl_cur  = RegNext(ccIO.rd_scl) init(False)
    val scl_prev = RegNext(scl_cur)     init(False)

    when(scl_cur && !scl_prev){
       risingEdge := True
    }

    when(!scl_cur && scl_prev){
      fallingEdge := True
    }

  }


  /**
    * Detect the start/restart and the stop sequence
    */
  val detector = new Area{

    val start   = False
    val stop    = False

    val sda_cur  = RegNext(ccIO.rd_sda) init(False)
    val sda_prev = RegNext(sda_cur)     init(False)

    // start = falling edge of sda while the scl is 1
    when(sclSampling.scl_cur && sclSampling.scl_prev && !sda_cur && sda_prev){
      start := True
    }

    // stop = rising edge of sda while the scl is 1
    when( sclSampling.scl_cur && sclSampling.scl_prev && sda_cur && !sda_prev){
      stop := True
    }
    // @TODO check why there is a difference between when or without the when... CCBuffer propagation
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

    val dataReceived = Reg(Bits(g.dataWidth bits)) init(0) // randBoot()
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

        goto(sWaitEndStart)
      }
    }

    val sIdle : State = new State with EntryPoint {
      whenIsActive{
        // nothing to do
      }
    }

    val sWaitEndStart : State = new State {
      onEntry{
        io.cmd.valid := True
        io.cmd.mode  := CmdMode.START
      }
      whenIsActive{
        when(sclSampling.fallingEdge){
          goto(sData)
        }
      }
    }

    val sData : State = new State {
      onEntry{
        bitCounter.clear()
      }
      whenIsActive{
        // Freeze the bus if no response received
        wr_scl := io.rsp.valid

        // Read data on bus
        when(io.rsp.mode === RspMode.DATA){
          // index change at each falling edge
          wr_sda := io.rsp.data(bitCounter.index )
        }

        // Always write on bus
        when(sclSampling.risingEdge) {
          dataReceived(bitCounter.index) := ccIO.rd_sda
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

        // Read ack on bus
        wr_sda := !(io.rsp.mode === RspMode.ACK)

        // Write ack on bus
        when(sclSampling.risingEdge){
          io.cmd.valid := True
          io.cmd.mode  := ccIO.rd_sda ? CmdMode.NACK | CmdMode.ACK
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

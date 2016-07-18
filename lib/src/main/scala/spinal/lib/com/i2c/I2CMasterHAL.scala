/******************************************************************************
  * I2C Master HAL
  *
  *                ________                       _______
  *               |        |<------- I2C ------->|       |
  *               | Master |                     | Slave |
  *  CMD Stream ->|________|-> RSP Flow          |_______|
  *
  * Write sequence :
  *
  *   CMD    : START   WRITE           WRITE         STOP
  *   Master :   | START | WRITE |     | WRITE |     | STOP |
  *   Slave  :   |       |       | ACK |       | ACK |      |
  *   RSP    :                  DATA  ACK     DATA  ACK
  *
  * Read sequence :
  *
  *   CMD    : START   READ     ACK   READ   NACK   STOP
  *   Master :   | START |      | ACK |      | NACK | STOP |
  *   Slave  :   |       | READ |     | READ |      |      |
  *   RSP    :                DATA        DATA
  *
  * Restart sequence :
  *
  *   CMD    : START   READ   NACK   RESTART  WRITE         STOP
  *   Master :   | START |      | NACK | START | WRITE |     | STOP |
  *   Slave  :   |       | READ |      |       |       | ACK |      |
  *   RSP    :                 DATA                   DATA  ACK
  */

// @TOTO Synch/Filter inputs signals..

package spinal.lib.com.i2c


import spinal.core._
import spinal.lib._
import spinal.lib.fsm._


/**
  * Global configuration of the I2C Master
  *
  * @param dataWidth         : Width of the data send
  * @param clockDividerWidth : Width of the clockDivider value
  * @param multiMaster_en    : Multi-Master or Single-Master
  */
case class I2CMasterHALGenerics(dataWidth         : Int =  8,
                                clockDividerWidth : Int = 20,
                                multiMaster_en    : Boolean = true){}


/**
  * Used to config the I2C master HAL at runtime
  */
case class I2CMasterHALConfig(g: I2CMasterHALGenerics) extends Bundle {

  val clockDivider = UInt (g.clockDividerWidth bits)
  val enCollision  = Bool

  def setFrequency(sclFrequency : Double, clkFrequency : Double = ClockDomain.current.frequency.getValue) : Unit = {
    clockDivider := (clkFrequency / sclFrequency * 2).toInt
  }
}


/**
  * 6 different modes can be used to manage the master
  *    START      : Send the start/restart sequence
  *    WRITE      : Write a data
  *    READ       : Read a data
  *    ACK        : Send an ACK after reading
  *    NACK       : Send a NACK after reading
  *    STOP       : Send the stop sequence
  */
object I2CMasterHALCmdMode extends SpinalEnum{
  val START, WRITE, READ, ACK, NACK, STOP = newElement()
}


/**
  * Define the command interface
  */
case class I2CMasteHALCmd(g : I2CMasterHALGenerics) extends Bundle{
  val mode   = I2CMasterHALCmdMode()
  val data   = Bits(g.dataWidth bits)
}


/**
  * 4 different modes are available for a response
  *    ACK       : ACK received after writting
  *    NACK      : NACK recieved after writting
  *    DATA      : Data read on the bus
  *    COLLISION : Collision detected during a write
  */
object I2CMasterHALRspMode extends SpinalEnum{
  val ACK, NACK, DATA, COLLISION = newElement()
}

/**
  * Define the response interface
  */
case class I2CMasterHALRsp(g : I2CMasterHALGenerics) extends Bundle{
  val mode  = I2CMasterHALRspMode()
  val data  = Bits(g.dataWidth bits)
}


/**
  * Define the interface of the I2C Master HAL
  */
case class I2CMasterHALio(g : I2CMasterHALGenerics) extends Bundle{
  val i2c    = master( I2C() )
  val config = in( I2CMasterHALConfig(g) )
  val cmd    = slave  Stream( I2CMasteHALCmd(g)  )
  val rsp    = master Flow  ( I2CMasterHALRsp(g) )
}

/**
  * Definition of the component I2C Master HAL
  */
class I2CMasterHAL(g : I2CMasterHALGenerics) extends Component {

  import spinal.lib.com.i2c.{I2CMasterHALRspMode => RspMode}
  import spinal.lib.com.i2c.{I2CMasterHALCmdMode => CmdMode}


  val io = I2CMasterHALio(g)

  // Enable/Disable the scl signal
  val scl_en     = Bool

  // freeze the SCL signal
  val sclFreeze = Bool

  val quarterDivider = io.config.clockDivider >> 1


  /**
    * Synchronize input's signals of the I2C
    */
  val ccIO = new Area{
    val rd_scl = BufferCC(io.i2c.scl.read)
    val rd_sda = BufferCC(io.i2c.sda.read)
  }


  /**
    * Rising and falling edge of the scl signal detection
    */
  val sclSampling = new Area{

    val risingEdge  = Bool
    val fallingEdge = Bool

    val scl_cur  = RegNext(ccIO.rd_scl)
    val scl_prev = RegNext(scl_cur)

    risingEdge  := scl_cur && !scl_prev
    fallingEdge := !scl_cur && scl_prev
  }


  /**
    * Generate and manage the scl clock,  signals to indicate the
    * rising and falling edge of SCL as well as a signal to indicate
    * when to execute a start/stop/restart operation
    */
  val sclGenerator = new Area{

    val cntValue        = Reg(UInt(g.clockDividerWidth bits)) init(0)
    val triggerSequence = Bool
    val scl             = RegInit(True)

    // start / stop the counter clock
    when(scl_en && !sclFreeze){

      cntValue := cntValue + 1
      when(cntValue >= io.config.clockDivider ){ cntValue := 0 }

    }otherwise{
     // when(sclFreezeByMater){ scl := False}otherwise{scl := True}

      cntValue := 0
    }

    // SCL generation
    when(cntValue === io.config.clockDivider){ scl := !scl }

    // Used to indicate when to generate the start/restart/stop sequence
    triggerSequence := scl && cntValue >= quarterDivider
  }




  /**
    * Detect the start/restart and the stop sequence
    */
  val detector = new Area{

    val start   = Bool
    val stop    = Bool

    val sda_cur  = RegNext(ccIO.rd_sda)
    val sda_prev = RegNext(sda_cur)

    // start = falling edge of sda while the scl is 1
    start := sclGenerator.scl && !sda_cur  && sda_prev

    // stop = rising edge of sda while the scl is 1
    stop  := sclGenerator.scl && sda_cur && !sda_prev

  }



  val state_Bus = new StateMachine{

    val busy = False

    val sIDLE : State = new State with EntryPoint{
      whenIsActive {
        when(detector.start){
          goto(sDELAY)
        }
      }

      val sDELAY : State = new StateDelay(cyclesCount= quarterDivider){
        whenCompleted{
          goto(sBUSY)
        }
      }

      val sBUSY : State = new State{
        whenIsActive{
          busy := True
          when(detector.stop){
            goto(sIDLE)
          }
        }
      }
    }
  }




  /**
    * Synchronize all SCL signals of the different master
    * in the case when several master drive the SCL.
    */
  val smSynchSCL = new Area{

    val freezeSCL = Bool

    freezeSCL := Delay(sclGenerator.scl,2) && !ccIO.rd_scl
  }


  /**
    * Counter of bit write/read. MSB is send first on the I2C bus
    * so counter goes from dataWdith to 0
    */
  val bitCounter = new Area{
    val index  = Reg(UInt( log2Up(g.dataWidth) bits )) randBoot()
    val isDone = False
    def clear() : Unit = index := g.dataWidth - 1

    when(sclSampling.fallingEdge) {
      index  := index - 1
      isDone := index === 0
    }
  }


  /**
    * Main state machine of the Master HAL
    */
  val smMaster = new StateMachine{

    val dataReceived   = Reg(Bits(g.dataWidth bits)) randBoot()

    val scl_en         = False
    val wr_sda         = True
    val wr_scl         = True

    // default value
    io.cmd.ready := False
    io.rsp.valid := False
    io.rsp.mode  := RspMode.ACK
    io.rsp.data  := dataReceived

    always{
      when(io.cmd.valid && io.cmd.mode === CmdMode.STOP){
          io.cmd.ready   := True
          goto(sStop)
      }

      when(io.cmd.valid && io.cmd.mode === CmdMode.START){
        io.cmd.ready   := True
        goto(sStart)
      }
    }

    val sIdle : State = new State with EntryPoint {
      whenIsActive {
        // nothing to do
      }
    }

    val sStart : State = new State {
      whenIsActive{
        scl_en := True
        wr_sda := !sclGenerator.triggerSequence

        // end of the stop sequence
        when(sclSampling.fallingEdge){
          goto(sData)
        }
      }
    }

    val sData : State = new State{
      onEntry{
        bitCounter.clear()
      }
      whenIsActive{
        scl_en := True
        wr_scl := io.cmd.valid

        // write data on bus and check collision
        when(io.cmd.mode === CmdMode.WRITE){
          wr_sda := io.cmd.data(bitCounter.index)

          when(sclSampling.risingEdge && io.config.enCollision){
            when(ccIO.rd_sda =/= wr_sda){
              io.rsp.mode  := RspMode.COLLISION
              io.rsp.valid := True
              goto(sIdle)
            }
          }
        }

        // Read data on bus
        when(sclSampling.risingEdge){
          dataReceived(bitCounter.index) := ccIO.rd_sda
        }

        // data sequence is done ?
        when(bitCounter.isDone){
          io.rsp.mode  := RspMode.DATA
          io.rsp.valid := True
          io.cmd.ready := !(io.cmd.mode === CmdMode.WRITE)
          goto(sACK)
        }
      }
    }

    val sACK : State = new State{
      whenIsActive{
        scl_en := True
        wr_scl := io.cmd.valid

        // write ACK
        wr_sda := !(io.cmd.mode === CmdMode.ACK)

        // read ACK
        when(sclSampling.risingEdge){
          io.rsp.mode  := (ccIO.rd_sda) ? RspMode.NACK | RspMode.ACK
          io.rsp.valid := True
        }

        // end of the ACK sequence ?
        when(sclSampling.fallingEdge){
          io.cmd.ready := True
          goto(sData)
        }
      }
    }

    val sStop : State = new State {
      whenIsActive{
        scl_en := True
        wr_sda := False

        when(sclGenerator.triggerSequence){
          goto(sIdle)
        }
      }
    }
  }

  io.i2c.sda.write := RegNext(smMaster.wr_sda)
  io.i2c.scl.write := RegNext(sclGenerator.scl) // && smMaster.wr_scl

  scl_en     := smMaster.scl_en

  sclFreeze  := !smMaster.wr_scl //smSynchSCL.freezeSCL || !smMaster.wr_scl

}






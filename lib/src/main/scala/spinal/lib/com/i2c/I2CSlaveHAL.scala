/******************************************************************************
  * I2C Slave HAL
  *
  * Write sequence :
  *
  *   RSP    :           NONE     ACK          ACK          NONE
  *   Master :   | START | DATA  |     | DATA  |     | STOP |
  *   Slave  :   |       |       | ACK |       | ACK |      |
  *   CMD    :       START    DATA          DATA         STOP
  *
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
  *   CMD    :
  *   Master :   | START |      | NACK | START | DATA  |     | STOP |
  *   Slave  :   |       | DATA |      |       |       | ACK |      |
  *   RSP    :
  */

package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._


/**
  * Global configuration of the I2C Slave
  *
  * @param dataWidth    : Width of the data send/read
  */
case class I2CSlaveHALGenerics(dataWidth  : Int =  8){}


/**
  * 5 different modes
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
  * 3 different modes
  *    DATA  : Send a data to the master
  *    NONE  : NO operation / NACK
  *    ACK   : ACK
  *   (FREEZE) is done with the stream
  */
object I2CSlaveHALRspMode extends SpinalEnum{
  val DATA, NONE, ACK = newElement() // @TODO FREEZE is done with stream...
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
  val cmd  = master Stream( I2CSlaveHALCmd(g) )
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


  /**
    * Rising and falling edge of the scl signal detection
    * @TODO oversampling the scl signals ???
    */
  val sclSampling = new Area{

    val risingEdge  = False
    val fallingEdge = False

    val scl_cur  = RegNext(ccIO.rd_scl) init(False)
    val scl_prev = RegNext(scl_cur)     init(False)

    when(scl_cur && scl_prev === False){
      risingEdge := True
    }

    when(scl_cur === False && scl_prev){
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
    when(sclSampling.scl_cur && sclSampling.scl_prev && sda_cur === False && sda_prev ){
      start   := True
    }

    // stop = rising edge of sda while the scl is 1
    when(sclSampling.scl_cur && sclSampling.scl_prev && sda_cur && sda_prev === False ){
      stop := True
    }
  }


  /**
    * Index counter (MSB is send first )
    */
  val bitCounter = new Area {

    val index = Reg(UInt(log2Up(g.dataWidth) bits)) init(g.dataWidth-1)

    def reset() = index := g.dataWidth-1
    def isOver : Bool = index === 0

    when(sclSampling.risingEdge) {
      index := index - 1
    }
  }


  /**
    * Main state machine
    */
  val smSlave = new StateMachine{

    import I2CSlaveHALCmdMode._

    val cmd          = RegInit(START)
    val dataReceived = Reg(Bits(g.dataWidth bits)) init(0)
    val wr_sda       = RegInit(True)
    val wr_clk       = RegInit(True)
    val waitCMDready = RegInit(False)
    val cmd_valid    = RegInit(False)
    val cmd_mode     = RegInit(CmdMode.STOP)

    // default value
    io.cmd.valid := False
    io.cmd.data  := 0
    io.cmd.mode  := CmdMode.STOP
    io.rsp.ready := False

    always{
      when(detector.stop)  { goto(sSTOP)  }
      when(detector.start) { goto(sSTART) }
    }


    when(io.cmd.ready){
      cmd_valid    := False
      waitCMDready := False
    }

    val sIDLE : State = new State with EntryPoint{
      whenIsActive{
        when(io.rsp.valid){
          io.rsp.ready := True
        }
        // no goto.. wait for a start signal...
      }
    }

    val sSTART : State = new State{
      whenIsActive{
        cmd_valid := True
        cmd_mode  := CmdMode.START

        goto(sWAIT_CMD)
      }
    }

    val sWAIT_CMD : State = new State{
      whenIsActive{
        when(sclSampling.fallingEdge){
          wr_sda := True
        }
        when(io.rsp.valid){
          switch(io.rsp.mode){
            is(RspMode.NONE){
              io.rsp.ready := True
              goto(sWAIT_BEFORE_WRITE)
            }
            is(RspMode.DATA){
              goto(sREAD)
            }
          }
        }
      }
    }

    val sWAIT_BEFORE_WRITE : State = new State{
      whenIsActive{
        when(sclSampling.fallingEdge){
          wr_sda := True
          bitCounter.reset()
          goto(sWRITE)
        }

      }
    }


    val sWRITE = new StateFsm(fsm=readSM(io.i2c.sda.read, dataReceived)){
      whenCompleted{

        cmd_valid    := True
        cmd_mode     := CmdMode.DATA
        waitCMDready := True

        goto(sWAIT_CMD_AFTER_WRITE)
      }
    }

    val sREAD = new StateParallelFsm (writeSM(wr_sda,io.rsp.data), readSM(ccIO.rd_sda, dataReceived)){
      whenCompleted{

        io.rsp.ready := True
        cmd_valid    := True
        cmd_mode     := CmdMode.DATA
        waitCMDready := True

        goto(sWAIT_CMD_AFTER_READ)
      }
    }

    val sWAIT_CMD_AFTER_WRITE : State = new State{
      whenIsActive{
        when(waitCMDready === False){
          wr_clk := True
          when(io.rsp.valid){
            switch(io.rsp.mode){
              is(RspMode.ACK, RspMode.NONE){
                goto(sWR_ACK)
              }
            }
          }
        }otherwise{
          when(sclSampling.fallingEdge){
            wr_clk := False
          }
        }
      }
    }

    val sWAIT_CMD_AFTER_READ : State = new State{
      whenIsActive{
        when(io.rsp.valid){
          io.rsp.ready := True
          when(io.rsp.mode === RspMode.NONE ){
            goto(sRD_ACK)
          }
        }
      }
    }

    val sWR_ACK : State = new State{
      whenIsActive{
        when(sclSampling.fallingEdge){
          io.rsp.ready := True
          wr_sda  := (io.rsp.mode === RspMode.ACK ) ? I2C.ACK |  I2C.NACK
        }
        when(sclSampling.risingEdge){
          goto(sWAIT_CMD)
        }
      }
    }

    val sRD_ACK : State = new State{
      whenIsActive{
        when(sclSampling.risingEdge){
          cmd_valid := True

          when(ccIO.rd_sda){
            cmd_mode  := CmdMode.NACK
          }otherwise{
            cmd_mode  := CmdMode.ACK
          }

          goto(sWAIT_CMD)
        }
      }
    }

    val sSTOP : State = new State{
      whenIsActive{

        cmd_valid := True
        cmd_mode  := CmdMode.STOP

        goto(sIDLE)
      }
    }

  }

  /**
    * Write a data on the I2C
    *
    * @param wr_sda    : The write signal of the sda
    * @param data2Send : Data that will be sent on the I2C
    */
  def writeSM(wr_sda : Bool, data2Send:Bits) = new StateMachine {

    val sWRITE: State = new State with EntryPoint {
      whenIsActive{
        when(sclSampling.fallingEdge){
          wr_sda := data2Send(bitCounter.index)
          when(bitCounter.isOver){
            exit()
          }
        }
      }
    }
  }


  /**
    * Read a data on the I2C bus
    *
    * @param sda           : The read signal of the sda
    * @param dataReceived  : Register that will contains the data receveid
    */
  def readSM(sda: Bool, dataReceived: Bits) = new StateMachine {
    val sREAD: State = new State with EntryPoint {
      whenIsActive{
        when(sclSampling.risingEdge){
          dataReceived(bitCounter.index) := sda
          when(bitCounter.isOver){
            exit()
          }
        }
      }
    }
  }



  io.i2c.scl.write := smSlave.wr_clk
  io.i2c.sda.write := smSlave.wr_sda

  io.cmd.valid := smSlave.cmd_valid
  io.cmd.data  := smSlave.dataReceived
  io.cmd.mode  := smSlave.cmd_mode


}

/******************************************************************************
  * I2C Slave HAL
  *          ________                       _______
  *         |        |<------- I2C ------->|       |
  *         | Master |                     | Slave |
  *         |________|      RSP Stream --->|_______|---> CMD Stream
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
case class I2CSlaveHALGenerics(dataWidth  : Int = 8){}


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
  *   (FREEZE) is done with the response stream. If no ready is received the bus if frozen
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
    */
  val sclSampling = new Area{

    val risingEdge  = False
    val fallingEdge = False

    val scl_cur  = RegNext(ccIO.rd_scl) init(False)
    val scl_prev = RegNext(scl_cur)     init(False)

    when(scl_cur && scl_prev === False){ risingEdge := True }

    when(scl_cur === False && scl_prev){ fallingEdge := True }
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
    * For counting the number of bit send/received (MSB is send first )
    */
  val bitCounter = new Area {

    val index = Reg(UInt(log2Up(g.dataWidth) bits)) init(g.dataWidth-1)

    def clear()  : Unit = index := g.dataWidth-1
    def isDone() : Bool = index === 0

    when(sclSampling.risingEdge) { index := index - 1 }
  }


  /**
    * Main state machine
    */
  val smSlave = new StateMachine{

    val dataReceived = Reg(Bits(g.dataWidth bits)) init(0)
    val wr_sda       = RegInit(True)
    val wr_scl       = RegInit(True)
    val cmd_valid    = RegInit(False)
    val cmd_mode     = RegInit(CmdMode.STOP)
    val busFreezed   = RegInit(False)

    // default value
    io.rsp.ready := False

    when(io.cmd.ready){ cmd_valid := False }

    always{
      when(detector.stop)  { goto(sSTOP)  }
      when(detector.start) { goto(sSTART) }
    }

    val sIDLE : State = new State with EntryPoint{
      // no goto.. wait for a start signal...
      whenIsActive{
        when(io.rsp.valid){ io.rsp.ready := True }
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
        freezeBusIfNeededOrDoThat{
          when(sclSampling.fallingEdge){ wr_sda := True }

          when(io.rsp.valid){
            switch(io.rsp.mode){
              is(RspMode.NONE){
                io.rsp.ready := True
                when(busFreezed){
                  busFreezed  := False
                  wr_sda := True
                  bitCounter.clear()
                  goto(sWRITE)
                }otherwise{
                  goto(sWAIT_BEFORE_WRITE)
                }
              }
              is(RspMode.DATA){
                busFreezed  := False
                bitCounter.clear()
                goto(sREAD)
              }
              default{
                busFreezed  := False
               // io.rsp.ready := True
                goto(sIDLE)
              }
            }
          }
        }
      }
    }

    val sWAIT_BEFORE_WRITE : State = new State{
      whenIsActive{
        when(sclSampling.fallingEdge){
          wr_sda := True
          bitCounter.clear()
          goto(sWRITE)
        }
      }
    }

    val sWRITE = new StateFsm(fsm=readSM(io.i2c.sda.read, dataReceived)){
      whenCompleted{
        cmd_valid    := True
        cmd_mode     := CmdMode.DATA

        goto(sWAIT_CMD_AFTER_WRITE)
      }
    }

    val sREAD = new StateParallelFsm (writeSM(wr_sda,io.rsp.data), readSM(ccIO.rd_sda, dataReceived)){
      whenCompleted{
        io.rsp.ready := True
        cmd_valid    := True
        cmd_mode     := CmdMode.DATA

        goto(sWAIT_CMD_AFTER_READ)
      }
    }

    val sWAIT_CMD_AFTER_WRITE : State = new State{
      whenIsActive{
        freezeBusIfNeededOrDoThat{
          when(io.rsp.valid){
            switch(io.rsp.mode){
              is(RspMode.ACK, RspMode.NONE){ goto(sRD_ACK) }
              default{
                io.rsp.ready := True
                goto(sIDLE)
              }
            }
          }
        }
      }
    }

    val sWAIT_CMD_AFTER_READ : State = new State{
      whenIsActive{
        freezeBusIfNeededOrDoThat{
          when(io.rsp.valid){
            io.rsp.ready := True
            when(io.rsp.mode === RspMode.NONE ){
              goto(sWR_ACK)
            }otherwise{
              io.rsp.ready := True
              goto(sIDLE)
            }
          }
        }
      }
    }

    val sWR_ACK : State = new State{
      whenIsActive{
        when(sclSampling.fallingEdge){
          wr_sda := True
        }
        when(sclSampling.risingEdge){
          cmd_valid := True
          cmd_mode  := ccIO.rd_sda ? CmdMode.NACK | CmdMode.ACK

          goto(sWAIT_NONE_AFTER_WR_ACK)
        }
      }
    }

    val sRD_ACK : State = new State{
      whenIsActive{
        when(sclSampling.fallingEdge){
          io.rsp.ready := True
          wr_sda       := (io.rsp.mode === RspMode.ACK ) ? I2C.ACK |  I2C.NACK
        }
        when(sclSampling.risingEdge){
          cmd_valid := True
          cmd_mode  := ccIO.rd_sda ? CmdMode.NACK | CmdMode.ACK
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

    val sWAIT_NONE_AFTER_WR_ACK : State = new State{
      whenIsActive{
        freezeBusIfNeededOrDoThat{
          when(io.rsp.valid){

            switch(io.rsp.mode){
              is(RspMode.NONE){
                io.rsp.ready := True
                goto(sWAIT_CMD)
              }
              is(RspMode.DATA){
                bitCounter.clear()
                goto(sREAD)
              }
              default{
                io.rsp.ready := True
                goto(sIDLE)
              }
            }

          }
        }
      }
    }

    /**
      * Used to freeze the bus if no cmd ready is received on time
      */
    def freezeBusIfNeededOrDoThat(doThat : => Unit) : Unit ={
      when(cmd_valid === False){
        wr_scl      := True

        doThat

      }otherwise{
        when(sclSampling.fallingEdge){
          wr_scl     := False
          busFreezed := True
        }
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
          when(bitCounter.isDone()){ exit() }
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
          when(bitCounter.isDone()){ exit() }
        }
      }
    }
  }

  io.i2c.scl.write := smSlave.wr_scl
  io.i2c.sda.write := smSlave.wr_sda

  io.cmd.valid := smSlave.cmd_valid
  io.cmd.data  := smSlave.dataReceived
  io.cmd.mode  := smSlave.cmd_mode
}

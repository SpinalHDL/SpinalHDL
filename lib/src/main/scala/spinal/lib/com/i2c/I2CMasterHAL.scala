/******************************************************************************
  * I2CMaster HAL
  *
  * Write : data write is read ...
  *
  */

// @TOTO some comments...

package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._


/**
  * Global configuration of the I2C Master
  *
  * @param dataWidth         : Width of the data send
  * @param clockDividerWidth : Width of the clockDivider value
  */
case class I2CMasterHALGenerics(dataWidth         : Int =  8,
                                clockDividerWidth : Int = 20  ){}


/**
  * Used to config the I2C master HAL at runtime
  */
case class I2CMasterHALConfig(g: I2CMasterHALGenerics) extends Bundle {

  val clockDivider = UInt (g.clockDividerWidth bits)

  def setFrequency(sclFrequency : Double, clkFrequency : Double = ClockDomain.current.frequency.getValue) : Unit = {
    clockDivider := (clkFrequency / sclFrequency * 2).toInt
  }
}


// @TODO modify comments... !!!
/**
  * 4 differents mode can be used to manage the I2C
  *    WRITE      : Send a data
  *    READ       : Read a data
  *    WRITECLOSE : Write the last byte and close the connection
  *    RESTART    : Send a restart sequence
  *    READCLOSE  : Read the last byte and close the connection
  *    ( The start sequence is automatically done )
  */
object I2CMasterHALCmdMode extends SpinalEnum{
  val WRITE, READ, WRITECLOSE, READCLOSE, READRESTART, WRITERESTART, IDLE= newElement()
}


/**
  * Define the command interface
  */
case class I2CMasteHALCmd(g : I2CMasterHALGenerics) extends Bundle{
  val mode   = I2CMasterHALCmdMode()
  val data   = Bits(g.dataWidth bits)
}


/**
  * Define the response interface
  */
case class I2CMasterHALRsp(g : I2CMasterHALGenerics) extends Bundle{
  val ack  = Bool
  val data = Bits(g.dataWidth bits)
}


/**
  * Definition of the component I2C Master HAL
  */
class I2CMasterHAL(g : I2CMasterHALGenerics) extends Component {

  import I2CMasterHALCmdMode._

  val io = new Bundle {

    val i2c    = master( I2C() )
    val config = in( I2CMasterHALConfig(g) )
    val cmd    = slave  Stream(I2CMasteHALCmd(g))
    val rsp    = master Flow(I2CMasterHALRsp (g))

  }



  // Enable/Disable the scl signal
  val scl_en = Bool // @TODO to be modified...



  /**
    * Generate the scl clock
    */
  val sclGenerator = new Area{

    val risingEdge      = False
    val fallingEdge     = False
    val triggerSequence = False
    val scl             = Reg(Bool) init(True)

    val counterValue = 50                 // @TODO !!! change at runtime this value !!!!!
    val counter =  Counter(counterValue)

    // start / stop the counter clock
    when(scl_en){
      counter.increment()
    }otherwise{
      scl := True
      counter.clear()
      // assert( scl === False , "Clock scl stop while it was low", WARNING)
    }

    // Generate the scl signal
    when(counter.willOverflowIfInc){
      scl := !scl

      // detect rising and falling edge
      when(scl){
        fallingEdge := True
      }otherwise{
        risingEdge  := True
      }
    }

    // Used to indicate when to generate the start/retard/stop sequence
    when(scl){
      when(counter.value === (counterValue/2).toInt){
        triggerSequence := True
      }
    }
  }


  /**
    * Counter of bit write/read
    */
  val counterBit = new Area{
    val index = Reg(UInt( log2Up(g.dataWidth) bits )) init(g.dataWidth-1)
    def isDone()    : Bool = index === 0
    def clear()     : Unit = index := g.dataWidth-1

    when(sclGenerator.risingEdge) {
      index := index - 1
    }
  }

  /**
    * State machine
    */
  val smMaster = new StateMachine{

    val scl_en       = Reg(Bool) init(False)
    val data2Send    = Reg(Bits(g.dataWidth bits)) init(0)
    val mode         = RegInit(IDLE)
    val wr_sda       = RegInit(True)
    val dataReceived = Reg(Bits(g.dataWidth bits)) init(0)
    val ack          = RegInit(False)


    io.cmd.ready := False
    io.rsp.valid := False
    io.rsp.ack   := I2C.NACK
    io.rsp.data  := 0

    val sIDLE : State = new State with EntryPoint{
      whenIsActive{

        when(io.cmd.valid){

          io.cmd.ready := True
          data2Send    := io.cmd.data
          mode         := io.cmd.mode

          switch(io.cmd.mode){
            is(WRITE)       { goto(sSTART) }
            is(READ)        { goto(sSTART) }
            is(WRITECLOSE)  { goto(sSTART) }
            is(READCLOSE)   { goto(sSTART) }
            is(READRESTART) { goto(sSTART) }
            is(WRITERESTART){ goto(sSTART) }
            default         { goto(sIDLE)  }
          }

        }
      }
    }

    val sWAIT_NEXT_CMD : State = new State {
      whenIsActive {
        // too late to received a new command
        when(sclGenerator.fallingEdge) {
          goto(sCLOSE)
        }
        when(io.cmd.valid) {
          io.cmd.ready := True
          data2Send    := io.cmd.data
          mode         := io.cmd.mode
          counterBit.clear()

          switch(io.cmd.mode) {
            is(WRITE)       { goto(sWRITE) }
            is(READ)        { goto(sWAIT_BEFORE_READ)  }
            is(WRITECLOSE)  { goto(sWRITE) }
            is(READCLOSE)   { goto(sWAIT_BEFORE_READ)  }
            is(READRESTART) { goto(sWAIT_BEFORE_READ) }
            is(WRITERESTART){ goto(sWRITE) }
            default         { goto(sIDLE)  }
          }
        }
      }
    }

    val sSTART : State = new State{
      whenIsActive{
        scl_en := True

        when(sclGenerator.triggerSequence){
          wr_sda := False
          counterBit.clear()

          switch(mode){
            is(WRITE)        { goto(sWRITE)            }
            is(READ)         { goto(sWAIT_BEFORE_READ) }
            is(WRITECLOSE)   { goto(sWRITE)            }
            is(READCLOSE)    { goto(sWAIT_BEFORE_READ) }
            is(READRESTART)  { goto(sWAIT_BEFORE_READ) }
            is(WRITERESTART) { goto(sWRITE)            }
            default          { goto(sIDLE)             }
          }
        }
      }
    }

    val sWRITE = new StateParallelFsm (writeSM(wr_sda,data2Send), readSM(io.i2c.sda.read, dataReceived)){
      whenCompleted{
        goto(sRD_ACK)
      }
    }

    val sRD_ACK : State = new State{
      whenIsActive{
        when(sclGenerator.fallingEdge){
          wr_sda := True
        }
        when(sclGenerator.risingEdge){
          ack := io.i2c.sda.read

          io.rsp.valid := True
          io.rsp.ack   := io.i2c.sda.read
          io.rsp.data  := dataReceived


          when(mode === WRITECLOSE){
            goto(sWAIT_BEFORE_CLOSE)
          }.elsewhen(mode === WRITERESTART){
            goto(sWAIT_BEFORE_RESTART)
          }otherwise{
            goto(sWAIT_NEXT_CMD)
          }
        }
      }
    }

    val sREAD = new StateFsm(fsm=readSM(io.i2c.sda.read, dataReceived)){
      whenCompleted{
        goto(sWR_ACK)
      }
    }

    val sWR_ACK = new State{
      whenIsActive{
        when(sclGenerator.fallingEdge){

          io.rsp.valid := True
          io.rsp.data  := dataReceived

          when(mode === READCLOSE) {
            wr_sda := I2C.NACK
            io.rsp.ack := I2C.NACK
            goto(sWAIT_BEFORE_CLOSE)
          }.elsewhen(mode === READRESTART){
            wr_sda := I2C.NACK
            io.rsp.ack := I2C.NACK
            goto(sWAIT_BEFORE_RESTART_RD)
          }otherwise{
            wr_sda     := I2C.ACK
            io.rsp.ack := I2C.ACK
            goto(sWAIT_NEXT_CMD)
          }

        }
      }
    }

    val sCLOSE : State = new State{
      whenIsActive{
        when(sclGenerator.triggerSequence){
          wr_sda := True
          scl_en := False
          goto(sIDLE)
        }
      }
    }

    val sWAIT_BEFORE_CLOSE : State = new State{
      whenIsActive{
        when(sclGenerator.fallingEdge){
          wr_sda := False
          goto ( sCLOSE )
        }
      }
    }

    val sWAIT_BEFORE_READ : State = new State{
      whenIsActive{
        when(sclGenerator.fallingEdge){
          wr_sda := True
          counterBit.clear()
          goto(sREAD)
        }
      }
    }

    val sWAIT_BEFORE_RESTART : State = new State{
      whenIsActive{
        when(sclGenerator.fallingEdge){
          wr_sda := True
        }
        when(sclGenerator.risingEdge){
          goto(sSTART)
        }
      }
    }

    val sWAIT_BEFORE_RESTART_RD : State = new State{
      whenIsActive{
        when(sclGenerator.risingEdge){
          goto(sWAIT_BEFORE_RESTART)
        }
      }
    }
  }


  /**
    * Write a data
    *
    * @param sda
    * @param data2Send
    * @return
    */
  def writeSM(sda : Bool, data2Send:Bits) = new StateMachine {

    val sWRITE: State = new State with EntryPoint {
      whenIsActive{
        when(sclGenerator.fallingEdge){
          sda := data2Send(counterBit.index)
          when(counterBit.isDone()){
            exit()
          }
        }
      }
    }
  }


  /**
    * Read a data
    *
    * @param sda
    * @param dataReceived
    * @return
    */
  def readSM(sda:Bool, dataReceived : Bits) = new StateMachine {
    val sREAD: State = new State with EntryPoint {
      whenIsActive{
        when(sclGenerator.risingEdge){
          dataReceived(counterBit.index) := sda
          when(counterBit.isDone()){
            exit()
          }
        }
      }
    }
  }

  io.i2c.sda.write := smMaster.wr_sda
  scl_en := smMaster.scl_en

  io.i2c.scl.write := sclGenerator.scl
}






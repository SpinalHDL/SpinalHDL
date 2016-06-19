/******************************************************************************
  * I2CMaster HAL
  *
  * Write : data write is read ...
  *
  */

// @TOTO some comments...
// @TOTO Synch inputs signals..

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


/**
  * 4 differents mode can be used to manage the I2C
  *    START      : Send the start/restart sequence
  *    WRITE      : Send a data
  *    READ       : Read a data
  *    STOP       : Send the stop sequence
  */
object I2CMasterHALCmdMode extends SpinalEnum{
  val START, WRITE, READ, STOP = newElement()
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
  val scl_en = Bool


  /**
    * Generate the scl clock
    */
  val sclGenerator = new Area{

    val risingEdge      = False
    val fallingEdge     = False
    val triggerSequence = False
    val scl             = RegInit(True)
    val cntValue        = Reg(UInt(g.clockDividerWidth bits)) init(0)

    // start / stop the counter clock
    when(scl_en){
      cntValue := cntValue + 1
      when(cntValue >= io.config.clockDivider ){
        cntValue := 0
      }
    }otherwise{
      scl := True
      cntValue := 0
      // assert( scl === False , "Clock scl stop while it was low", WARNING)
    }

    // Generate the scl signal
    when(cntValue === io.config.clockDivider){
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
      when(cntValue === (io.config.clockDivider >> 2)){
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

    val scl_en         = Reg(Bool) init(False)
    val data2Send      = Reg(Bits(g.dataWidth bits)) init(0)
    val mode           = RegInit(READ)
    val wr_sda         = RegInit(True)
    val dataReceived   = Reg(Bits(g.dataWidth bits)) init(0)
    val ack            = RegInit(False)
    val isPrevWriteOp  = Reg(Bool) init(True)
    val isFirstRead    = RegInit(True)
    val hasTransactionStarted = RegInit(False)

    // default value
    io.cmd.ready := False
    io.rsp.valid := False
    io.rsp.ack   := I2C.NACK
    io.rsp.data  := 0


    val sIDLE : State = new State with EntryPoint{
      whenIsActive{
        when(io.cmd.valid){

          io.cmd.ready := True
          mode         := io.cmd.mode
          data2Send    := io.cmd.data

          switch(io.cmd.mode){
            is(START){
              isFirstRead := True
              when(hasTransactionStarted === False){
                hasTransactionStarted := True
                goto(sSTART)
              }otherwise{
                when(isPrevWriteOp){
                  goto(sSTART)
                }otherwise{
                  ack := I2C.NACK
                  goto(sRD_ACK)
                }
              }
            }
            is(STOP) {
              hasTransactionStarted := False
              when(isPrevWriteOp){
                goto(sWAIT_BEFORE_STOP)
              }otherwise{
                ack := I2C.NACK // send a nack before stoping
                goto(sWR_ACK)
              }
            }
            is(READ) {
              isPrevWriteOp := False
              when(isFirstRead){
                isFirstRead := False
                goto(sWAIT_BEFORE_READ)
              }otherwise{
                ack := I2C.ACK
                goto(sWR_ACK)
              }
            }
            is(WRITE){
              isPrevWriteOp := True
              counterBit.clear()
              goto(sWRITE)
            }
          }
        }
      }
    }


    val sSTART : State = new State {
      whenIsActive{
        scl_en := True
        when(sclGenerator.triggerSequence){
            wr_sda := False
            goto(sIDLE)
        }
      }
    }

    val sREAD = new StateFsm(fsm=readSM(io.i2c.sda.read, dataReceived)){
      whenCompleted{
        goto(sIDLE)
      }
    }

    val sWRITE = new StateParallelFsm (writeSM(wr_sda,data2Send), readSM(io.i2c.sda.read, dataReceived)){
      whenCompleted{
        goto(sRD_ACK)
      }
    }

    val sRD_ACK : State = new State {
      whenIsActive{
        when(sclGenerator.fallingEdge){
          wr_sda := True
        }
        when(sclGenerator.risingEdge){

          val ackRead = io.i2c.sda.read
          ack := ackRead

          io.rsp.valid := True
          io.rsp.data  := dataReceived
          io.rsp.ack   := ackRead

          when(mode === START){
            goto(sSTART)
          }otherwise{
            goto(sIDLE)
          }
        }
      }
    }

    val sWR_ACK : State = new State {
      whenIsActive{
        when(sclGenerator.fallingEdge){
          wr_sda := ack
        }
        when(sclGenerator.risingEdge){

          io.rsp.valid := True
          io.rsp.data  := dataReceived
          io.rsp.ack   := I2C.ACK

          counterBit.clear()

          when(mode === STOP){
            goto(sWAIT_BEFORE_STOP)
          }otherwise{
            goto(sWAIT_BEFORE_READ)
          }
        }
      }
    }

    val sSTOP : State = new State {
      whenIsActive{
        when(sclGenerator.fallingEdge){
          wr_sda := False
        }
        when(sclGenerator.triggerSequence){
          scl_en := False
          wr_sda := True
          goto(sIDLE)
        }
      }
    }

    val sWAIT_BEFORE_STOP : State = new State{
      whenIsActive{
        when (sclGenerator.triggerSequence){
          goto(sSTOP)
        }
      }
    }

    val sWAIT_BEFORE_READ : State = new State{
      whenIsActive{
        when(sclGenerator.fallingEdge){
          wr_sda := True
          goto(sREAD)
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






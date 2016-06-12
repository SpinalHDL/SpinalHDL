/******************************************************************************
  * I2C master controller.
  *  - Single Master !!
  */

package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._


/**
  * Define all modes
  */
trait I2CMode { def frequency : Double }
case object Standard extends I2CMode { def frequency : Double = 100000 } // 100kHz
case object Fast     extends I2CMode { def frequency : Double = 400000 } // 400kHz
case object FastPlus extends I2CMode { def frequency : Double = 0 }



/**
  * Class to configure the I2C Master
  */
case class I2CMasterCtrConfig(modeAddr   : ADDR_MODE,
                              mode       : I2CMode ){

  def dataSize : Int = 8

  def isAddr10bits : Boolean  = modeAddr match {
    case ADDR_10bits => true
    case _           => false
  }

  def clockDivider : Int = (ClockDomain.current.frequency.getValue / mode.frequency * 2).toInt
}


/**
  * Define all state of the state machine for the I2C controller
  */
object I2CCtrlMasterState extends SpinalEnum {
  val IDLE, GEN_START, GEN_STOP_1, GEN_STOP_2, GEN_RESTART, WR_ADDR, RD_ACK, RD_DATA, WR_DATA, WR_ACK, WR_NACK = newElement()
}


/**
  * I2C : Master controller
  */
class I2CMasterCtrl(config: I2CMasterCtrConfig) extends Component{

  val io = new Bundle{
    val i2c         = master( I2C() )
    val read        = master Flow(Bits(config.dataSize bits))
    val write       = slave  Stream(Bits(config.dataSize bits))
    val start       = in Bool // pulse to start the sequence..
    val read_cmd    = slave( Event )
    val addrDevice  = in UInt(config.modeAddr.value bits)
    val errorAck    = out Bool
    val busy        = out Bool
  }

  /**
    * Synchronize input's signals of the I2C
    */
  val ccIO = new Area{
    val i2c_sda = BufferCC(io.i2c.sda.read)
  }

  val scl_en = Bool


  /**
    * Generate the scl clock
    */
  val scl_gen = new Area{
    val risingEdge      = False
    val fallingEdge     = False
    val triggerSequence = False
    val scl             = Reg(Bool) init(True)

    val counter =  Counter(config.clockDivider)

    // start / stop the clock generation
    when(scl_en){
      counter.increment()
    }otherwise{
      scl := True
      counter.clear()
    }

    // generate the scl signal
    when(counter.willOverflowIfInc){
      scl := !scl

      // detect rising and falling edge
      when(scl){
        fallingEdge := True
      }otherwise{
        risingEdge  := True
      }
    }

    // used to indicate when to generate the start/stop sequence
    when(scl){
      when(counter.value === (config.clockDivider/2).toInt){
        triggerSequence := True
      }
    }
  }

  // drive scl signal
  io.i2c.scl := scl_gen.scl


  /**
    * Counter of bit send
    */
  // @TODO implement the counter
  val counterIndex = new Area{
    val index = Reg(UInt( 32 bits )) init(config.dataSize-1) // @TODO change size
    def lastIndex() : Bool = index === 1
    def isOver()    : Bool = index === 0
    def clear()     : Unit = index := config.dataSize-1
  }


  io.busy := False // @TODO manage the busy

  assert(io.write.valid && io.read_cmd.valid, "Can't perform a read and write simultaneously", WARNING)

  /**
    * I2C : Master state machine
    */
  val stateMachine = new Area {

    import I2CCtrlMasterState._

    val state      = RegInit(IDLE)
    val data       = Reg(Bits(config.dataSize bits)) init(0)
    val addrDevice = Reg(UInt(config.modeAddr.value bits)) init(0)
    val clk_en     = Reg(Bool) init(False)
    val sda        = Reg(Bool) init(True)
    val rw         = Reg(Bool) init(False)

    io.errorAck       := False
    io.read_cmd.ready := True
    io.write.ready    := True
    io.read.valid     := False
    io.read.payload   := 0

    switch(state){
      is(IDLE){
        when(io.start){
          addrDevice := io.addrDevice
          state      := GEN_START
        }
      }
      is(GEN_START){
        clk_en := True
        when(scl_gen.triggerSequence){
          sda   := False
          state := WR_ADDR
          counterIndex.clear()
        }
      }
      is(GEN_RESTART){
        when(scl_gen.fallingEdge){
          state := GEN_START
        }
      }
      is(GEN_STOP_1){
        when(scl_gen.fallingEdge){
          sda   := False
          state := GEN_STOP_2
        }
      }
      is(GEN_STOP_2){
        when(scl_gen.triggerSequence){
          sda   := True
          state := IDLE
        }
      }
      is(WR_ADDR){
        when(scl_gen.fallingEdge){
          when(counterIndex.isOver){
            state := RD_ACK
          }

          when(counterIndex.lastIndex){

            when(io.write.valid){
              sda := False
              rw  := False
            }.elsewhen(io.read_cmd.valid){
              sda := True
              rw  := True
            }otherwise{
              report(
                message   = "No read or write to execute ",
                severity  = NOTE
              )
              state       := GEN_STOP_1 // @TODO maybe manage better this case (wait ack from slave before stopping)
              io.errorAck := True
            }

          }otherwise{
            sda := addrDevice(counterIndex.index-1)
          }
        }
      }
      is(RD_ACK){

        when(scl_gen.risingEdge){

          // NACK received
          when(io.i2c.sda.read){

            io.errorAck := True
            state       := GEN_STOP_1
            report(
              message   = "NACK received ",
              severity  = NOTE
            )

          // ACK received
          }otherwise{
            counterIndex.clear()
            // succession of read
            when(io.read_cmd.valid && rw){
              state := RD_DATA
              io.read_cmd.ready   := False
              rw  := True
            // sucession of write
            }.elsewhen(io.write.valid && rw === False) {
              data := io.write.payload
              io.write.ready := False
              state := WR_DATA
            // write after read, or read after write
            }.elsewhen( (io.write.valid && rw) || (io.read_cmd.valid && rw === False) ){
              state := GEN_RESTART
            }otherwise{
                state := GEN_STOP_1
                report(
                  message   = "No read/write operation ready",
                  severity  = NOTE
                )
            }
          }
        }
      }
      is(WR_DATA){

        io.write.ready := True

        when(scl_gen.fallingEdge){
          sda := data(counterIndex.index)
        }

        when(scl_gen.risingEdge){
          when(counterIndex.isOver()){
            state := RD_ACK
          }
        }
      }
      is(RD_DATA){
        when(scl_gen.risingEdge){
          data(counterIndex.index) := ccIO.i2c_sda

          when(counterIndex.isOver){
            state := io.read_cmd.valid ? WR_ACK | WR_NACK
            io.read.payload := data // @TODO to put somewhere else because data is not correct yet
            io.read.valid := True
          }
        }
      }
      is(WR_ACK){
        when(scl_gen.fallingEdge){
          sda   := False
          state := RD_ACK
        }
      }
      is(WR_NACK){
        when(scl_gen.fallingEdge){
          sda   := True
          state := GEN_STOP_1
        }
      }
    }

  }

  io.i2c.sda.write := stateMachine.sda
  scl_en := stateMachine.clk_en
}



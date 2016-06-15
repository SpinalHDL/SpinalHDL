/******************************************************************************
  * I2C master controller.
  *  - Single Master !!
  */

package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._



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

    when(scl_gen.risingEdge) {
      index := index - 1
    }

  }


  io.busy := False // @TODO manage the busy

 // assert( io.write.valid && io.read_cmd.valid, "Can't perform a read and write simultaneously ", WARNING)


  /**
    * I2C : Master state machine
    */

  val masterSM = new StateMachine{

    val data       = Reg(Bits(config.dataSize bits))       init(0)
    val addrDevice = Reg(UInt(config.modeAddr.value bits)) init(0)
    val clk_en     = Reg(Bool) init(False)
    val sda        = Reg(Bool) init(True)
    val rw         = Reg(Bool) init(False)

    io.errorAck       := False
    io.read_cmd.ready := False
    io.write.ready    := False
    io.read.valid     := False
    io.read.payload   := 0

    val IDLE : State = new State with EntryPoint{
      whenIsActive{
        when(io.start){
          addrDevice := io.addrDevice
          goto(GEN_START)
        }
      }
    }

    val GEN_START : State = new State{
      whenIsActive{
        clk_en := True
        when(scl_gen.triggerSequence){
          sda   := False
          counterIndex.clear()
          goto(WR_ADDR)
        }
      }
    }

    val WR_ADDR : State = new State{
      whenIsActive{
        when(scl_gen.fallingEdge){
          when(counterIndex.isOver){

            goto(WAIT_END_TX_DATA)

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
              goto(GEN_STOP_1) // @TODO maybe manage better this case (wait ack from slave before stopping)
              io.errorAck := True
            }

          }otherwise{
            sda := addrDevice(counterIndex.index-1)
          }
        }
      }
    }

    val WAIT_END_TX_DATA : State = new State{
      whenIsActive{
        when(scl_gen.risingEdge){
          goto(RD_ACK)
        }
      }
    }

    val RD_ACK : State = new State{
      whenIsActive{
        when(scl_gen.fallingEdge){
          sda := True
        }
        when(scl_gen.risingEdge){

          // NACK received
          when(ccIO.i2c_sda){

            io.errorAck := True
            goto(GEN_STOP_1)

            report(
              message   = "NACK received ",
              severity  = WARNING
            )

          // ACK received
          }otherwise{

            counterIndex.clear()

            // succession of read
            when(io.read_cmd.valid && rw){

              io.read_cmd.ready   := True
              rw                  := True

              goto(RD_DATA)

            // succession of write
            }.elsewhen(io.write.valid && rw === False) {

              data           := io.write.payload
              io.write.ready := True
              goto(WR_DATA)

            // write after read, or read after write
            }.elsewhen( (io.write.valid && rw) || (io.read_cmd.valid && rw === False) ){

              goto(GEN_RESTART)

            }otherwise{

              report(
                message   = "No read/write operation ready",
                severity  = WARNING
              )

              goto(GEN_STOP_1)

            }
          }
        }
      }
    }

    val RD_DATA : State = new State{
      whenIsActive{
        when(scl_gen.risingEdge){
          data(counterIndex.index) := ccIO.i2c_sda

          when(counterIndex.isOver){
            when(io.read_cmd.valid){
              goto(WR_ACK)
            }otherwise{
              goto(WR_NACK)
            }
            io.read.payload := data // @TODO to put somewhere else because data is not correct yet
            io.read.valid   := True
          }
        }
      }
    }

    val WR_DATA : State = new State{

      whenIsActive{
        when(scl_gen.fallingEdge){
          sda := data(counterIndex.index)
        }

        when(scl_gen.risingEdge){
          when(counterIndex.isOver()){
            goto(RD_ACK)
          }
        }
      }
    }

    val GEN_RESTART : State = new State{
      whenIsActive{
        when(scl_gen.fallingEdge){
          goto(GEN_START)
        }
      }
    }

    val GEN_STOP_1 : State = new State{
      whenIsActive{
        when(scl_gen.fallingEdge){
          sda   := False
          goto(GEN_STOP_2)
        }
      }
    }

    val GEN_STOP_2 : State = new State{
      whenIsActive{
        when(scl_gen.triggerSequence){
          sda    := True
          clk_en := False
          goto(IDLE)
        }
      }
    }

    val WR_ACK  : State  = new State{
      whenIsActive {
        when(scl_gen.fallingEdge) {
          sda := False
          goto(RD_ACK)
        }
      }
    }

    val WR_NACK : State = new State{
      whenIsActive {
        when(scl_gen.fallingEdge) {
          sda := True
          goto(GEN_STOP_1)
        }
      }
    }

  }


  io.i2c.sda.write := masterSM.sda
  scl_en           := masterSM.clk_en
}



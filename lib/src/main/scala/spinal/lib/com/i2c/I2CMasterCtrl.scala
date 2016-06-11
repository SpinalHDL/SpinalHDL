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
  val IDLE, GEN_START, GEN_STOP_1, GEN_STOP_2, WR_ADDR, RD_ACK, RD_DATA, WR_DATA = newElement()
}


/**
  * I2C : Master controller
  */
class I2CMasterCtrl(config: I2CMasterCtrConfig) extends Component{

  val io = new Bundle{
    val i2c        = slave( I2C() )
    val read       = master Flow(Bits(config.dataSize bits))
    val write      = slave  Stream(Bits(config.dataSize bits))
    val write_en   = in Bool
    val read_en    = in Bool
    val addrDevice = in UInt(config.modeAddr.value bits)
    val errorAck   = out Bool
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
      scl      := !scl

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


  assert(io.write_en && io.read_en, "Error write and read can't occur together", FAILURE)


  /**
    * I2C : Master state machine
    */
  val stateMachine = new Area {

    import I2CCtrlMasterState._

    val state      = RegInit(IDLE)
    val data2Send  = Reg(Bits(config.dataSize bits)) init(0)
    val addrDevice = Reg(UInt(config.modeAddr.value bits)) init(0)
    val clk_en     = Reg(Bool) init(False)
    val sda        = Reg(Bool) init(True)
    val rw         = Reg(Bool) init(False)


    io.write.ready := True // @TODO manage this signal
    io.errorAck    := False

    switch(state){
      is(IDLE){
        when((io.write_en || io.read_en)){
          addrDevice := io.addrDevice
          state      := GEN_START
          rw         := io.read_en
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
            sda   := rw
          }otherwise{
            sda := addrDevice(counterIndex.index-1)
          }
        }
      }
      is(RD_ACK){
        when(scl_gen.risingEdge){
          when(io.i2c.sda.read){
            io.errorAck := True
            state       := GEN_STOP_2
            report(
              message   = "NACK received ",
              severity  = NOTE
            )

          }otherwise{
            state := rw ? RD_DATA | WR_DATA
          }
        }
      }
      is(WR_ADDR){ // @TODO store somewhere the data input
        when(scl_gen.fallingEdge){
          sda := data2Send(counterIndex.index)
          when(counterIndex.isOver()){
            state := RD_ACK
          }
        }

      }
    }

  }

  io.i2c.sda.write := stateMachine.sda
  scl_en := stateMachine.clk_en





}



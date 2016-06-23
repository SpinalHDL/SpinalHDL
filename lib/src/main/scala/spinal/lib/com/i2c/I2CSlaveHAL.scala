/******************************************************************************
  * I2C Slave HAL
  *
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


object I2CSlaveHALCmdMode extends SpinalEnum{
  val START, NACK, ACK, STOP, DATA = newElement()

}

case class I2CSlaveHALCmd(g : I2CSlaveHALGenerics) extends Bundle{
  val mode  = I2CSlaveHALCmdMode()
  val data  = Bits(g.dataWidth bits)
}

object I2CSlaveHALRspMode extends SpinalEnum{
  val DATA, NONE, ACK = newElement()
}

case class I2CSlaveHALRsp(g : I2CSlaveHALGenerics) extends Bundle{
  val mode  = I2CSlaveHALRspMode()
  val data  = Bits(g.dataWidth bits)
}

case class I2CSlaveHALio(g : I2CSlaveHALGenerics) extends Bundle{
  val i2c  = slave( I2C() )
  val cmd  = master Stream( I2CSlaveHALCmd(g) )
  val rsp  = slave  Stream( I2CSlaveHALRsp(g) )
}


class I2CSlaveHAL(g : I2CSlaveHALGenerics) extends Component{

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

    val sda_cur  = RegNext(ccIO.rd_scl) init(False)
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
    * Index counter (MSB is send frist )
    */
  val bitCounter = new Area {

    val index = Reg(UInt(log2Up(g.dataWidth) bits)) init(g.dataWidth-1)

    def reset() = index := g.dataWidth-1
    def isOver : Bool = index === 0

    when(sclSampling.risingEdge) {
      index := index - 1
    }
  }

  val smSlave = new StateMachine{

    import I2CSlaveHALCmdMode._

    val cmd = RegInit(START)
    val data = Reg(Bits(g.dataWidth bits)) init(0)


    val sIDLE : State = new State with EntryPoint{
      whenIsActive{
        when(io.cmd.valid){
          cmd  := io.cmd.mode
          data := io.cmd.data

          switch(io.cmd.mode){
            is(START)  { goto(sIDLE) }
            is(START) { goto(sIDLE) }
          }
        }
      }
    }

  }

  io.i2c.scl.write := False
  io.cmd.ready := False
  io.rsp.mode := I2CSlaveHALRspMode.ACK
  io.rsp.data := 0
  io.i2c.sda.write := False
  io.rsp.valid := False



}

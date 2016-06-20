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
  val READ, WRITE, FREEZE, ACK = newElement()
}

case class I2CSlaveHALCmd(g : I2CSlaveHALGenerics) extends Bundle{
  val mode  = I2CSlaveHALCmdMode()
  val data  = Bits(g.dataWidth bits)
}

object I2CSlaveHALRspMode extends SpinalEnum{
  val READ, START, STOP, ACK = newElement()
}

case class I2CSlaveHALRsp(g : I2CSlaveHALGenerics) extends Bundle{
  val mode  = I2CSlaveHALRspMode()
  val data  = Bits(g.dataWidth bits)
}


class I2CSlaveHAL extends Component{

  val io = new Bundle {
    val i2c  = slave( I2C() )
    val cmd  = master Stream( I2CSlaveHALCmd() )
    val rsp  = slave  Flow  ( I2CSlaveHALRsp() )
  }

}

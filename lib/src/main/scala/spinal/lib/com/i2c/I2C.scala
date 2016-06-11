package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
/**
  * Define an open drai port
  */
case class ReadableOpenDrain[T<: Data](dataType : T) extends Bundle with IMasterSlave{
  val write,read = dataType.clone()

  override def asMaster(): this.type = {
    out(write)
    in(read)
    this
  }

  override def asSlave(): this.type = asMaster()
}


/**
  * Interface definition
  */
case class I2C() extends Bundle with IMasterSlave {
  val sda   = ReadableOpenDrain(Bool)
  val scl   = Bool

  override def asMaster(): this.type = {
    out(scl)
    master(sda)
    this
  }

  // @TODO try to remove this ..
  override def asSlave(): this.type = {
    in(scl)
    slave(sda)
    this
  }
}


/**
  * Define I2C constant
  */
object I2C {
  def ACK  = False
  def NACK = True
}
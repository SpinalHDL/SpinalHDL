package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.io._


/**
  * I2C interface definition
  */
case class I2C() extends Bundle with IMasterSlave {

  val sda   = ReadableOpenDrain(Bool)
  val scl   = Bool

  override def asMaster(): this.type = {
    out(scl)
    master(sda)
    this
  }

  override def asSlave(): this.type = {
    in(scl)
    master(sda)
    this
  }
}


/**
  * Define I2C constants
  */
object I2C {
  def ACK  = False
  def NACK = True
}
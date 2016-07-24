package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.io._


/**
  * I2C interface definition
  */
case class I2C() extends Bundle with IMasterSlave {

  val sda   = ReadableOpenDrain(Bool)
  val scl   = ReadableOpenDrain(Bool)

  override def asMaster(): this.type = {
    master(scl)
    master(sda)
    this
  }

  override def asSlave(): this.type = {
    master(scl)
    master(sda)
    this
  }
}


/**
  * Counter of bit write/read.
  * MSB is send first on the I2C bus so the counter goes from dataWdith-1 to 0
  */
class I2CBitCounter(sclFallingEdge : Bool, dataWidth : Int) extends Area {

  val index  = Reg(UInt(log2Up(dataWidth) bits)) randBoot()
  val isDone = False

  def clear() : Unit = index := dataWidth-1

  when(sclFallingEdge) {
    index  := index - 1
    isDone := index === 0
  }
}


/**
  * Detect the rising and falling Edge of the SCL signals
  */
class SCLEdgeDetector(scl:Bool) extends Area{
  val scl_prev = RegNext(scl) init(True)

  val rising  =  scl && !scl_prev
  val falling = !scl && scl_prev
}


/**
  * Filter the SCL and SDA input signals
  */
class I2CFilterInput(i2c_sda:Bool, i2c_scl:Bool, clockDivider : UInt, samplingSize:Int, clockDividerWidth : Int) extends Area{

  // Clock divier for sampling the input signals
  val samplingClockDivider = new Area{
    val counter = Reg(UInt(clockDividerWidth bits)) init(0)
    val tick    = counter === 0

    counter := counter - 1
    when(tick){ counter := clockDivider }
  }

  // Input sampling
  val samplingInput = new Area {
    val cc_scl = BufferCC(i2c_scl)
    val cc_sda = BufferCC(i2c_sda)

    val sdaSamples = History(that = cc_sda, length = samplingSize, when = samplingClockDivider.tick, init = True)
    val sclSamples = History(that = cc_scl, length = samplingSize, when = samplingClockDivider.tick, init = True)

    val sda = RegNext(MajorityVote(sdaSamples))
    val scl = RegNext(MajorityVote(sclSamples))
  }

  val sda = samplingInput.sda
  val scl = samplingInput.scl
}


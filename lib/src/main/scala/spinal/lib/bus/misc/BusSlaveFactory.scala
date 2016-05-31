package spinal.lib.bus.misc

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer



trait BusSlaveFactory  extends Area{

  def busDataWidth : Int
  
  def read(that : Data,
           address : BigInt,
           bitOffset : Int = 0) : Unit
  
  def write(that : Data,
            address : BigInt,
            bitOffset : Int = 0) : Unit

  def onWrite(address : BigInt)(doThat : => Unit) : Unit
  def onRead (address : BigInt)(doThat : => Unit) : Unit

  def nonStopWrite( that : Data,
                    address : BigInt,
                    bitOffset : Int = 0) : Unit

  def multiRead(that : Data,
                address : BigInt,
                onRsp :  (BigInt) => Unit = (x) => {}) : Unit  = {
    val wordCount = (widthOf(that) - 1) / busDataWidth + 1
    val valueBits = that.asBits.resize(wordCount*busDataWidth)
    val words = (0 until wordCount).map(id => valueBits(id * busDataWidth , busDataWidth bit))
    for (wordId <- (0 until wordCount)) {
      read(words(wordId), address + wordId*busDataWidth/8)
    }
  }

  def multiWrite(that : Data,
                 address : BigInt,
                 onCmd : (BigInt) => Unit = (x) => {}) : Unit  = {
    val wordCount = (widthOf(that) - 1) / busDataWidth + 1
    for (wordId <- (0 until wordCount)) {
      write(
        that = new DataWrapper{
          override def getBitsWidth: Int =
            Math.min(busDataWidth, widthOf(that) - wordId * busDataWidth)

          override def assignFromBits(value : Bits): Unit = {
            that.assignFromBits(
              bits     = value.resized,
              offset   = wordId * busDataWidth,
              bitCount = getBitsWidth bits)
          }
        },
        address = address + wordId * busDataWidth / 8,0)
    }
  }

  def writeOnlyRegOf[T <: Data](dataType: T,
                                baseAddress: BigInt,
                                bitOffset : Int = 0): T = {
    val ret = Reg(dataType)
    write(ret,baseAddress,bitOffset)
    ret
  }
  def writeReadRegOf[T <: Data](that: T,
                                baseAddress: BigInt,
                                bitOffset : Int = 0): T = {
    val reg = Reg(that)
    write(reg,baseAddress,bitOffset)
    read(reg,baseAddress,bitOffset)
    reg
  }

  def cumulateBitsAndClearOnRead(that : Bits,
                                 base : BigInt,
                                 bitOffset : Int = 0): Unit ={
    assert(that.getWidth <= busDataWidth)
    val reg = Reg(that.clone)
    reg := reg | that
    read(reg,base,bitOffset)
    onRead(base){
      reg := that
    }
  }


  def drive(that : Data,
            address : BigInt,
            bitOffset : Int = 0) : Unit = {
    val reg = Reg(that)
    write(reg,address,bitOffset)
    that := reg
  }

  def driveFlow[T <: Data](that : Flow[T],
                           baseAddress: BigInt,
                           bitOffset : Int = 0) : Unit = {
    that.valid := False
    onWrite(baseAddress){
      that.valid := True
    }
    nonStopWrite(that.payload,baseAddress,bitOffset)
  }


  def createFlow[T <: Data](dataType : T,
                            baseAddress: BigInt,
                            bitOffset : Int = 0) : Flow[T] = {
    val flow = Flow(dataType)
    driveFlow(flow,baseAddress,bitOffset)
    flow
  }

  def readStreamNonBlocking[T <: Data] (that : Stream[T],
                                        baseAddress: BigInt,
                                        bitOffset : Int = 0) : Unit = {
    val flow = Flow(that.dataType)
    flow.valid := that.valid
    flow.payload := that.payload
    that.ready := False
    onRead(baseAddress){
      that.ready := True
    }
    read(flow,baseAddress,bitOffset)
  }
}



trait BusSlaveFactoryElement

case class BusSlaveFactoryRead(that : Data,
                               address : BigInt,
                               bitOffset : Int) extends BusSlaveFactoryElement

case class BusSlaveFactoryWrite(that : Data,
                                address : BigInt,
                                bitOffset : Int) extends BusSlaveFactoryElement

case class BusSlaveFactoryOnWrite(address : BigInt,
                                  doThat : () => Unit) extends BusSlaveFactoryElement

case class BusSlaveFactoryOnRead( address : BigInt,
                                  doThat : () => Unit) extends BusSlaveFactoryElement

case class BusSlaveFactoryNonStopWrite(that : Data,
                                address : BigInt,
                                bitOffset : Int) extends BusSlaveFactoryElement


trait BusSlaveFactoryDelayed extends BusSlaveFactory{
  val elements = ArrayBuffer[BusSlaveFactoryElement]()
  val elementsPerAddress = collection.mutable.HashMap[BigInt,ArrayBuffer[BusSlaveFactoryElement]]()
  override def read(that : Data,
           address : BigInt,
           bitOffset : Int = 0) : Unit  = {
    assert(bitOffset + that.getBitsWidth <= busDataWidth)
    val e = BusSlaveFactoryRead(that,address,bitOffset)
    elements += e
    elementsPerAddress.getOrElseUpdate(address,ArrayBuffer[BusSlaveFactoryElement]()) += e
  }

  override def write(that : Data,
            address : BigInt,
            bitOffset : Int = 0) : Unit  = {
    assert(bitOffset + that.getBitsWidth <= busDataWidth)
    val e = BusSlaveFactoryWrite(that,address,bitOffset)
    elements += e
    elementsPerAddress.getOrElseUpdate(address,ArrayBuffer[BusSlaveFactoryElement]()) += e
  }

  def onWrite(address : BigInt)(doThat : => Unit) : Unit = {
    val e = BusSlaveFactoryOnWrite(address,() => doThat)
    elements += e
    elementsPerAddress.getOrElseUpdate(address,ArrayBuffer[BusSlaveFactoryElement]()) += e
  }
  def onRead (address : BigInt)(doThat : => Unit) : Unit = {
    val e = BusSlaveFactoryOnRead(address,() => doThat)
    elements += e
    elementsPerAddress.getOrElseUpdate(address,ArrayBuffer[BusSlaveFactoryElement]()) += e
  }

  def nonStopWrite( that : Data,
                    address : BigInt,
                    bitOffset : Int = 0) : Unit = {
    assert(bitOffset + that.getBitsWidth <= busDataWidth)
    val e = BusSlaveFactoryNonStopWrite(that,address,bitOffset)
    elements += e
    elementsPerAddress.getOrElseUpdate(address,ArrayBuffer[BusSlaveFactoryElement]()) += e
  }


  def build() : Unit

  component.addPrePopTask(() => build())
}
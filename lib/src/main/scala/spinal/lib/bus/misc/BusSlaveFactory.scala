package spinal.lib.bus.misc

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer



trait BusSlaveFactory  extends Area{

  def busDataWidth : Int
  
  def read(that : Data,
           address : BigInt,
           bitOffset : Int = 0) : Unit
  
  def write[T <: Data](that : T,
            address : BigInt,
            bitOffset : Int = 0) : T

  def onWrite(address : BigInt)(doThat : => Unit) : Unit
  def onRead (address : BigInt)(doThat : => Unit) : Unit

  def nonStopWrite( that : Data,
                    bitOffset : Int = 0) : Unit
                    
  def readAndWrite(that : Data,
                   address: BigInt,
                   bitOffset : Int = 0): Unit = {
    write(that,address,bitOffset)
    read(that,address,bitOffset)
  }


  def isWriting(address : BigInt) : Bool = {
    val ret = False
    this.onWrite(address){
      ret := True
    }
    ret
  }

  def isReading (address : BigInt) : Bool = {
    val ret = False
    this.onRead(address){
      ret := True
    }
    ret
  }


  def readMultiWord(that : Data,
                address : BigInt) : Unit  = {
    val wordCount = (widthOf(that) - 1) / busDataWidth + 1
    val valueBits = that.asBits.resize(wordCount*busDataWidth)
    val words = (0 until wordCount).map(id => valueBits(id * busDataWidth , busDataWidth bit))
    for (wordId <- (0 until wordCount)) {
      read(words(wordId), address + wordId*busDataWidth/8)
    }
  }

  def writeMultiWord(that : Data,
                 address : BigInt) : Unit  = {
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
        address = address + wordId * busDataWidth / 8,0
      )
    }
  }


  def createWriteOnly[T <: Data](dataType: T,
                                 address: BigInt,
                                 bitOffset : Int = 0): T = {
    val ret = Reg(dataType)
    write(ret,address,bitOffset)
    ret
  }

  def createReadWrite[T <: Data](dataType: T,
                                 address: BigInt,
                                 bitOffset : Int = 0): T = {
    val reg = Reg(dataType)
    write(reg,address,bitOffset)
    read(reg,address,bitOffset)
    reg
  }

  def createAndDriveFlow[T <: Data](dataType : T,
                                 address: BigInt,
                                 bitOffset : Int = 0) : Flow[T] = {
    val flow = Flow(dataType)
    driveFlow(flow,address,bitOffset)
    flow
  }

  def doBitsAccumulationAndClearOnRead(   that : Bits,
                                          address : BigInt,
                                          bitOffset : Int = 0): Unit = {
    assert(that.getWidth <= busDataWidth)
    val reg = Reg(that)
    reg := reg | that
    read(reg,address,bitOffset)
    onRead(address){
      reg := that
    }
  }


  def drive[T <: Data](that : T,
            address : BigInt,
            bitOffset : Int = 0) : T = {
    val reg = Reg(that)
    write(reg,address,bitOffset)
    that := reg
    reg
  }

  def driveAndRead[T <: Data](that : T,
                   address : BigInt,
                   bitOffset : Int = 0) : T = {
    val reg = Reg(that)
    write(reg,address,bitOffset)
    read(reg,address,bitOffset)
    that := reg
    reg
  }

  def driveFlow[T <: Data](that : Flow[T],
                           address: BigInt,
                           bitOffset : Int = 0) : Unit = {
    that.valid := False
    onWrite(address){
      that.valid := True
    }
    nonStopWrite(that.payload,bitOffset)
  }

  
  def readStreamNonBlocking[T <: Data] (that : Stream[T],
                                        address: BigInt,
                                        validBitOffset : Int,
                                        payloadBitOffset : Int) : Unit = {
    that.ready := False
    onRead(address){
      that.ready := True
    }
    read(that.valid  ,address,validBitOffset)
    read(that.payload,address,payloadBitOffset)
  }
}



trait BusSlaveFactoryElement

// Ask to make `that` readable when a access is done on `address`.
// bitOffset specify where `that` is placed on the answer
case class BusSlaveFactoryRead(that : Data,
                               address : BigInt,
                               bitOffset : Int) extends BusSlaveFactoryElement

// Ask to make `that` writable when a access is done on `address`.
// bitOffset specify where `that` get bits from the request
case class BusSlaveFactoryWrite(that : Data,
                                address : BigInt,
                                bitOffset : Int) extends BusSlaveFactoryElement

// Ask to execute `doThat` when a write access is done on `address`
case class BusSlaveFactoryOnWrite(address : BigInt,
                                  doThat : () => Unit) extends BusSlaveFactoryElement

// Ask to execute `doThat` when a read access is done on `address`
case class BusSlaveFactoryOnRead( address : BigInt,
                                  doThat : () => Unit) extends BusSlaveFactoryElement

// Ask to constantly drive `that` with the data bus
// bitOffset specify where `that` get bits from the request
case class BusSlaveFactoryNonStopWrite(that : Data,
                                       bitOffset : Int) extends BusSlaveFactoryElement

trait BusSlaveFactoryDelayed extends BusSlaveFactory{
  val elements = ArrayBuffer[BusSlaveFactoryElement]()
  val elementsPerAddress = collection.mutable.HashMap[BigInt,ArrayBuffer[BusSlaveFactoryElement]]()
  private def addAddressableElement(e : BusSlaveFactoryElement,address : BigInt) = {
    elements += e
    elementsPerAddress.getOrElseUpdate(address, ArrayBuffer[BusSlaveFactoryElement]()) += e
  }

  override def read(that : Data,
           address : BigInt,
           bitOffset : Int = 0) : Unit  = {
    assert(bitOffset + that.getBitsWidth <= busDataWidth)
    addAddressableElement(BusSlaveFactoryRead(that,address,bitOffset),address)
  }

  override def write[T <: Data](that : T,
            address : BigInt,
            bitOffset : Int = 0) : T  = {
    assert(bitOffset + that.getBitsWidth <= busDataWidth)
    addAddressableElement(BusSlaveFactoryWrite(that,address,bitOffset),address)
    that
  }

  def onWrite(address : BigInt)(doThat : => Unit) : Unit = {
    addAddressableElement(BusSlaveFactoryOnWrite(address,() => doThat),address)
  }
  def onRead (address : BigInt)(doThat : => Unit) : Unit = {
    addAddressableElement(BusSlaveFactoryOnRead(address,() => doThat),address)
  }

  def nonStopWrite( that : Data,
                    bitOffset : Int = 0) : Unit = {
    assert(bitOffset + that.getBitsWidth <= busDataWidth)
    elements += BusSlaveFactoryNonStopWrite(that,bitOffset)
  }

  //This is the only thing that should be implement by class that extends BusSlaveFactoryDelayed
  def build() : Unit

  component.addPrePopTask(() => build())
}

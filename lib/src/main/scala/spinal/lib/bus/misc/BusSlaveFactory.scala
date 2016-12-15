/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Lib                          **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/  MIT Licence                      **
**                                                                           **
** Permission is hereby granted, free of charge, to any person obtaining a   **
** copy of this software and associated documentation files (the "Software"),**
** to deal in the Software without restriction, including without limitation **
** the rights to use, copy, modify, merge, publish, distribute, sublicense,  **
** and/or sell copies of the Software, and to permit persons to whom the     **
** Software is furnished to do so, subject to the following conditions:      **
**                                                                           **
** The above copyright notice and this permission notice shall be included   **
** in all copies or substantial portions of the Software.                    **
**                                                                           **
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS   **
** OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF                **
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.    **
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY      **
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT **
** OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR  **
** THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                **
\*                                                                           */
package spinal.lib.bus.misc

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer


/**
  * Bus slave factory is a tool that provide an abstract and smooth way to define register bank
  */
trait BusSlaveFactory extends Area{

  /** Return the data width of the bus */
  def busDataWidth: Int

  /**
    * When the bus read the address, fill the response with that at bitOffset
    */
  def read(that: Data,
           address: BigInt,
           bitOffset: Int = 0): Unit

  /**
    * When the bus write the address, assign that with bus’s data from bitOffset
    */
  def write[T <: Data](that: T,
                       address: BigInt,
                       bitOffset: Int = 0): T

  /**
    * Call doThat when a write transaction occurs on address
    */
  def onWrite(address: BigInt)(doThat: => Unit): Unit

  /**
    * Call doThat when a read transaction occurs on address
    */
  def onRead(address: BigInt)(doThat: => Unit): Unit

  /**
    * Call doThat when a write transaction occurs and the condition is true
    */
  def onWriteCondition(condition: => Bool)(doThat: => Unit): Unit

  /**
    * Call doThat when a read transaction occurs and the condition is true
    */
  def onReadCondition(condition: => Bool)(doThat: => Unit): Unit

  /**
    * Permanently assign that by the bus write data from bitOffset
    */
  def nonStopWrite(that: Data,
                   bitOffset: Int = 0): Unit

  /**
    * Make that readable and writable at address and placed at bitOffset in the word
    */
  def readAndWrite(that: Data,
                   address: BigInt,
                   bitOffset: Int = 0): Unit = {
    write(that, address, bitOffset)
    read(that, address, bitOffset)
  }

  /** Return true if the bus is writing */
  def isWriting(address: BigInt): Bool = {
    val ret = False
    onWrite(address){ ret := True }
    ret
  }

  /** Return true if the bus is reading */
  def isReading (address: BigInt): Bool = {
    val ret = False
    onRead(address){ ret := True }
    ret
  }

  /**
    * Create the memory mapping to read that from address
    * If that  is bigger than one word it extends the register on followings addresses
    */
  def readMultiWord(that: Data,
                    address: BigInt): Unit  = {
    val wordCount = (widthOf(that) - 1) / busDataWidth + 1
    val valueBits = that.asBits
    var pos = 0
    for (wordId <- 0 until wordCount) {
      read(valueBits(pos, Math.min(widthOf(that)-pos, busDataWidth) bits), address + wordId*busDataWidth / 8)
      pos += busDataWidth
    }
  }

  /**
    * Create the memory mapping to write that at address.
    * If that  is bigger than one word it extends the register on followings addresses
    */
  def writeMultiWord(that: Data,
                     address: BigInt): Unit  = {
    val wordCount = (widthOf(that) - 1) / busDataWidth + 1
    for (wordId <- 0 until wordCount) {
      write(
        that = new DataWrapper{
          override def getBitsWidth: Int =
            Math.min(busDataWidth, widthOf(that) - wordId * busDataWidth)

          override def assignFromBits(value: Bits): Unit = {
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

  /**
    * Create a write only register of type dataType at address and placed at bitOffset in the word
    */
  def createWriteOnly[T <: Data](dataType: T,
                                 address: BigInt,
                                 bitOffset: Int = 0): T = {
    val ret = Reg(dataType)
    write(ret, address, bitOffset)
    ret
  }

  /**
    * Create a read write register of type dataType at address and placed at bitOffset in the word
    */
  def createReadWrite[T <: Data](dataType: T,
                                 address: BigInt,
                                 bitOffset: Int = 0): T = {
    val reg = Reg(dataType)
    write(reg, address, bitOffset)
    read(reg, address, bitOffset)
    reg
  }

  /**
    * Create a writable Flow register of type dataType at address and placed at bitOffset in the word
    */
  def createAndDriveFlow[T <: Data](dataType: T,
                                    address: BigInt,
                                    bitOffset: Int = 0): Flow[T] = {
    val flow = Flow(dataType)
    driveFlow(flow, address, bitOffset)
    flow
  }

  /**
    * Instanciate an internal register which at each cycle do : reg := reg | that
    * Then when a read occur, the register is cleared. This register is readable at address and placed at bitOffset in the word
    */
  def doBitsAccumulationAndClearOnRead(that: Bits,
                                       address: BigInt,
                                       bitOffset: Int = 0): Unit = {
    assert(that.getWidth <= busDataWidth)
    val reg = Reg(that)
    reg := reg | that
    read(reg, address, bitOffset)
    onRead(address){ reg := that }
  }

  /**
    * Drive that with a register writable at address placed at bitOffset in the word
    */
  def drive[T <: Data](that: T,
                       address: BigInt,
                       bitOffset: Int = 0): T = {
    val reg = Reg(that)
    write(reg, address, bitOffset)
    that := reg
    reg
  }

  /**
    * Drive that with a register writable and readable at address placed at bitOffset in the word
    */
  def driveAndRead[T <: Data](that: T,
                              address: BigInt,
                              bitOffset: Int = 0): T = {
    val reg = Reg(that)
    write(reg, address, bitOffset)
    read(reg, address, bitOffset)
    that := reg
    reg
  }

  /**
    * Emit on that a transaction when a write happen at address by using data placed at bitOffset in the word
    */
  def driveFlow[T <: Data](that: Flow[T],
                           address: BigInt,
                           bitOffset: Int = 0): Unit = {
    that.valid := False
    onWrite(address){ that.valid := True }
    nonStopWrite(that.payload, bitOffset)
  }

  /**
    * Read that and consume the transaction when a read happen at address.
    */
  def readStreamNonBlocking[T <: Data](that: Stream[T],
                                       address: BigInt,
                                       validBitOffset: Int,
                                       payloadBitOffset: Int): Unit = {
    that.ready := False
    onRead(address){
      that.ready := True
    }
    read(that.valid,   address, validBitOffset)
    read(that.payload, address, payloadBitOffset)
  }
}



/**
  * Base element
  */
trait BusSlaveFactoryElement

/**
  * Ask to make that readable when an access is done on address
  * bitOffset specify where that is placed on the answer
  */
case class BusSlaveFactoryRead(that: Data,
                               address: BigInt,
                               bitOffset: Int) extends BusSlaveFactoryElement

/**
  * Ask to make that writable when a access is done on address.
  * bitOffset specify where `that` get bits from the request
  */
case class BusSlaveFactoryWrite(that: Data,
                                address: BigInt,
                                bitOffset: Int) extends BusSlaveFactoryElement

/**
  * Ask to execute doThat when a write access is done on address
  */
case class BusSlaveFactoryOnWrite(address: BigInt,
                                  doThat: () => Unit) extends BusSlaveFactoryElement

/**
  * Ask to execute doThat when a read access is done on address
  */
case class BusSlaveFactoryOnRead(address: BigInt,
                                 doThat: () => Unit) extends BusSlaveFactoryElement

/**
  * Ask to execute doThat when a write access is done in a given range of address
  */
case class BusSlaveFactoryOnWriteCondition(condition: () => Bool,
                                           doThat: () => Unit) extends BusSlaveFactoryElement

/**
  * Ask to execute doThat when a read access is done in a given range of address
  */
case class BusSlaveFactoryOnReadCondition(condition: () => Bool,
                                          doThat: () => Unit) extends BusSlaveFactoryElement

/**
  * Ask to constantly drive that with the data bus
  * bitOffset specify where that get bits from the request
  */
case class BusSlaveFactoryNonStopWrite(that: Data,
                                       bitOffset: Int) extends BusSlaveFactoryElement




/**
  * BusSlaveFactoryDelayed captures each primitives (BusSlaveFactoryElement) into a data-model
  *
  * @example{{{
  *     class Apb3SlaveFactory(bus : Apb3) extends BusSlaveFactoryDelayed{
  *         override def build(): Unit = { ... }
  *         override def busDataWidth: Int = bus.config.dataWidth
  *      }
  * }}}
  */
trait BusSlaveFactoryDelayed extends BusSlaveFactory{

  /** Contains all elements created */
  val elements = ArrayBuffer[BusSlaveFactoryElement]()
  /** Contains all elements related to an address */
  val elementsPerAddress = collection.mutable.HashMap[BigInt, ArrayBuffer[BusSlaveFactoryElement]]()

  private def addAddressableElement(e: BusSlaveFactoryElement, address: BigInt) = {
    elements += e
    elementsPerAddress.getOrElseUpdate(address, ArrayBuffer[BusSlaveFactoryElement]()) += e
  }

  private def addElement(e: BusSlaveFactoryElement) = {
    elements += e
  }

  override def read(that: Data,
                    address: BigInt,
                    bitOffset: Int = 0): Unit  = {
    assert(bitOffset + that.getBitsWidth <= busDataWidth)
    addAddressableElement(BusSlaveFactoryRead(that, address, bitOffset), address)
  }

  override def write[T <: Data](that: T,
                                address: BigInt,
                                bitOffset: Int = 0): T = {
    assert(bitOffset + that.getBitsWidth <= busDataWidth)
    addAddressableElement(BusSlaveFactoryWrite(that, address, bitOffset),address)
    that
  }

  override def onWrite(address: BigInt)(doThat: => Unit): Unit = {
    addAddressableElement(BusSlaveFactoryOnWrite(address, () => doThat), address)
  }

  override def onRead(address: BigInt)(doThat: => Unit): Unit = {
    addAddressableElement(BusSlaveFactoryOnRead(address, () => doThat), address)
  }

  override def onWriteCondition(condition: => Bool)(doThat: => Unit) = {
    addElement(BusSlaveFactoryOnWriteCondition(() => condition, () => doThat))
  }

  override def onReadCondition(condition: => Bool)(doThat: => Unit) = {
    addElement(BusSlaveFactoryOnReadCondition(() => condition, () => doThat))
  }

  override def nonStopWrite(that: Data,
                            bitOffset: Int = 0): Unit = {
    assert(bitOffset + that.getBitsWidth <= busDataWidth)
    elements += BusSlaveFactoryNonStopWrite(that, bitOffset)
  }


  /**
    * In this function you have to define the read/write logic thanks to element, elementsPerAddress and elementsPerRangeAddress
    * This is the only thing with def busDataWidth that should be implement by class that extends BusSlaveFactoryDelay
    */
  def build(): Unit

  // add build function to prepoptask
  component.addPrePopTask(() => build())
}




class BusSlaveFactoryAddressWrapper(f: BusSlaveFactory, addressOffset: Int) extends BusSlaveFactory {
  override def busDataWidth: Int = f.busDataWidth
  override def read(that: Data, address: BigInt, bitOffset: Int): Unit = f.read(that, address + addressOffset, bitOffset)
  override def write[T <: Data](that: T, address: BigInt, bitOffset: Int): T = f.write(that, address + addressOffset, bitOffset)
  override def onWrite(address: BigInt)(doThat: => Unit): Unit = f.onWrite(address + addressOffset)(doThat)
  override def onRead(address: BigInt)(doThat: => Unit): Unit = f.onRead(address + addressOffset)(doThat)
  override def nonStopWrite(that: Data, bitOffset: Int): Unit = f.nonStopWrite(that, bitOffset)
  override def onWriteCondition(condition: => Bool)(doThat: => Unit) = f.onWriteCondition(condition)(doThat)
  override def onReadCondition(condition: => Bool)(doThat: => Unit) = f.onReadCondition(condition)(doThat)
}
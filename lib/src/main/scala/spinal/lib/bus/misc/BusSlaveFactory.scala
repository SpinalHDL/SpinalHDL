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

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}



/**
  * Configuration of the bus Slave Factory
  *
  * @param wordEndianness Endianness for the multiWrite or multiRead operations
  */
case class BusSlaveFactoryConfig(wordEndianness: Endianness = LITTLE){
}


/**
  * Bus slave factory is a tool that provide an abstract and smooth way to define register bank
  */
trait BusSlaveFactory extends Area{

  /** Configuration of the BusSlaveFactory */
  protected var _config = BusSlaveFactoryConfig()
  def getConfig : BusSlaveFactoryConfig = _config
  def setConfig(value : BusSlaveFactoryConfig) : this.type = {
    _config = value
    this
  }

  /** Return the data width of the bus */
  def busDataWidth: Int

  /** Address incrementation used by the read and write multi words registers */
  def wordAddressInc: Int = busDataWidth / 8

  /** Set the endianness during write/read multiword */
  def setWordEndianness(value : Endianness) = setConfig(getConfig.copy(wordEndianness = value))

  def withOffset(offset : Int) = new BusSlaveFactoryAddressWrapper(this, offset)


  /**
    * Return true if the configuration if set to little-endian
    */
  private def isLittleWordEndianness: Boolean = getConfig.wordEndianness match{
    case LITTLE => true
    case BIG    => false
  }

  def readPrimitive[T <: Data](that          : T,
                               address       : AddressMapping,
                               bitOffset     : Int,
                               documentation : String): Unit

  def writePrimitive[T <: Data](that          : T,
                                address       : AddressMapping,
                                bitOffset     : Int,
                                documentation : String): Unit

  def onWritePrimitive(address       : AddressMapping,
                       haltSensitive : Boolean,
                       documentation : String)(doThat: => Unit): Unit

  def onReadPrimitive(address       : AddressMapping,
                      haltSensitive : Boolean,
                      documentation : String)(doThat: => Unit): Unit

  def readHalt(): Unit
  def writeHalt(): Unit

  def readAddress(): UInt
  def writeAddress(): UInt

  /**
   * Byte enable bits, defaulting to all ones
   */
  def writeByteEnable(): Bits = B(busDataWidth / 8 bits, default -> True)

  /**
    * Permanently assign that by the bus write data from bitOffset
    */
  def nonStopWrite[T <: Data](that          : T,
                              bitOffset     : Int = 0,
                              documentation : String = null): T


  /**
    * When the bus read the address, fill the response with that at bitOffset
    */
  def read[T <: Data](that          : T,
                      address       : BigInt,
                      bitOffset     : Int = 0,
                      documentation : String = null): T = {
    readPrimitive(
      that          = that,
      address       = SingleMapping(address),
      bitOffset     = bitOffset,
      documentation = documentation
    )
    that
  }

  /**
    * When the bus write the address, assign that with bus’s data from bitOffset
    */
  def write[T <: Data](that          : T,
                       address       : BigInt,
                       bitOffset     : Int = 0,
                       documentation : String = null): T = {
    writePrimitive(
      that          = that,
      address       = SingleMapping(address),
      bitOffset     = bitOffset,
      documentation = documentation
    )
    that
  }


  /**
    * Call doThat when a write transaction occurs on address
    */
  def onWrite(address: BigInt, documentation: String = null)(doThat: => Unit): Unit = {
    onWritePrimitive(
      address       = SingleMapping(address),
      haltSensitive = true,
      documentation = documentation
    )(doThat)
  }


  /**
    * Call doThat when a read transaction occurs on address
    */
  def onRead(address: BigInt, documentation: String = null)(doThat: => Unit): Unit = {
    onReadPrimitive(
      address       = SingleMapping(address),
      haltSensitive = true,
      documentation = documentation
    )(doThat)
  }

  def write[T <: Data](address    : BigInt,
                       bitMapping : (Int, Data)*): Unit = {
    bitMapping.foreach{ case (bitId, that) => write(that, address, bitId) }
  }

  def read[T <: Data](address    : BigInt,
                       bitMapping : (Int, Data)*): Unit = {
    bitMapping.foreach{ case (bitId, that) => read(that, address, bitId) }
  }


  /**
    * Make that readable and writable at address and placed at bitOffset in the word
    */
  def readAndWrite(that          : Data,
                   address       : BigInt,
                   bitOffset     : Int = 0,
                   documentation : String = null): Unit = {
    write(that, address, bitOffset, documentation)
    read(that, address, bitOffset, documentation)
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
    * Create the memory mapping to read `that` from `address`
    * If `that` is bigger than one word it extends the register on following addresses.
    */
  def readMultiWord(that: Data, address: BigInt, documentation: String = null): Unit  = {
    // round up
    val wordCount = (widthOf(that) - 1) / busDataWidth + 1
    val valueBits = that.asBits
    var pos = if(isLittleWordEndianness) 0 else widthOf(that) - (if (widthOf(that) % busDataWidth  == 0)  busDataWidth else widthOf(that) % busDataWidth  )
    for (wordId <- 0 until wordCount) {
      // support unaligned access
      val mapping = SizeMapping(address + wordId * wordAddressInc, wordAddressInc)
      if (isLittleWordEndianness) {
        readPrimitive(valueBits(pos, Math.min(widthOf(that) - pos, busDataWidth) bits), mapping, 0, documentation)
        pos += busDataWidth
      } else {
        readPrimitive(valueBits(pos, Math.min(widthOf(that) - ((wordCount - 1) - wordId) * busDataWidth, busDataWidth) bits), mapping, 0, documentation)
        pos -= Math.min(pos, busDataWidth)
      }
    }
  }

  /**
    * Create the memory mapping to write that at address.
    * If `that` is bigger than one word it extends the register on following addresses.
    */
  def writeMultiWord(that: Data, address: BigInt, documentation: String = null): Unit = {
    // round up
    val wordCount = (widthOf(that) - 1) / busDataWidth + 1
    for (wordId <- 0 until wordCount) {
      // support unaligned access
      val mapping = SizeMapping(address + wordId * wordAddressInc, wordAddressInc)
      // split `that` into words
      writePrimitive(
        that = new DataWrapper {
          override def getBitsWidth: Int = if (isLittleWordEndianness) {
            Math.min(busDataWidth, widthOf(that) - wordId * busDataWidth)
          } else {
            Math.min(busDataWidth, widthOf(that) - ((wordCount - 1) - wordId) * busDataWidth)
          }

          override def assignFromBits(value: Bits): Unit = {
            assignFromBits(value, offset = 0, bitCount = getBitsWidth bits)
          }

          override def assignFromBits(value: Bits, offset: Int, bitCount: BitCount): Unit = {
            assert(bitCount.value <= getBitsWidth)
            that.assignFromBits(
              bits = value.resize(bitCount),
              offset = offset + (if (isLittleWordEndianness) wordId * busDataWidth else ((wordCount - 1) - wordId) * busDataWidth),
              bitCount = bitCount)
          }
        },
        mapping, 0, documentation)
    }
  }

  /**
    * Create the memory mapping to write/read that from address
    */
  def readAndWriteMultiWord(that: Data, address: BigInt, documentation: String = null): Unit = {
    writeMultiWord(that, address, documentation)
    readMultiWord(that, address, documentation)
  }


  /**
    * Create a write only register of type dataType at address and placed at bitOffset in the word
    */
  def createWriteOnly[T <: Data](dataType      : T,
                                 address       : BigInt,
                                 bitOffset     : Int = 0,
                                 documentation : String = null): T = {
    val ret = Reg(dataType)
    write(ret, address, bitOffset, documentation)
    ret
  }


  /**
    * Create a read only register of type dataType at address and placed at bitOffset in the word
    */
  def createReadOnly[T <: Data](dataType      : T,
                                address       : BigInt,
                                bitOffset     : Int = 0,
                                documentation : String = null): T = {
    val ret = Reg(dataType)
    read(ret, address, bitOffset)
    ret
  }

  /**
    * Create a read write register of type dataType at address and placed at bitOffset in the word
    */
  def createReadAndWrite[T <: Data](dataType      : T,
                                    address       : BigInt,
                                    bitOffset     : Int = 0,
                                    documentation : String = null): T = {
    val reg = Reg(dataType)
    write(reg, address, bitOffset, documentation)
    read(reg, address, bitOffset, documentation)
    reg
  }



  def createReadAndClearOnSet[T <: Data](dataType  : T,
                                         address   : BigInt,
                                         bitOffset : Int = 0):T= {
    readAndClearOnSet(Reg(dataType), address, bitOffset)
  }

  def readAndClearOnSet[T <: Data](that      : T,
                                   address   : BigInt,
                                   bitOffset : Int = 0): T = {
    clearOnSet(that, address, bitOffset)
    read(that, address, bitOffset)
    that
  }


  def clearOnSet[T <: Data](that      : T,
                            address   : BigInt,
                            bitOffset : Int = 0): T = {
    val bitClears = nonStopWrite(Bits(widthOf(that) bits), bitOffset)
    when(isWriting(address)){
      for(i <- 0 until widthOf(that)){
        when(bitClears(i)){
          that.assignFromBits(B"0", i, 1 bits)
        }
      }
    }
    that
  }


  def createReadAndSetOnSet[T <: Data](dataType  : T,
                                       address   : BigInt,
                                       bitOffset : Int = 0):T= {
    readAndSetOnSet(Reg(dataType), address, bitOffset)
  }


  def readAndSetOnSet[T <: Data](that      : T,
                                 address   : BigInt,
                                 bitOffset : Int = 0): T = {
    setOnSet(that, address, bitOffset)
    read(that, address, bitOffset)
    that
  }


  def setOnSet[T <: Data](that      : T,
                          address   : BigInt,
                          bitOffset : Int = 0): T = {
    val bitSets = nonStopWrite(Bits(widthOf(that) bits), bitOffset)
    when(isWriting(address)){
      for(i <- 0 until widthOf(that)){
        when(bitSets(i)){
          that.assignFromBits(B"1", i, 1 bits)
        }
      }
    }
    that
  }

  @deprecated("Use createReadAndWrite instead", "???")
  def createReadWrite[T <: Data](dataType  : T,
                                 address   : BigInt,
                                 bitOffset : Int = 0): T = createReadAndWrite(dataType,address,bitOffset)

  /**
    * Create a writable Flow register of type dataType at address and placed at bitOffset in the word
    */
  def createAndDriveFlow[T <: Data](dataType  : T,
                                    address   : BigInt,
                                    bitOffset : Int = 0): Flow[T] = {
    val flow = Flow(dataType)
    driveFlow(flow, address, bitOffset)
    flow
  }

  /**
    * Create multi-words write register of type dataType
    */
  def createWriteMultiWord[T <: Data](that: T, address: BigInt, documentation: String = null): T = {
    val reg = Reg(that)
    writeMultiWord(reg, address, documentation)
    reg
  }

  /**
    * Create multi-words read register of type dataType
    */
  def createReadMultiWord[T <: Data](that: T, address: BigInt, documentation: String = null): T = {
    val reg = Reg(that)
    readMultiWord(reg, address, documentation)
    reg
  }

  /**
    * Create multi-words write and read register of type dataType
    */
  def createWriteAndReadMultiWord[T <: Data](that: T, address: BigInt, documentation: String = null): T = {
    val reg = Reg(that)
    writeMultiWord(reg, address, documentation)
    readMultiWord(reg, address, documentation)
    reg
  }



  /**
    * Drive that with a register writable at address placed at bitOffset in the word
    */
  def drive[T <: Data](that          : T,
                       address       : BigInt,
                       bitOffset     : Int = 0,
                       documentation : String = null): T = {
    val reg = Reg(that)
    write(reg, address, bitOffset, documentation)
    that := reg
    reg
  }

  def drive[T <: Data](address    : BigInt,
                       bitMapping : (Int, Data)*): Unit = {
    bitMapping.foreach{ case (bitId, that) => drive(that, address, bitId) }
  }

  /**
    * Drive that with a register writable and readable at address placed at bitOffset in the word
    */
  def driveAndRead[T <: Data](that          : T,
                              address       : BigInt,
                              bitOffset     : Int = 0,
                              documentation : String = null): T = {
    val reg = Reg(that).setCompositeName(that, "driver", true)
    write(reg, address, bitOffset, documentation)
    read(reg, address, bitOffset,  documentation)
    that := reg
    reg
  }


  /**
    * Drive that on multi-words
    */
  def driveMultiWord[T <: Data](that: T, address: BigInt, documentation: String = null): T = {
    val reg = Reg(that)
    writeMultiWord(reg, address, documentation)
    that := reg
    reg
  }


  /**
    * Drive and read that on multi-word
    */
  def driveAndReadMultiWord[T <: Data](that: T, address: BigInt, documentation: String = null): T = {
    val reg = Reg(that)
    writeMultiWord(reg, address, documentation)
    readMultiWord(reg, address, documentation)
    that := reg
    reg
  }

  /**
    * Emit on that a transaction when a write happen at address by using data placed at bitOffset in the word
    */
  def driveFlow[T <: Data](that      : Flow[T],
                           address   : BigInt,
                           bitOffset : Int = 0): Unit = {

    val wordCount = (bitOffset + widthOf(that.payload) - 1 ) / busDataWidth + 1

    if (wordCount == 1){
      that.valid := False
      onWrite(address){ that.valid := True }
      nonStopWrite(that.payload, bitOffset)
    }else{

      assert(bitOffset == 0, "BusSlaveFactory ERROR [driveFlow] : BitOffset must be equal to 0 if the payload of the Flow is bigger than the data bus width")

      val regValid = RegNext(False) init(False)
      onWrite(address + ((wordCount - 1) * wordAddressInc)){ regValid := True }
      driveMultiWord(that.payload, address)
      that.valid := regValid
    }
  }

  /**
    * Read that (that is bigger than the busWidth) and consume the transaction when a read happen at address.
    * @note in order to avoid to read wrong data read first the address which contains the
    *       valid signal.
    *       Little : payload - valid at address 0x00
    *       Big    : valid - payload at address 0x00
    *       Once the valid signal is true you can read all registers
    */
  def readStreamNonBlocking[T <: Data](that: Stream[T], address: BigInt): Unit = {

    val wordCount = (1 + widthOf(that.payload) - 1 ) / busDataWidth + 1
    val addressHigh = address + (wordCount - 1) * wordAddressInc

    if (wordCount == 1) {
      // we set that.ready irrespective of that.valid.
      that.ready := isReading(addressHigh)
    } else {
      // we set that.ready to the value of that.valid that it had when reading the base address
      val payloadIsValid = RegInit(False)
      onRead(address) { payloadIsValid := that.valid }

      that.ready := False
      onRead(addressHigh) {
        that.ready := payloadIsValid
        payloadIsValid := False
      }
    }

    if(isLittleWordEndianness){
      readMultiWord(that.payload ## that.valid, address)
    }else{
      readMultiWord(that.valid ## that.payload, address)
    }
  }


  /**
    * Read that and consume the transaction when a read happen at address.
    */
  def readStreamNonBlocking[T <: Data](that             : Stream[T],
                                       address          : BigInt,
                                       validBitOffset   : Int,
                                       payloadBitOffset : Int,
                                       validInverted    : Boolean = false): Unit = {

    assert(widthOf(that.payload) + 1 <= busDataWidth, "BusSlaveFactory ERROR [readStreamNonBlocking] : width of that parameter payload + valid signal is bigger than the data bus width. To solve it use readStreamNonBlocking(that: Stream, address: BigInt)")
    assert(payloadBitOffset + widthOf(that.payload) <= busDataWidth, "BusSlaveFactory ERROR [readStreamNonBlocking] : payloadBitOffset + width of that parameter payload is bigger than the data bus width" )
    assert(validBitOffset <= busDataWidth - 1, "BusSlaveFactory ERROR [readStreamNonBlocking] : validBitOffset is outside the data bus width")

    that.ready := False
    onRead(address){
      that.ready := True
    }
    read(that.valid ^ Bool(validInverted),   address, validBitOffset)
    read(that.payload, address, payloadBitOffset)
  }


  /**
    * Instantiate an internal register which at each cycle do : reg := reg | that
    * Then when a read occur, the register is cleared. This register is readable at address and placed at bitOffset in the word
    */
  def doBitsAccumulationAndClearOnRead(that      : Bits,
                                       address   : BigInt,
                                       bitOffset : Int = 0): Unit = {

    assert(bitOffset + that.getWidth <= busDataWidth, "BusSlaveFactory ERROR [doBitsAccumulationAndClearOnRead] : the width of the parameter that is bigger than the data bus width")

    val reg = Reg(that)
    reg := reg | that
    read(reg, address, bitOffset)
    onRead(address){ reg := that }
  }

  def multiCycleRead(address: AddressMapping, cycles: BigInt): Unit = {
    val counter = Counter(cycles)
    onReadPrimitive(
      address       = address,
      haltSensitive = false,
      documentation = null
    ){
      counter.increment()
      when(!counter.willOverflowIfInc){
        readHalt()
      }
    }
  }

  def readAddress(address: AddressMapping): UInt = address.removeOffset(readAddress())
  def writeAddress(address: AddressMapping): UInt = address.removeOffset(writeAddress())

  def readSyncMemWordAligned[T <: Data](mem           : Mem[T],
                                        addressOffset : BigInt,
                                        bitOffset     : Int = 0) : Mem[T] = {
    val mapping = SizeMapping(addressOffset,mem.wordCount << log2Up(busDataWidth/8))
    val memAddress = readAddress(mapping) >> log2Up(busDataWidth/8)
    val readData = mem.readSync(memAddress)
    multiCycleRead(mapping,2)
    readPrimitive(readData, mapping, bitOffset, null)
    mem
  }

  /**
    * Memory map a Mem to bus for reading. Elements can be larger than bus data width in bits.
    */
  def readSyncMemMultiWord[T <: Data](mem: Mem[T],
                                      addressOffset: BigInt): Mem[T] = {
    val mapping = SizeMapping(addressOffset, mem.wordCount << log2Up(mem.width / 8))
    val memAddress = readAddress(mapping) >> log2Up(mem.width / 8)
    val readData = mem.readSync(memAddress).asBits
    val offset = readAddress(mapping)(log2Up(mem.width / 8) - 1 downto log2Up(busDataWidth / 8))
    val partialRead = readData(offset << log2Up(busDataWidth), busDataWidth bits)
    multiCycleRead(mapping, 2)
    readPrimitive(partialRead, mapping, 0, null)
    mem
  }

  def writeMemWordAligned[T <: Data](mem           : Mem[T],
                                     addressOffset : BigInt,
                                     bitOffset     : Int = 0) : Mem[T] = {
    val mapping    = SizeMapping(addressOffset,mem.wordCount << log2Up(busDataWidth / 8))
    val memAddress = writeAddress(mapping) >> log2Up(busDataWidth / 8)
    val port       = mem.writePort

    port.address := memAddress
    port.valid := False
    onWritePrimitive(mapping,true, null){
      port.valid := True
    }
    nonStopWrite(port.data, bitOffset)
    mem
  }

  /**
    * Memory map a Mem to bus for writing. Elements can be larger than bus data width in bits.
    */
  def writeMemMultiWord[T <: Data](mem: Mem[T],
                                   addressOffset: BigInt): Mem[T] = {
    // sanity check
    if (mem.width % busDataWidth != 0) {
      PendingError(s"Memory width ${mem.width} must be multiple of bus data width ${busDataWidth} \n${getScalaLocationLong}")
    }

    val mapping = SizeMapping(addressOffset, mem.wordCount << log2Up(mem.width / 8))
    val memAddress = writeAddress(mapping) >> log2Up(mem.width / 8)
    val port = mem.writePortWithMask
    val data = Bits(busDataWidth bits)

    port.address := memAddress
    port.valid := False
    onWritePrimitive(mapping, true, null) {
      port.valid := True
    }
    // replicate data to `mem.width` bits
    port.data.assignFromBits(Cat(Seq.fill(mem.width / busDataWidth)(data): _*))

    // generate mask
    val maskWidth = mem.width / busDataWidth
    val mask = UInt(maskWidth bits)
    mask := 0
    mask(writeAddress(mapping)(log2Up(mem.width / 8) - 1 downto log2Up(busDataWidth / 8))) := True
    port.mask := mask.asBits

    nonStopWrite(data)
    mem
  }
}


/**
  * Base element
  */
trait BusSlaveFactoryElement{
  def mapping: AddressMapping
}


/**
  * Ask to make that readable when an access is done on address
  * bitOffset specify where that is placed on the answer
  */
case class BusSlaveFactoryRead(that          : Data,
                               address       : AddressMapping,
                               bitOffset     : Int,
                               documentation : String) extends BusSlaveFactoryElement{
  def mapping: AddressMapping = address
}


/**
  * Ask to make that writable when a access is done on address.
  * bitOffset specify where `that` get bits from the request
  */
case class BusSlaveFactoryWrite(that          : Data,
                                address       : AddressMapping,
                                bitOffset     : Int,
                                documentation : String) extends BusSlaveFactoryElement{
  def mapping: AddressMapping = address
}


/** Ask to execute doThat when a write access is done on address */
case class BusSlaveFactoryOnWriteAtAddress(address       : AddressMapping,
                                           haltSensitive : Boolean,
                                           documentation : String,
                                           doThat        : () => Unit) extends BusSlaveFactoryElement{
  def mapping: AddressMapping = address
}


/**  Ask to execute doThat when a read access is done on address */
case class BusSlaveFactoryOnReadAtAddress(address       : AddressMapping,
                                          haltSensitive : Boolean,
                                          documentation : String,
                                          doThat        : () => Unit) extends BusSlaveFactoryElement{
  def mapping: AddressMapping = address
}



/**
  * Ask to constantly drive that with the data bus
  * bitOffset specify where that get bits from the request
  */
case class BusSlaveFactoryNonStopWrite(that          : Data,
                                       bitOffset     : Int,
                                       documentation : String) extends BusSlaveFactoryElement{
  def mapping: AddressMapping = null
}



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
trait BusSlaveFactoryDelayed extends BusSlaveFactory {

  /** Contains all elements created */
  val elements = ArrayBuffer[BusSlaveFactoryElement]()
  /** Contains all elements related to an address */
  val elementsPerAddress = mutable.LinkedHashMap[AddressMapping, ArrayBuffer[BusSlaveFactoryElement]]()
  val elementsOk = mutable.HashSet[BusSlaveFactoryElement]()


  component.addPrePopTask(() => {

    // prohibit reading two signals on the same address / bit
    for ((address, jobs) <- elementsPerAddress) {
      val occupied_range = new ListBuffer[Int]
      for (job <- jobs if job.isInstanceOf[BusSlaveFactoryRead]) {
        val read_job = job.asInstanceOf[BusSlaveFactoryRead]
        val current_bits = List.range(read_job.bitOffset, read_job.that.getBitsWidth + read_job.bitOffset, 1)
        for (bit <- current_bits) {
          assert(!occupied_range.contains(bit), s"BusSlaveFactory DOUBLE-READ-ERROR : bit $bit of bus address ${address.asInstanceOf[SingleMapping].address} should be written by ${read_job.that.getName()} but it is already occupied by another signal at the same address!")
          occupied_range.append(bit)
        }
      }
    }

    build()

    if (elementsOk.size != elements.size) {
      PendingError(s"$this isn't able generate the following requests :\n${(elements --= elementsOk).mkString("\n")} at \n${this.getScalaLocationLong}")
    }
  })

  private def addElement(e: BusSlaveFactoryElement) = {
    elements += e
    if (e.mapping != null)
      elementsPerAddress.getOrElseUpdate(e.mapping, ArrayBuffer[BusSlaveFactoryElement]()) += e
  }

  override def readPrimitive[T <: Data](that          : T,
                                        address       : AddressMapping,
                                        bitOffset     : Int,
                                        documentation : String): Unit = {

    assert(bitOffset + that.getBitsWidth <= busDataWidth, "BusSlaveFactory ERROR [readPrimitive] : bitOffset + width of the parameter that is bigger than the data bus width")

    addElement(BusSlaveFactoryRead(
      that          = that,
      address       = address,
      bitOffset     = bitOffset,
      documentation = documentation
    ))
  }

  override def writePrimitive[T <: Data](that          : T,
                                         address       : AddressMapping,
                                         bitOffset     : Int,
                                         documentation : String): Unit = {

    assert(bitOffset + that.getBitsWidth <= busDataWidth, "BusSlaveFactory ERROR [writePrimitive] : bitOffset + width of the parameter that is bigger than the data bus width")

    addElement(BusSlaveFactoryWrite(
      that          = that,
      address       = address,
      bitOffset     = bitOffset,
      documentation = documentation
    ))
  }

  override def onWritePrimitive(address       : AddressMapping,
                                haltSensitive : Boolean,
                                documentation : String)(doThat: => Unit): Unit = {
    addElement(BusSlaveFactoryOnWriteAtAddress(
      address       = address,
      haltSensitive = haltSensitive,
      documentation = documentation,
      doThat        = () => doThat
    ))
  }


  def onReadPrimitive(address       : AddressMapping,
                      haltSensitive : Boolean,
                      documentation : String)(doThat: => Unit): Unit = {
    addElement(BusSlaveFactoryOnReadAtAddress(
      address       = address,
      haltSensitive = haltSensitive,
      documentation = documentation,
      doThat        = () => doThat
    ))
  }


  /**
    * Permanently assign that by the bus write data from bitOffset
    */
  def nonStopWrite[T <: Data](that          : T,
                              bitOffset     : Int = 0,
                              documentation : String = null): T = {
    addElement(BusSlaveFactoryNonStopWrite(
      that          = that,
      bitOffset     = bitOffset,
      documentation = documentation
    ))
    that
  }


  /**
    * In this function you have to define the read/write logic thanks to element, elementsPerAddress and elementsPerRangeAddress
    * This is the only thing with def busDataWidth that should be implement by class that extends BusSlaveFactoryDelay
    */
  def build(): Unit


  def dataModelString(): String = {
    val builder = new StringBuilder()
    for ((address, tasks) <- elementsPerAddress.toList.sortBy(_._1.lowerBound)) {
      builder ++= s"$address :\n"
      for (task <- tasks) task match {
        case task: BusSlaveFactoryRead  => builder ++= s"  R[${task.bitOffset + widthOf(task.that) - 1}:${task.bitOffset}] ${task.that.getName()} ${if(task.documentation != null) s"- ${task.documentation}" else ""} \n"
        case task: BusSlaveFactoryWrite => builder ++= s"  W[${task.bitOffset + widthOf(task.that) - 1}:${task.bitOffset}] ${task.that.getName()} ${if(task.documentation != null) s"- ${task.documentation}" else ""} \n"
        case _ =>
      }
    }
    builder.toString
  }

  def printDataModel(): Unit = print(dataModelString())


  def doNonStopWrite(writeData: Bits): Unit = {
    for (element <- elements) element match {
      case element: BusSlaveFactoryNonStopWrite =>
        element.that.assignFromBits(writeData(element.bitOffset, element.that.getBitsWidth bits))
        elementsOk += element
      case _ =>
    }
  }

  def doMappedReadElements(jobs: Seq[BusSlaveFactoryElement], askRead: Bool, doRead: Bool, readData: Bits): Unit = {
    when(askRead) {
      for (element <- jobs) element match {
        case element: BusSlaveFactoryOnReadAtAddress if !element.haltSensitive =>
          element.doThat()
          elementsOk += element
        case _ =>
      }
    }

    when(doRead) {
      for (element <- jobs) element match {
        case element: BusSlaveFactoryOnReadAtAddress if element.haltSensitive =>
          element.doThat()
          elementsOk += element
        case _ =>
      }
    }

    for (element <- jobs) element match {
      case element: BusSlaveFactoryRead =>
        readData(element.bitOffset, element.that.getBitsWidth bits) := element.that.asBits
        elementsOk += element
      case _ =>
    }
  }

  def doMappedWriteElements(jobs: Seq[BusSlaveFactoryElement], askWrite: Bool, doWrite: Bool, writeData: Bits): Unit = {
    when(askWrite) {
      for (element <- jobs) element match {
        case element: BusSlaveFactoryOnWriteAtAddress if !element.haltSensitive =>
          element.doThat()
          elementsOk += element
        case _ =>
      }
    }

    val byteEnable = writeByteEnable()
    when(doWrite) {
      for (element <- jobs) element match {
        case element: BusSlaveFactoryWrite =>
          // check byte enable
          for (i <- (0 until busDataWidth / 8)) {
            val from = element.bitOffset max i * 8
            val to = (element.bitOffset + element.that.getBitsWidth) min (i + 1) * 8
            if (from < to) {
              when(byteEnable(i)) {
                element.that.assignFromBits(writeData(from until to), from - element.bitOffset, to - from bits)
              }
            }
          }

          elementsOk += element
        case element: BusSlaveFactoryOnWriteAtAddress if element.haltSensitive =>
          element.doThat()
          elementsOk += element
        case _ =>
      }
    }
  }

  def doMappedElements(jobs: Seq[BusSlaveFactoryElement], askWrite: Bool, askRead: Bool, doWrite: Bool, doRead: Bool, writeData: Bits, readData: Bits): Unit = {
    doMappedWriteElements(jobs, askWrite, doWrite, writeData)
    doMappedReadElements(jobs, askRead, doRead, readData)
  }
}



class BusSlaveFactoryAddressWrapper(f: BusSlaveFactory, addressOffset: BigInt) extends BusSlaveFactory {
  override def busDataWidth: Int = f.busDataWidth
  override def nonStopWrite[T <: Data](that: T, bitOffset: Int, documentation: String): T = f.nonStopWrite(that, bitOffset, documentation)
  override def wordAddressInc: Int = f.wordAddressInc
  override def getConfig = f.getConfig
  override def setConfig(value : BusSlaveFactoryConfig): this.type = {
    f.setConfig(value)
    this
  }

  override def readPrimitive[T <: Data](that: T, address: AddressMapping, bitOffset: Int, documentation: String): Unit = f.readPrimitive(that, address.applyOffset(addressOffset), bitOffset, documentation)
  override def writePrimitive[T <: Data](that: T, address: AddressMapping, bitOffset: Int, documentation: String): Unit = f.writePrimitive(that, address.applyOffset(addressOffset), bitOffset, documentation)
  override def onWritePrimitive(address: AddressMapping, haltSensitive: Boolean, documentation: String)(doThat: => Unit): Unit = f.onWritePrimitive(address.applyOffset(addressOffset), haltSensitive, documentation)(doThat)
  override def onReadPrimitive(address: AddressMapping, haltSensitive: Boolean, documentation: String)(doThat: => Unit): Unit = f.onReadPrimitive(address.applyOffset(addressOffset), haltSensitive, documentation)(doThat)
  override def readHalt(): Unit = f.readHalt()
  override def writeHalt(): Unit = f.writeHalt()
  override def readAddress(): UInt = f.readAddress()  - addressOffset
  override def writeAddress(): UInt = f.writeAddress() - addressOffset
}

package spinal.lib.bus.amba4.axi.sim

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

import scala.collection.mutable
import scala.collection.concurrent.TrieMap
import java.nio.file.Paths
import java.nio.file.Files
import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import javax.imageio.ImageIO
import java.io.File
import java.awt.image.Raster
import java.awt.image.DataBufferByte
import scala.util.Random

class MemoryPage(size : Int) {
    val data = new Array[Byte](size)

    def clear(value : Byte) : Unit = {
      data.transform(x => value)
    }

    def read(offset : Int) : Byte = {
        this.data(offset)
    }

    def write(offset : Int, data : Byte) : Unit = {
        this.data(offset) = data
    }
    
    /** Reads an array from this page.
     * 
     * @param offset Offset into page
     * @return Byte array containing the read bytes. Reads may be limited by the page end.
     */
    def readArray(offset : Int, len : Int) : Array[Byte] = {
        var length = scala.math.min(len, size - offset)
        var data = new Array[Byte](length)

        for(i <- 0 until length) {
          data(i) = this.data(offset + i)
        }

        data
    }

    /** Writes an array to this page.
     * 
     * @param offset Offset into page.
     * @param data The byte array.
     * @return Number of bytes written. Writes may be limited by the page end.
     */
    def writeArray(offset : Int, data : Array[Byte]) : Int = {
        var length = scala.math.min(data.length, size - offset)

        for(i <- 0 until length) {
          this.data(offset + i) = data(i)
        }

        length
    }
}

case class SparseMemory() {
  val memory = Array.fill[MemoryPage](4096)(null)

  def allocPage() : MemoryPage = {
    val page = new MemoryPage(1024*1024)
    page.clear(0xcd.toByte)
    page
  }

  def invalidPage() : MemoryPage = {
    val page = new MemoryPage(1024*1024)
    page.clear(0xef.toByte)
    page
  }

  def getElseAllocPage(index : Int) : MemoryPage = {
    if(memory(index) == null) {
      println(s"Adding page ${index} at 0x${(index << 20).toHexString}")
      memory(index) = allocPage()
    }
    memory(index)
  }

  def getElseInvalidPage(index : Int) : MemoryPage = {
    if(memory(index) == null) {
      println(s"Page fault while reading page ${index} (0x${(index << 20).toHexString})")
      invalidPage()
    }
    else
      memory(index)
  }

  def getPageIndex(address : Long) : Int = {
    (address >> 20).toInt
  }

  def getOffset(address : Long) : Int = {
    val mask = (1 << 20) - 1
    (address & mask).toInt
  }

  def read(address : Long) : Byte = {
    getElseInvalidPage(getPageIndex(address)).read(getOffset(address))
  }

  def write(address : Long, data : Byte) : Unit = {
    getElseAllocPage(getPageIndex(address)).write(getOffset(address), data)
  }

  def readArray(address : Long, len : Long) : Array[Byte] = {
    val startPageIndex = getPageIndex(address)
    val endPageIndex = getPageIndex(address + len - 1)
    var offset = getOffset(address)
    val buffer = new mutable.ArrayBuffer[Byte](0)
    
    for(i <- startPageIndex to endPageIndex) {
      val page = getElseInvalidPage(i)
      val readArray = page.readArray(offset, len.toInt - buffer.length)
      buffer.appendAll(readArray)
      offset = 0
    }

    buffer.toArray
  }

  def writeArray(address : Long, data : Array[Byte]) : Unit = {
    val startPageIndex = getPageIndex(address)
    val endPageIndex = getPageIndex(address + data.length - 1)
    var offset = getOffset(address)
    
    for(i <- startPageIndex to endPageIndex) {
      val page = getElseAllocPage(i)
      val bytesWritten = page.writeArray(offset, data)
      data.drop(bytesWritten)
      offset = 0
    }
  }

  /** Reads a BigInt value from the given address.
   * 
   * @param address Read address.
   * @param width Length of the byte array to be read in bytes.
   * @return BigInt read from the given address.
   */
  def readBigInt(address : Long, length : Int) : BigInt = {
    val dataArray = readArray(address, length)
    val buffer = dataArray.reverse.toBuffer // revert for Little Endian representation

    // We never want negative numbers
    buffer.prepend(0.toByte)

    BigInt(buffer.toArray)
  }

  /** Writes a BigInt value to the given address.
   * The BigInt will be resized to a byte Array of given width.
   * The data will be trimmed if it is bigger than the given width.
   * If it is smaller, the unused bytes will be filled with '0x00'.
   * 
   * @param address Write address.
   * @param data Data to be written.
   * @param width Width of the byte Array the data is resized to (if necessary).
   */
  def writeBigInt(address : Long, data : BigInt, width : Int) {
    var dataArray = data.toByteArray.reverse
    var length = scala.math.min(width, dataArray.length)
    var result = Array.fill[Byte](width)(0.toByte)

    for(i <- 0 until length)
      result(i) = dataArray(i)
    
    writeArray(address, result)
  }

  def loadBinary(address : Long, file : String) : Unit = {
    val byteArray = Files.readAllBytes(Paths.get(file))
    writeArray(address, byteArray)

    println(s"Loading 0x${byteArray.length.toHexString} bytes from ${file} to 0x${address.toHexString}")
  }

  def loadDebugSequence(address : Long, length : Int, width : Int) : Unit = {
    for(i <- 0 until length) {
      writeBigInt(address + i * width, BigInt(address + i * width), width)
    }
  }

  //def saveImage(address : Long, len : Long, file : String) : Unit = {
  //  val byteArray = readArray(address, len)
  //  val img = new BufferedImage(480, 640, BufferedImage.TYPE_INT_RGB)
  //  img.setData(Raster.createRaster(img.getSampleModel(), new DataBufferByte(byteArray, byteArray.length), null))
  //  ImageIO.write(img, "png", new File(file));
  //}

  def saveBinary(address : Long, len : Long, file : String) : Unit = {
    val byteArray = readArray(address, len)
    Files.write(Paths.get(file), byteArray)

    println(s"Saving 0x${len.toHexString} bytes from 0x${address.toHexString} to ${file}")
  }
}

case class AxiJob (
  address     : Long,
  burstLength : Int
) {
  // check for read/write over 4k boundary
}

/**
  * Configuration class for the AxiMemorySim.
  *
  * @param maxOutstandingReads
  * @param maxOutstandingWrites
  * @param useAlteraBehavior    Couple write command and write channel as in the Altera Cyclone 5 F2H_SDRAM port.
  */
case class AxiMemorySimConfig (
  maxOutstandingReads  : Int = 8,
  maxOutstandingWrites : Int = 8,
  readResponseDelay    : Int = 0,
  writeResponseDelay   : Int = 0,
  interruptProbability : Int = 0,
  interruptMaxDelay    : Int = 0,
  useAlteraBehavior    : Boolean = false
) {

}

case class AxiMemorySim(axi : Axi4, clockDomain : ClockDomain, config : AxiMemorySimConfig) {
  val memory = SparseMemory()
  val pending_reads = new mutable.Queue[AxiJob]
  val pending_writes = new mutable.Queue[AxiJob]

  /** Bus word width in bytes */
  val busWordWidth = axi.config.dataWidth / 8

  def newAxiJob(address : Long, burstLength : Int) : AxiJob = {
    AxiJob(address, burstLength)
  }

  def start() : Unit = {
    fork {
      handleAr(axi.ar)
    }

    fork {
      handleR(axi.r)
    }

    if(config.useAlteraBehavior) {
      fork {
        handleAwAndW(axi.w, axi.aw, axi.b)
      }
    }
    else {
      fork {
        handleAw(axi.aw)
      }

      fork {
        handleW(axi.w, axi.b)
      }
    }
  }

  def handleAr(ar : Stream[Axi4Ar]) : Unit = {
    println("Handling AXI4 Master read cmds...")

    ar.ready #= false

    while(true) {
      ar.ready #= true
      clockDomain.waitSamplingWhere(ar.valid.toBoolean)
      ar.ready #= false

      assert(
        assertion = (ar.payload.len.toBigInt + (ar.payload.addr.toBigInt & 4095)) <= 4095,
        message   = s"Read request crossing 4k boundary (addr=${ar.payload.addr.toBigInt.toString(16)}, len=${ar.payload.len.toLong.toHexString}"
      )
      
      pending_reads += newAxiJob(ar.payload.addr.toLong, ar.payload.len.toInt)

      //println("AXI4 read cmd: addr=0x" + ar.payload.addr.toLong.toHexString + " count=" + (ar.payload.len.toBigInt+1))

      if(pending_reads.length >= config.maxOutstandingReads)
        clockDomain.waitSamplingWhere(pending_reads.length < config.maxOutstandingReads)
    }
  }

  def handleR(r : Stream[Axi4R]) : Unit = {
    println("Handling AXI4 Master read resp...")

    val random = Random

    r.valid #= false
    r.payload.last #= false

    while(true) {
      clockDomain.waitSampling(1)

      // todo: implement read issuing delay

      if(pending_reads.nonEmpty) {
        var job = pending_reads.front

        r.valid #= true

        var i = 0
        while(i <= job.burstLength) {
          if(config.interruptProbability > random.nextInt(100)) {
            r.valid #= false
            clockDomain.waitSampling(random.nextInt(config.interruptMaxDelay + 1))
            r.valid #= true
          }
          else {
            if(i == job.burstLength)
              r.payload.last #= true
            r.payload.data #= memory.readBigInt(job.address + i * busWordWidth, busWordWidth)
            clockDomain.waitSamplingWhere(r.ready.toBoolean)
            i = i + 1
          }
        }

        r.valid #= false
        r.payload.last #= false

        pending_reads.dequeue()

        //println("AXI4 read rsp: addr=0x" + job.address.toLong.toHexString + " count=" + (job.burstLength+1))
      }
    }
  }

  def handleAw(aw : Stream[Axi4Aw]) : Unit = {
    println("Handling AXI4 Master write cmds...")

    aw.ready #= false

    while(true) {
      aw.ready #= true
      clockDomain.waitSamplingWhere(aw.valid.toBoolean)
      aw.ready #= false

      assert(
        assertion = (aw.payload.len.toBigInt + (aw.payload.addr.toBigInt & 4095)) <= 4095,
        message   = s"Write request crossing 4k boundary (addr=${aw.payload.addr.toBigInt.toString(16)}, len=${aw.payload.len.toLong.toHexString}"
      )

      pending_writes += newAxiJob(aw.payload.addr.toLong, aw.payload.len.toInt)

      //println("AXI4 write cmd: addr=0x" + aw.payload.addr.toLong.toHexString + " count=" + (aw.payload.len.toBigInt+1))

      if(pending_writes.length >= config.maxOutstandingWrites)
        clockDomain.waitSamplingWhere(pending_writes.length < config.maxOutstandingWrites)
    }
  }

  def handleW(w : Stream[Axi4W], b : Stream[Axi4B]) : Unit = {
    println("Handling AXI4 Master write...")

    w.ready #= false
    b.valid #= false

    while(true) {
      clockDomain.waitSampling(10)

      if(pending_writes.nonEmpty) {
        var job = pending_writes.front
        var count = job.burstLength
       
        w.ready #= true

        for(i <- 0 to job.burstLength) {
          clockDomain.waitSamplingWhere(w.valid.toBoolean)
          memory.writeBigInt(job.address + i * busWordWidth, w.payload.data.toBigInt, busWordWidth)
        }

        w.ready #= false

        clockDomain.waitSampling(config.writeResponseDelay)

        b.valid #= true
        b.payload.resp #= 0
        clockDomain.waitSamplingWhere(b.ready.toBoolean)
        b.valid #= false

        pending_writes.dequeue()

        //println("AXI4 write: addr=0x" + job.address.toLong.toHexString + " count=" + (job.burstLength+1))
      }
    }
  }

  /**
    * Handle write command, write, and write response channel as implemented
    * by Altera/Intel on their Cyclone 5 platform.
    * Their implementation behaves as all three channels are coupled. The
    * implementation waits until all words for a write operation have been
    * transfered. Then it asserts the AWREADY to accept the write command.
    * After that, BVALID is asserted.
    *
    * @param w  AXI write channel
    * @param aw AXI write command channel
    * @param b  AXI write response channel
    */
  def handleAwAndW(w : Stream[Axi4W], aw : Stream[Axi4Aw], b : Stream[Axi4B]) : Unit = {
    println("Handling AXI4 Master write cmds and write (Altera/Intel behavior)...")

    val random = Random

    aw.ready #= false
    w.ready  #= false
    b.valid  #= false

    while(true) {
      clockDomain.waitSamplingWhere(aw.valid.toBoolean && w.valid.toBoolean)
      w.ready #= true

      assert(
        assertion = (aw.payload.len.toBigInt + (aw.payload.addr.toBigInt & 4095)) <= 4095,
        message   = s"Write request crossing 4k boundary (addr=${aw.payload.addr.toBigInt.toString(16)}, len=${aw.payload.len.toLong.toHexString}"
      )

      var i = 0;
      while(i <= aw.payload.len.toInt) {
        if(config.interruptProbability > random.nextInt(100)) {
          w.ready #= false
          clockDomain.waitSampling(random.nextInt(config.interruptMaxDelay + 1))
          w.ready #= true
        }
        else {
          clockDomain.waitSamplingWhere(w.valid.toBoolean)
          memory.writeBigInt(aw.payload.addr.toLong + i * busWordWidth, w.payload.data.toBigInt, busWordWidth)
          i = i + 1
        }
      }

      aw.ready #= true

      clockDomain.waitSampling(1)

      aw.ready #= false
      w.ready #= false

      // Handle write response
      clockDomain.waitSampling(config.writeResponseDelay)

      b.valid #= true
      b.payload.resp #= 0
      clockDomain.waitSamplingWhere(b.ready.toBoolean)
      b.valid #= false

      //println("AXI4 write cmd: addr=0x" + aw.payload.addr.toLong.toHexString + " count=" + (aw.payload.len.toBigInt+1))
    }
  }
}

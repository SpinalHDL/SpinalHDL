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
    
    List.tabulate(endPageIndex - startPageIndex + 1)(_ + startPageIndex).foldLeft(data){
      (writeData,pageIndex) => {
          val page = getElseAllocPage(pageIndex)
          val bytesWritten = page.writeArray(offset, writeData)
          offset = 0
          writeData.drop(bytesWritten)
      }
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
  def writeBigInt(address : Long, data : BigInt, width : Int, strb : BigInt=null) {
    var dataArray = data.toByteArray.reverse
    var length = scala.math.min(width, dataArray.length)
    var result = Array.fill[Byte](width)(0.toByte)

    for(i <- 0 until length)
      result(i) = dataArray(i)

    if(strb != null){
      val strbArray = strb.toByteArray.reverse
      val origin = readArray(address,width)
      // replace with origin data according to strobes
      for(i <- Range(0,width,8)){
        val strb = strbArray.applyOrElse(i>>3,(x:Int)=>0.toByte).toInt
        if(strb != 0xff){
          for(j <- 0 until 8;k = i + j){
            if(k < width && (strb & (1<<j)) == 0){
              result(k) = origin(k)
            }
          }
        }
      }
    }
    
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
  burstLength : Int,
  burstSize   : Int,
  burstType   : Int,
  id          : Long
) {
  val dataTransactionSize : Int  = (1 + burstLength) << burstSize
  val lowerWrapBoundary   : Long = (address / dataTransactionSize) * dataTransactionSize 
  val upperWrapBoundary   : Long = lowerWrapBoundary + dataTransactionSize
  def incrAddress(i : Int): Long = ((address >> burstSize) + i) << burstSize
  def wrapAddress(i : Int): Long = {
    val ret = incrAddress(i)
    if(ret >= upperWrapBoundary){
      ret - dataTransactionSize
    }
    else ret
  }
  // check for read/write over 4k boundary
  if(burstType == 1){
    assert(
      assertion = ((burstLength << burstSize) + (address & 4095)) <= 4095,
      message   = s"Read/write request crossing 4k boundary (addr=${address.toHexString}, len=${burstLength.toLong.toHexString}"
    )
  }
  def burstAddress(i : Int):Long = {
    val ret = burstType match {
      case 0 => address        // FIXED
      case 1 => incrAddress(i) // INCR
      case 2 => wrapAddress(i) // WRAP
    }
    ret
  }
  def alignedBurstAddress(i : Int, maxBurstSize : Int):Long = {
    (burstAddress(i) >> maxBurstSize) << maxBurstSize
  }
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
  defaultBurstType     : Int = 1,
  useAlteraBehavior    : Boolean = false
) {

}

case class AxiMemorySim(axi : Axi4, clockDomain : ClockDomain, config : AxiMemorySimConfig) {
  val memory = SparseMemory()
  val pending_reads = new mutable.Queue[AxiJob]
  val pending_writes = new mutable.Queue[AxiJob]
  val threads = new mutable.Queue[SimThread]

  /** Bus word width in bytes */
  val busWordWidth = axi.config.dataWidth / 8
  val maxBurstSize = log2Up(busWordWidth)

  def newAxiJob(address : Long, burstLength : Int, burstSize : Int, burstType : Int, id : Long) : AxiJob = {
    AxiJob(address, burstLength, burstSize, burstType, id)
  }

  def newAxiJob(ax : Axi4Ax) : AxiJob = {
    newAxiJob(
      address = ax.addr.toLong,
      burstLength = getLen(ax),
      burstSize = getSizeAndCheck(ax),
      burstType = getBurst(ax),
      id = getId(ax)
    )
  }

  def start() : Unit = {
    threads.enqueue(fork {
      handleAr(axi.ar)
    })

    threads.enqueue(fork {
      handleR(axi.r)
    })

    if(config.useAlteraBehavior) {
      threads.enqueue(fork {
        handleAwAndW(axi.w, axi.aw, axi.b)
      })
    }
    else {
      threads.enqueue(fork {
        handleAw(axi.aw)
      })

      threads.enqueue(fork {
        handleW(axi.w, axi.b)
      })
    }
  }

  def stop(): Unit = {
    threads.map(f => f.terminate())
  }

  def reset(): Unit = {
    stop()
    pending_reads.clear()
    pending_writes.clear()
    start()
  }

  def getLen(ax : Axi4Ax):Int = {
    if(ax.config.useLen) ax.len.toInt else 0
  }
  def getSize(ax : Axi4Ax):Int = {
    if(ax.config.useSize) ax.size.toInt else maxBurstSize
  }
  def getSizeAndCheck(ax : Axi4Ax):Int = {
    val burstSize = getSize(ax)
    assert(burstSize <= maxBurstSize)
    burstSize
  }
  def getId(ax : Axi4Ax) : Long = {
    if(ax.config.useId) ax.id.toLong else 0L
  }

  def getBurst(ax : Axi4Ax) : Int = {
    if(ax.config.useBurst) ax.burst.toInt else config.defaultBurstType
  }

  def getStrb(w : Axi4W) : BigInt = {
    if(w.config.useStrb) w.strb.toBigInt else null
  }

  def setLast(r : Axi4R, last : Boolean) : Unit = {
    if(r.config.useLast){
      r.last #= last
    }
  }

  def handleAr(ar : Stream[Axi4Ar]) : Unit = {
    println("Handling AXI4 Master read cmds...")

    ar.ready #= false

    while(true) {
      ar.ready #= true
      clockDomain.waitSamplingWhere(ar.valid.toBoolean)
      ar.ready #= false
      
      pending_reads += newAxiJob(ar.payload)

      //println("AXI4 read cmd: addr=0x" + ar.payload.addr.toLong.toHexString + " count=" + (ar.payload.len.toBigInt+1))

      if(pending_reads.length >= config.maxOutstandingReads)
        clockDomain.waitSamplingWhere(pending_reads.length < config.maxOutstandingReads)
    }
  }

  def handleR(r : Stream[Axi4R]) : Unit = {
    println("Handling AXI4 Master read resp...")

    val random = Random

    r.valid #= false
    setLast(r.payload,false)

    while(true) {
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

          if(i == job.burstLength)
            setLast(r.payload,true)
          if(r.config.useId)
            r.payload.id   #= job.id
          if(r.config.useResp)
            r.payload.resp #= 0

          r.payload.data #= memory.readBigInt(job.alignedBurstAddress(i, maxBurstSize), busWordWidth)
          clockDomain.waitSamplingWhere(r.ready.toBoolean)
          i = i + 1
        }

        r.valid #= false
        setLast(r.payload,false)

        pending_reads.dequeue()

        //println("AXI4 read rsp: addr=0x" + job.address.toLong.toHexString + " count=" + (job.burstLength+1))
      } else {
        clockDomain.waitSampling(1)
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

      pending_writes += newAxiJob(aw.payload)

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
          memory.writeBigInt(job.alignedBurstAddress(i, maxBurstSize), w.payload.data.toBigInt, busWordWidth, getStrb(w.payload))
        }

        w.ready #= false

        clockDomain.waitSampling(config.writeResponseDelay)

        b.valid #= true
        if(b.config.useId)
          b.payload.id #= job.id
        if(b.config.useResp)
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

      val job = newAxiJob(aw.payload)

      var i = 0;
      while(i <= aw.payload.len.toInt) {
        if(config.interruptProbability > random.nextInt(100)) {
          w.ready #= false
          clockDomain.waitSampling(random.nextInt(config.interruptMaxDelay + 1))
          w.ready #= true
        }
        else {
          clockDomain.waitSamplingWhere(w.valid.toBoolean)
          memory.writeBigInt(job.alignedBurstAddress(i, maxBurstSize), w.payload.data.toBigInt, busWordWidth, getStrb(w.payload))
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
      if(b.config.useId)
        b.payload.id #= job.id
      if(b.config.useResp)
        b.payload.resp #= 0
      clockDomain.waitSamplingWhere(b.ready.toBoolean)
      b.valid #= false

      //println("AXI4 write cmd: addr=0x" + aw.payload.addr.toLong.toHexString + " count=" + (aw.payload.len.toBigInt+1))
    }
  }
}

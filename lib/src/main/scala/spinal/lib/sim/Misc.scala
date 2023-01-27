package spinal.lib.sim
import java.nio.file.{Files, Paths}

import spinal.core.sim._
import spinal.lib.bus.misc.SizeMapping
import spinal.sim.SimManagerContext

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Phase(var next : Phase){
  var isActive : Boolean = false
  var activeListeners = ArrayBuffer[() => Unit]()
  var endListeners = ArrayBuffer[() => Unit]()

  def createNewNextPhase(): Phase ={
    val p = new Phase(next)
    next = p
    p
  }

  def activate(): Unit ={
    isActive = true
    activeListeners.foreach { body =>
      retain()
      fork {
        body()
        release()
      }
    }
    release()
  }
  private var retains = 1
  def retain() : Unit = retains += 1
  def release() : Unit = {
    retains -= 1
    if(retains == 0){
      isActive = false
      endListeners.foreach(_())
      next.activate()
    }
  }

  def retainer(count : Int) = new RetainerClass(count)
  class RetainerClass(count : Int) {
    var counter = 0
    if(count != 0) retain()
    def release(): Unit ={
      counter += 1
      if(counter == count){
        Phase.this.release()
      }
    }
  }
  def onActivate(listener :  => Unit) : Unit = activeListeners += (() => listener)
  def onEnd(listener :  => Unit) : Unit = endListeners += (() => listener)
  def apply(listener :  => Unit) : Unit = onActivate(listener)
  def retainFor(time : Long): Unit ={
    def doit: Unit ={
      fork{
        sleep(time)
        release()
      }
    }

    retain()
    if(isActive){
      doit
    }else{
      onActivate(doit)
    }
  }

}

class PhaseContext{
  val end = new Phase(null){
    retain()
    override def activate(): Unit = {
      super.activate()
      simSuccess()
    }
  }
  val check = new Phase(end)
  val flush = new Phase(check)
  val stimulus = new Phase(flush)
  val setup = new Phase(stimulus)
  fork{
    setup.activate()
  }
}

object Phase{
  def context = SimManagerContext.current.get[PhaseContext](Phase)
  def boot() : Unit = {
    SimManagerContext.current.manager.retain()
    SimManagerContext.current.set(this, new PhaseContext)
  }
  def setup: Phase = context.setup
  def stimulus: Phase = context.stimulus
  def flush: Phase = context.flush
  def check:  Phase = context.check
  private def end:  Phase = context.check
  def isUsed = SimManagerContext.current.contains(Phase)
}

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

case class SparseMemory(){
  val content = Array.fill[MemoryPage](4096)(null)

  def allocPage(): MemoryPage = {
    val page = new MemoryPage(1024 * 1024)
    page.clear(0xcd.toByte)
    page
  }

  def invalidPage(): MemoryPage = {
    val page = new MemoryPage(1024 * 1024)
    page.clear(0xef.toByte)
    page
  }

  def getPageIndex(address: Long): Int = {
    (address >> 20).toInt
  }

  def getOffset(address: Long): Int = {
    (address & 0xFFFFF).toInt
  }

  def getElseAlocate(idx: Int): MemoryPage = {
    if (content(idx) == null) {
      println(s"Adding page ${idx} at 0x${(idx << 20).toHexString}")
      content(idx) = allocPage()
    }
    content(idx)
  }

  def getElseInvalidPage(idx: Int): MemoryPage = {
    if (content(idx) == null) {
      println(s"Page fault while reading page ${idx} (0x${(idx << 20).toHexString})")
      invalidPage()
    }
    else
      content(idx)
  }

  def write(address: Long, data: Byte): Unit = {
    getElseAlocate(getPageIndex(address)).write(getOffset(address), data)
  }

  def write(address : Long, data : Int) : Unit = {
    for(i <- 0 to 3) {
      val a = address + i
      getElseAlocate(getPageIndex(address)).write(getOffset(a),(data >> (i*8)).toByte)
    }
  }

  def write(address : Long, data : Long) : Unit = {
    for(i <- 0 to 7) {
      val a = address + i
      getElseAlocate(getPageIndex(a)).write(getOffset(a), (data >> (i*8)).toByte)
    }
  }

  def writeArray(address: Long, data: Array[Byte]): Unit = {
    val startPageIndex = getPageIndex(address)
    val endPageIndex = getPageIndex(address + data.length - 1)
    var offset = getOffset(address)

    List.tabulate(endPageIndex - startPageIndex + 1)(_ + startPageIndex).foldLeft(data) {
      (writeData, pageIndex) => {
        val page = getElseAlocate(pageIndex)
        val bytesWritten = page.writeArray(offset, writeData)
        offset = 0
        writeData.drop(bytesWritten)
      }
    }
  }

  def writeInt(address: Long, data: Int): Unit = {
    write(address, data)
  }

  /** Writes a BigInt value to the given address.
    * The BigInt will be resized to a byte Array of given width.
    * The data will be trimmed if it is bigger than the given width.
    * If it is smaller, the unused bytes will be filled with '0x00'.
    *
    * @param address Write address.
    * @param data    Data to be written.
    * @param width   Width of the byte Array the data is resized to (if necessary).
    */
  def writeBigInt(address: Long, data: BigInt, width: Int, strb: BigInt = null) {
    var dataArray = data.toByteArray.reverse
    var length = scala.math.min(width, dataArray.length)
    var result = Array.fill[Byte](width)(0.toByte)

    for (i <- 0 until length)
      result(i) = dataArray(i)

    if (strb != null) {
      val strbArray = strb.toByteArray.reverse
      val origin = readArray(address, width)
      // replace with origin data according to strobes
      for (i <- Range(0, width, 8)) {
        val strb = strbArray.applyOrElse(i >> 3, (x: Int) => 0.toByte).toInt
        if (strb != 0xff) {
          for (j <- 0 until 8; k = i + j) {
            if (k < width && (strb & (1 << j)) == 0) {
              result(k) = origin(k)
            }
          }
        }
      }
    }
    writeArray(address, result)
  }


  def read(address : Long) : Byte = {
    getElseInvalidPage(getPageIndex(address)).read(getOffset(address))
  }

  def readByteAsInt(address : Long) : Int = read(address).toInt & 0xFF

  def readInt(address : Long) : Int = {
    var value = 0
    for(i <- 0 until 4) value |= (read(address + i).toInt & 0xFF) << i*8
    return value
  }

  /** Reads a BigInt value from the given address.
    *
    * @param address Read address.
    * @param width   Length of the byte array to be read in bytes.
    * @return BigInt read from the given address.
    */
  def readBigInt(address: Long, length: Int): BigInt = {
    val dataArray = readArray(address, length)
    val buffer = dataArray.reverse.toBuffer // revert for Little Endian representation

    // We never want negative numbers
    buffer.prepend(0.toByte)

    BigInt(buffer.toArray)
  }

  def readArray(address: Long, len: Long): Array[Byte] = {
    val startPageIndex = getPageIndex(address)
    val endPageIndex = getPageIndex(address + len - 1)
    var offset = getOffset(address)
    val buffer = new mutable.ArrayBuffer[Byte](0)

    for (i <- startPageIndex to endPageIndex) {
      val page = getElseInvalidPage(i)
      val readArray = page.readArray(offset, len.toInt - buffer.length)
      buffer.appendAll(readArray)
      offset = 0
    }

    buffer.toArray
  }

  def loadDebugSequence(address: Long, length: Int, width: Int): Unit = {
    for (i <- 0 until length) {
      writeBigInt(address + i * width, BigInt(address + i * width), width)
    }
  }


  def loadBinary(address: Long, file: String): Unit = {
    val byteArray = Files.readAllBytes(Paths.get(file))
    writeArray(address, byteArray)

    println(s"Loading 0x${byteArray.length.toHexString} bytes from ${file} to 0x${address.toHexString}")
  }

  def saveBinary(address: Long, len: Long, file: String): Unit = {
    val byteArray = readArray(address, len)
    Files.write(Paths.get(file), byteArray)

    println(s"Saving 0x${len.toHexString} bytes from 0x${address.toHexString} to ${file}")
  }

  def loadHex(file: String, offset: Long = 0): Unit = {
    import spinal.lib.misc.HexTools.readHexFile
    readHexFile(file, offset, (address, data) => {
      writeBigInt(address, data, 2)
    })
  }

  //def saveImage(address : Long, len : Long, file : String) : Unit = {
  //  val byteArray = readArray(address, len)
  //  val img = new BufferedImage(480, 640, BufferedImage.TYPE_INT_RGB)
  //  img.setData(Raster.createRaster(img.getSampleModel(), new DataBufferByte(byteArray, byteArray.length), null))
  //  ImageIO.write(img, "png", new File(file));
  //}

}


case class MemoryRegionAllocator(base : Long, size : Long){
//  case class Allocation(base : Long, size : Long)
  val allocations = mutable.HashSet[SizeMapping]()
  def sizeRand() = (Random.nextLong()&Long.MaxValue)%size
  def free(region : SizeMapping) = allocations.remove(region)
  def free(address : BigInt) = {
    allocations.remove(allocations.find(a => a.base <= address && a.base + a.size > address).get)
  }
  def isAllocated(address : Long) = allocations.exists(a => a.base <= address && a.base + a.size > address)
  def isAllocated(address : Long, size : Long) = allocations.exists(a => a.base < address+size && a.base + a.size > address)
  def allocate(sizeMax : Long, sizeMin : Long) : SizeMapping = {
    var tryies = 0
    while(tryies < 10){

      val region = SizeMapping(sizeRand() + base, Random.nextLong%(sizeMax-sizeMin + 1)+sizeMin)
      if(allocations.forall(r => r.base > region.end || r.end < region.base) && region.end < size) {
        allocations += region
        return region
      }
      tryies += 1
    }
    return null
  }
  def allocate(size : Long) : SizeMapping = {
    var tryies = 0
    while(tryies < 10){

      val region = SizeMapping(sizeRand() + base, size)
      if(allocations.forall(r => r.base > region.end || r.end < region.base) && region.end < MemoryRegionAllocator.this.size) {
        allocations += region
        return region
      }
      tryies += 1
    }
    return null
  }

  def allocateAligned(size : Long) : SizeMapping = {
    var tryies = 0
    while(tryies < 10){

      val region = SizeMapping(sizeRand() + base & ~(size-1), size)
      if(allocations.forall(r => r.base > region.end || r.end < region.base) && region.end < MemoryRegionAllocator.this.size) {
        allocations += region
        return region
      }
      tryies += 1
    }
    return null
  }

  def allocateAligned(size : Long, align : Long) : SizeMapping = {
    var tryies = 0
    while(tryies < 50){

      val region = SizeMapping(sizeRand() + base & ~(align-1), size)
      if(allocations.forall(r => r.base > region.end || r.end < region.base) && region.end < MemoryRegionAllocator.this.size) {
        allocations += region
        return region
      }
      tryies += 1
    }
    return null
  }

  def allocateOn(base : Long, size : Long) = allocations += SizeMapping(base, size)
  def freeSize = size - allocations.foldLeft(0)(_ + _.size.toInt)
}


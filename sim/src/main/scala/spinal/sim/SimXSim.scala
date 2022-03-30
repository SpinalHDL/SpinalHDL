package spinal.sim

import spinal.sim.xsi.VecI8
import java.util.NoSuchElementException
import scala.collection.mutable.HashMap
import collection.JavaConverters._

class SimXSim(backend: XSimBackend) extends SimRaw {
  val instance = backend.getInterface()
  val handleMap: HashMap[Int, Int] = new HashMap()
  val filledByte = 255.toByte
  val zeroByte = 0.toByte

  def getSignalId(signal: Signal) = {
    try {
      handleMap(signal.hash)
    } catch {
      case _: NoSuchElementException => {
        val handle = instance.get_signal_handle(signal.toXsiAddress)
        handleMap += (signal.hash -> handle)
        handle
      }
    }
  }

  override def getInt(signal: Signal) = {
    val id = getSignalId(signal)
    instance.read32(id)
  }

  override def getLong(signal: Signal) = {
    val id = getSignalId(signal)
    instance.read64(id)
  }

  override def setLong(signal: Signal, value: Long) = {
    val id = getSignalId(signal)
    instance.write64(id, value)
  }

  override def getBigInt(signal: Signal) = {
    val id = getSignalId(signal)
    val value = instance.read(id, signal.width)
    if(!signal.dataType.isInstanceOf[SIntDataType]) value.add(0, zeroByte)
    BigInt(value.asScala.toArray.map{x => x.toByte})
  }

  override def setBigInt(signal: Signal, value: BigInt) = {
    val id = getSignalId(signal)
    var value_arr = value.toByteArray
    if (value_arr.length*8 < signal.dataType.width) {
      if (signal.dataType.isInstanceOf[SIntDataType] &&
        (value  < 0)) {
        value_arr = (Array.fill[Byte](signal.dataType.width/8 -
          value_arr.length + 1)(filledByte) ++
          value_arr)
      } else {
        value_arr = (Array.fill[Byte](signal.dataType.width/8 -
          value_arr.length + 1)(zeroByte) ++
          value_arr)
      }
    }
    instance.write(id, signal.width, new VecI8(value_arr))
  }

  override def getIntMem(signal: Signal, index: Long) = {
    Int.MaxValue
  }

  override def getLongMem(signal: Signal, index: Long) = {
    Long.MaxValue
  }

  override def setLongMem(signal: Signal, value: Long, index: Long) = { }

  override def getBigIntMem(signal: Signal, index: Long) = {
    BigInt(0)
  }

  override def setBigIntMem(signal: Signal, value: BigInt, index: Long) = { }

  override def sleep(cycles: Long) = {
    instance.sleep(cycles)
  }

  override def enableWave() = { }

  override def disableWave() = { }

  override def eval() = {
    instance.sleep(0)
    false
  }

  override def end() = { }

  override def isBufferedWrite = false
}

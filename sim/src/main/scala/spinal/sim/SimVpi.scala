package spinal.sim

import spinal.sim.vpi._
import collection.JavaConverters._
import scala.collection.mutable.HashMap
import java.util.NoSuchElementException
import scala.sys._

class VpiException(message: String) extends Exception(message)

class SimVpi(backend: VpiBackend) extends SimRaw {

  val filledByte = 255.toByte
  val zeroByte = 0.toByte
  val (nativeIface, thread) = backend.instanciate()
  val handleMap: HashMap[Int, Long] = new HashMap()
  val vectorInt8 = new VectorInt8()

  override def getInt(signal : Signal) = {
    val id = getSignalId(signal)
    val ret = nativeIface.read32(id)
    
    if(signal.dataType.isInstanceOf[SIntDataType] &&
       (signal.dataType.width < 32)){
        (ret << (32-signal.dataType.width)) >> (32-signal.dataType.width)  
    } else ret
  }

  def setInt(signal : Signal, value: Int) {
    val id = getSignalId(signal)
    if (signal.dataType.width > 32) this.setBigInt(signal, BigInt(value))
    else nativeIface.write32(id, value)
  }

  override def getLong(signal : Signal) = {
    val id = getSignalId(signal)
    val ret = nativeIface.read64(id)
    if(signal.dataType.isInstanceOf[SIntDataType] &&
       (signal.dataType.width < 64)){
        (ret << (64-signal.dataType.width)) >> (64-signal.dataType.width) 
    } else ret
  }

  override def setLong(signal : Signal, value: Long) {
    val id = getSignalId(signal)
    if (signal.dataType.width > 64) this.setBigInt(signal, BigInt(value))
    else nativeIface.write64(id, value)
  }

  override def getBigInt(signal : Signal) : BigInt = {
    val id = getSignalId(signal)
    nativeIface.read(id, vectorInt8)
    if(!signal.dataType.isInstanceOf[SIntDataType]) vectorInt8.add(0, zeroByte) 
    BigInt(vectorInt8.asScala.toArray.map{x => x.toByte})
  }

  override def setBigInt(signal : Signal, value : BigInt) {
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
    nativeIface.write(id, new VectorInt8(value_arr))
  }

  override def getIntMem(signal : Signal, index : Long) = {
    val id = getSignalId(signal)
    val ret = nativeIface.read32_mem(id, index)
    
    if(signal.dataType.isInstanceOf[SIntDataType] &&
       (signal.dataType.width < 32)){
        (ret << (32-signal.dataType.width)) >> (32-signal.dataType.width)  
    } else ret
  }

  def setIntMem(signal : Signal, value: Int, index : Long) {
    val id = getSignalId(signal)
    if (signal.dataType.width > 32) this.setBigIntMem(signal, BigInt(value), index)
    else nativeIface.write32_mem(id, value, index)
  }

  override def getLongMem(signal : Signal, index : Long) = {
    val id = getSignalId(signal)
    val ret = nativeIface.read64_mem(id, index)
    if(signal.dataType.isInstanceOf[SIntDataType] &&
       (signal.dataType.width < 64)){
        (ret << (64-signal.dataType.width)) >> (64-signal.dataType.width) 
    } else ret
  }

  override def setLongMem(signal : Signal, value: Long, index : Long) {
    val id = getSignalId(signal)
    if (signal.dataType.width > 64) this.setBigIntMem(signal, BigInt(value), index)
    else nativeIface.write64_mem(id, value, index)
  }

  override def getBigIntMem(signal : Signal, index : Long) : BigInt = {
    val id = getSignalId(signal)
    nativeIface.read_mem(id, vectorInt8, index)
    if(!signal.dataType.isInstanceOf[SIntDataType]) vectorInt8.add(0, zeroByte) 
    BigInt(vectorInt8.asScala.toArray.map{x => x.toByte})
  }

  override def setBigIntMem(signal : Signal, value : BigInt, index : Long) {
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
    nativeIface.write_mem(id, new VectorInt8(value_arr), index)
  }

  override def sleep(cycles : Long) {
    nativeIface.sleep(cycles)
  }
  
  override def eval() : Boolean = {
    nativeIface.eval
    false
  } 

  def randomize(seed: Long) {
    nativeIface.randomize(seed)
  }

  override def end() {
    nativeIface.close
    thread.join
  }

  def getSignalId(signal: Signal) : Long = {
    try {
      handleMap(signal.hash)
    } catch {
      case _: NoSuchElementException => {
        val handle = nativeIface.get_signal_handle(signal.toVpiAddress)
        handleMap += (signal.hash -> handle)
        handle
      }
    }
  }
  
  override def enableWave() {}
  override def disableWave() {}
  override def isBufferedWrite = backend.isBufferedWrite
}

package spinal.sim

import spinal.sim.vpi._
import collection.JavaConverters._
import scala.collection.mutable.HashMap
import java.util.NoSuchElementException
import scala.sys._

class VpiException(message: String) extends Exception(message)

class SimVpi(backend: VpiBackend) extends SimRaw {

  val zeroByte = 0.toByte
  val (nativeIface, thread) = backend.instanciate()
  val handleMap: HashMap[Int, Long] = new HashMap()
  val vectorInt8 = new VectorInt8()

  override def getInt(signal : Signal) = {
    val id = getSignalId(signal)
    nativeIface.read32(id)
  }

  //for some reason setInt is not in SimRaw...
  def setInt(signal : Signal, value: Int) {
    val id = getSignalId(signal)
    nativeIface.write32(id, value)
  }

  override def getLong(signal : Signal) = {
    val id = getSignalId(signal)
    nativeIface.read64(id)
  }

  override def setLong(signal : Signal, value: Long) {
    val id = getSignalId(signal)
    nativeIface.write64(id, value)
  }

  override def getBigInt(signal : Signal) : BigInt = {
    val id = getSignalId(signal)
    nativeIface.read(id, vectorInt8)
    if(!signal.dataType.isInstanceOf[SIntDataType]) vectorInt8.add(0, zeroByte) 
    BigInt(vectorInt8.asScala.toArray.map{x => x.toByte})
  }

  override def setBigInt(signal : Signal, value : BigInt) {
    val id = getSignalId(signal)
    signal.dataType.checkBigIntRange(value, signal)
    nativeIface.write(id, new VectorInt8(value.toByteArray))
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

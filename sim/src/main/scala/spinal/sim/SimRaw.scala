package spinal.sim


abstract class SimRaw {
  var userData : Any = null
  def getInt(signal : Signal) : Int
  def getLong(signal : Signal) : Long
  def setLong(signal : Signal, value : Long) : Unit
  def getBigInt(signal : Signal) : BigInt
  def setBigInt(signal : Signal, value : BigInt): Unit
  def getIntMem(signal : Signal, index : Long) : Int
  def getLongMem(signal : Signal, index : Long) : Long
  def setLongMem(signal : Signal, value : Long, index : Long): Unit
  def getBigIntMem(signal : Signal, index : Long) : BigInt
  def setBigIntMem(signal : Signal, value : BigInt, index : Long): Unit
  def sleep(cycles : Long): Unit
  def enableWave(): Unit
  def disableWave(): Unit
  def eval() : Boolean
  def end(): Unit
  def isBufferedWrite : Boolean
}

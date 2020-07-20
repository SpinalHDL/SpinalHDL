package spinal.sim


abstract class SimRaw {
  var userData : Any = null
  def getInt(signal : Signal) : Int
  def getLong(signal : Signal) : Long
  def setLong(signal : Signal, value : Long)
  def getBigInt(signal : Signal) : BigInt
  def setBigInt(signal : Signal, value : BigInt)
  def getIntMem(signal : Signal, index : Long) : Int
  def getLongMem(signal : Signal, index : Long) : Long
  def setLongMem(signal : Signal, value : Long, index : Long)
  def getBigIntMem(signal : Signal, index : Long) : BigInt
  def setBigIntMem(signal : Signal, value : BigInt, index : Long)
  def sleep(cycles : Long)
  def enableWave()
  def disableWave()
  def eval() : Boolean
  def end()
  def isBufferedWrite : Boolean
}

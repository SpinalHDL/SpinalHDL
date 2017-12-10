package spinal.sim

import scala.util.continuations._

//object SimRaw {
//  val threadLocal = new ThreadLocal[SimRaw]
//  def current = threadLocal.get()
//  def peak(bt : Signal) : Long = current.getLong(bt)
//  def poke(bt : Signal, value : Long)= current.setLong(bt, value)
//  def eval() = current.eval
//  def sleep(cycles : Long) = current.sleep(cycles)
//  def end() = current.end()
//}

abstract class SimRaw(){
  var userData : Any = null
  def getInt(signal : Signal) : Int
  def getLong(signal : Signal) : Long
  def setLong(signal : Signal, value : Long)
  def getBigInt(signal : Signal) : BigInt
  def setBigInt(signal : Signal, value : BigInt)
  def sleep(cycles : Long)
  def eval()
  def end()
  def isBufferedWrite : Boolean
}
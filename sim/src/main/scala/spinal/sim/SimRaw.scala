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
  def getInt(bt : Signal) : Int
  def getLong(bt : Signal) : Long
  def setLong(bt : Signal, value : Long)
  def eval()
  def sleep(cycles : Long)
  def end()
  def isBufferedWrite : Boolean
}
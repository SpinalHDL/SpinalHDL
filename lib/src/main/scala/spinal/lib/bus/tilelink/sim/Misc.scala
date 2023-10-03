package spinal.lib.bus.tilelink.sim

import scala.collection.mutable.ArrayBuffer
import spinal.core.sim._

class WeightedDistribution[T](){
  val storage = ArrayBuffer[(Int, () => T)]()
  var total = 0
  def apply(weight : Int)(that : => T) = {
    storage += weight -> (() => that)
    total += weight
  }

  def randomExecute() : T = {
    val rand = simRandom.nextInt(total)
    var stack = 0
    for((w,body) <- storage) {
      stack += w
      if(rand < stack) return body()
    }
    ???
  }
}

class OrderingCtrl(bytes : Int){
  val todo = Array.fill(bytes)(1)
  var counter = bytes

  def -=(id : Int) : Unit = {
    if(id >= 0 && id < bytes && todo(id) == 1) {
      todo(id) = 0
      counter -= 1
    }
  }
  def -=(id : Int, count : Int) : Unit = {
    for(i <- id until id+count) this -= i
  }

  def empty = counter == 0
}
package spinal.lib.bus.tilelink.sim

import spinal.lib.bus.tilelink.DebugId

import scala.collection.mutable

class OrderingManager(width : Int) {
  val tasks = mutable.LinkedHashMap[Long, OrderingArgs => Unit]()

  def allocate(body : OrderingArgs => Unit) : Long = {
    while(tasks.contains(allocationCounter)){
      incr()
    }
    val id = allocationCounter
    assert(!tasks.contains(id))
    tasks(id) = body
    incr()
    id
  }

  def remove(id : Long): Unit ={
    assert(tasks.contains(id))
    tasks.remove(id)
  }
  def call(id : Long)(args : OrderingArgs) = {
    tasks.get(id) match {
      case Some(body) => body(args)
    }
  }

  var allocationCounter = 0l
  def incr(): Unit ={
    allocationCounter = ((allocationCounter + 1) & ((1l << width)-1)) max DebugId.space.reserved
  }
}

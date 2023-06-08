package spinal.lib.bus.tilelink.sim

import spinal.lib.bus.tilelink.DebugId

import scala.collection.mutable

class IdCallbackHub {
  val tasks = mutable.LinkedHashMap[Long, Any => Unit]()

  def add(id : Long)(body : Any => Unit) = {
    assert(!tasks.contains(id))
    tasks(id) = body
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
}


class IdAllocator(width : Int) {
  val allocated = mutable.LinkedHashSet[Long]()

  def allocate() : Long = {
    while(allocated.contains(allocationCounter)){
      incr()
    }
    val id = allocationCounter
    incr()
    id
  }

  def remove(id : Long): Unit ={
    assert(allocated.contains(id))
    allocated.remove(id)
  }

  var allocationCounter = 0l
  def incr(): Unit ={
    allocationCounter = ((allocationCounter + 1) & ((1l << width)-1)) max DebugId.space.reserved
  }
}
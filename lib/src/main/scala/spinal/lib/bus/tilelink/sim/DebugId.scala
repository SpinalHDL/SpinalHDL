package spinal.lib.bus.tilelink.sim

import spinal.lib.bus.tilelink.DebugId
import spinal.sim.{SimThread, SimThreadUnschedule}
import spinal.core.sim._

import scala.collection.mutable

class IdCallback {
  val tasks = mutable.LinkedHashMap[Long, mutable.LinkedHashMap[Any, Any => Unit]]()

  def add(id : Long, key : Any)(body : Any => Unit) = {
    val m = tasks.getOrElseUpdate(id, mutable.LinkedHashMap[Any, Any => Unit]())
    assert(!m.contains(key))
    m(key) = body
  }

  def remove(id : Long, key : Any): Unit ={
    val m = tasks.getOrElseUpdate(id, mutable.LinkedHashMap[Any, Any => Unit]())
    assert(m.contains(key))
    m.remove(key)
    if(m.isEmpty) tasks.remove(id)
  }

  def call(id : Long)(args : Any) = {
    val m = tasks.get(id).foreach{l =>
      l.foreach(_._2(args))
    }
  }
}


class IdAllocator(width : Int) {
  val allocated = mutable.LinkedHashSet[Long]()

  var occupancy = 0

  protected def allocateImpl(id : Long){
    allocated += id
    occupancy += 1
  }

  def isFull() = occupancy == 1l << width

  def allocate() : Long = {
    assert(!isFull())
    while(allocated.contains(allocationCounter)){
      incr()
    }
    val id = allocationCounter
    allocateImpl(id)
    incr()
    id
  }

  def allocate(id : Long) : Unit = {
    assert(!allocated.contains(id))
    allocateImpl(id)
  }

  def remove(id : Long): Unit ={
    assert(allocated.contains(id))
    allocated.remove(id)
    occupancy -= 1
  }

  var allocationCounter = 0l
  def incr(): Unit ={
    allocationCounter = ((allocationCounter + 1) & ((1l << width)-1)) max DebugId.space.reserved
  }
}

class BlockingIdAllocator(width : Int) extends IdAllocator(width){
  val waitings = mutable.Queue[SimThread]()


  override def allocate() = {
    while(isFull()){
      val t = simThread
      waitings += t
      t.suspend()
    }
    super.allocate()
  }

  override def remove(id: Long) = {
    if(waitings.nonEmpty){
      waitings.dequeue().resume()
    }
    super.remove(id)
  }
}
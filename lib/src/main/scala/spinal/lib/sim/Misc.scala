package spinal.lib.sim
import java.nio.file.{Files, Paths}

import spinal.core.sim._
import spinal.sim.SimManagerContext

import scala.collection.mutable.ArrayBuffer

class Phase(var next : Phase){
  var isActive : Boolean = false
  var activeListeners = ArrayBuffer[() => Unit]()
  var endListeners = ArrayBuffer[() => Unit]()

  def createNewNextPhase(): Phase ={
    val p = new Phase(next)
    next = p
    p
  }

  def activate(): Unit ={
    isActive = true
    activeListeners.foreach { body =>
      retain()
      fork {
        body()
        release()
      }
    }
    release()
  }
  private var retains = 1
  def retain() : Unit = retains += 1
  def release() : Unit = {
    retains -= 1
    if(retains == 0){
      isActive = false
      endListeners.foreach(_())
      next.activate()
    }
  }

  def retainer(count : Int) = new {
    var counter = 0
    if(count != 0) retain()
    def release(): Unit ={
      counter += 1
      if(counter == count){
        Phase.this.release()
      }
    }
  }
  def onActivate(listener :  => Unit) : Unit = activeListeners += (() => listener)
  def onEnd(listener :  => Unit) : Unit = endListeners += (() => listener)
  def apply(listener :  => Unit) : Unit = onActivate(listener)
  def retainFor(time : Long): Unit ={
    def doit: Unit ={
      fork{
        sleep(time)
        release()
      }
    }

    retain()
    if(isActive){
      doit
    }else{
      onActivate(doit)
    }
  }

}

class PhaseContext{
  val end = new Phase(null){
    retain()
    override def activate(): Unit = {
      super.activate()
      simSuccess()
    }
  }
  val check = new Phase(end)
  val flush = new Phase(check)
  val stimulus = new Phase(flush)
  val setup = new Phase(stimulus)
  fork{
    setup.activate()
  }
}

object Phase{
  def context = SimManagerContext.current.get[PhaseContext](Phase)
  def boot() : Unit = {
    SimManagerContext.current.manager.retain()
    SimManagerContext.current.set(this, new PhaseContext)
  }
  def setup: Phase = context.setup
  def stimulus: Phase = context.stimulus
  def flush: Phase = context.flush
  def check:  Phase = context.check
  private def end:  Phase = context.check
  def isUsed = SimManagerContext.current.contains(Phase)
}

case class SparseMemory(){
  val content = Array.fill[Array[Byte]](4096)(null)
  def getElseAlocate(idx : Int) = {
    if(content(idx) == null) content(idx) = new Array[Byte](1024*1024)
    content(idx)
  }

  def write(address : Long, data : Byte) : Unit = {
    getElseAlocate((address >> 20).toInt)(address.toInt & 0xFFFFF) = data
  }

  def read(address : Long) : Byte = {
    getElseAlocate((address >> 20).toInt)(address.toInt & 0xFFFFF)
  }

  def loadBin(offset : Long, file : String): Unit ={
    val bin = Files.readAllBytes(Paths.get(file))
    for(byteId <- 0 until bin.size){
      write(offset + byteId, bin(byteId))
    }
  }
}
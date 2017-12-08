package spinal.sim



import spinal.core.{BaseType, ClockDomain}

import scala.collection.Iterator
import scala.collection.mutable.ArrayBuffer
import scala.util.continuations._


//object StandaloneGeneratorFix{
//  def apply[T](block : => Unit@suspendable) ={
//    new StandaloneGenerator[T](){
//      generate {
//        block
//      }
//    }
//    //    new StandaloneGenerator[Any] {
//    //      generate {
//    //        yld(1)
//    //        yld(2)
//    //        yld(3)
//    //      }
//    //    }
//  }
//}

/** Standalone generic generator class.
  *
  * @author Jim McBeath
  * @since 1.0.1
  */

trait SimBlocker{
  def isBlocked : Boolean
}

object SimBlockerNone extends SimBlocker{
  override def isBlocked = false
}

object SimManagedContext{
  private[sim]val threadLocal = new ThreadLocal[SimManagedContext]
  private[sim]def current = threadLocal.get()
  private[sim]def reset() = threadLocal.set(new SimManagedContext)

  def peak(bt : BaseType) : Long = current.manager.peak(bt)
  def poke(bt : BaseType, value : Long)= current.manager.poke(bt, value)
  def sleep(cycles : Long) : Unit@suspendable = current.thread.sleep(cycles)
  def fork(body : => Unit@suspendable) : SimThread@suspendable = current.manager.newThread(body)

  implicit class BaseTypePimper(bt : BaseType) {
    def toLong = current.manager.peak(bt)
    def :<< (value : Long) = current.manager.poke(bt, value)
  }

  implicit class ClockDomainPimper(cd : ClockDomain) {
    def fallingEdge = {
      val current = SimManagedContext.current.manager
      current.poke(current.getClock(cd), 0)
    }
    def risingEdge = {
      val current = SimRaw.current
      current.poke(current.getClock(cd), 1)
    }
  }

}

class SimManagedContext{
  var thread : SimThread = null
  var manager : SimManaged = null
}



class SimThread(body :  => Unit@suspendable, var time : Long){
  private val manager = SimManagedContext.current.manager
  private var nextStep: Unit => Unit = null
  var waitingThreads = ArrayBuffer[SimThread]()

  def join(): Unit@suspendable ={
    val thread = SimManagedContext.current.thread
    assert(thread != this)
    if(!this.isDone) {
      waitingThreads += thread
      thread.suspend()
    }
  }

  def sleep(cycles : Long): Unit@suspendable ={
    time += cycles
    manager.scheduleThread(this)
    suspend()
  }

  /** Subclass calls this method to generate values.
    *
    * @param body The code for your generator.
    */
  reset {
    suspend()
    body
  }


  /** True if there is another value to retrieve.
    * Call this from your main code.
    */
  def isDone: Boolean = {
    nextStep == null
  }

  /** Save our continuation to resume later. */
  def suspend(): Unit@suspendable = {
    shift { k: (Unit => Unit) =>
      nextStep = k
    }
  }

  /** If we have a next step but we don't have a value queued up,
    * run the generator until it calls yld or completes. */
  def resume() = {
    if (nextStep != null) {
      val back = nextStep
      nextStep = null
      if(back != null) back()
    }
    if(isDone){
      waitingThreads.foreach(thread => {
        thread.time = time
        SimManagedContext.current.manager.scheduleThread(thread)
      })
    }
  }
}

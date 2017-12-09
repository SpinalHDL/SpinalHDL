package spinal.sim

import spinal.core.{BaseType, ClockDomain, Component}

import scala.util.continuations.suspendable

object SimVerilatorManaged {
  def apply[T <: Component](gen: => T)(body: T => Unit@suspendable): Unit = {
    val sim = SimVerilator(gen)
    val manager = new SimManager(sim)
    manager.run(body(sim.dut.asInstanceOf[T]))
  }
}


object SimManagerApi{
  private[sim]val threadLocal = new ThreadLocal[SimManagerApi]
  private[sim]def current = threadLocal.get()
  private[sim]def reset() = threadLocal.set(new SimManagerApi)

//  def peak(bt : BaseType) : Long = current.manager.peak(bt)
//  def poke(bt : BaseType, value : Long)= current.manager.poke(bt, value)
  def sleep(cycles : Long) : Unit@suspendable = current.thread.sleep(cycles)
  def fork(body : => Unit@suspendable) : SimThread@suspendable = current.manager.newThread(body)

  implicit class BaseTypePimper(bt : BaseType) {
    def toLong = current.manager.peak(bt)
    def :<< (value : Long) = current.manager.poke(bt, value)
  }

  implicit class ClockDomainPimper(cd : ClockDomain) {
    def fallingEdge = {
      val current = SimManagerApi.current.manager
      current.poke(current.getClock(cd), 0)
    }
    def risingEdge = {
      val current = SimRaw.current
      current.poke(current.getClock(cd), 1)
    }
  }
}

class SimManagerApi{
  var thread : SimThread = null
  var manager : SimManager = null
}

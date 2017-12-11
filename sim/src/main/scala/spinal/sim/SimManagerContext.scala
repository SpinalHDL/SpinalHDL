package spinal.sim


object SimManagerContext{
  val threadLocal = new ThreadLocal[SimManagerContext]
  def current = threadLocal.get()
  def reset() = threadLocal.set(new SimManagerContext)
}

class SimManagerContext{
  var thread : SimThread = null
  var manager : SimManager = null
}

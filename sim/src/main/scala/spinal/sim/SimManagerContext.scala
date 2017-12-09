package spinal.sim


object SimManagerContext{
  private[sim]val threadLocal = new ThreadLocal[SimManagerContext]
  private[sim]def current = threadLocal.get()
  private[sim]def reset() = threadLocal.set(new SimManagerContext)
}

class SimManagerContext{
  var thread : SimThread = null
  var manager : SimManager = null
}

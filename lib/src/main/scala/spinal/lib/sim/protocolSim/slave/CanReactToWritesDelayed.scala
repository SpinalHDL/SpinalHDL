package spinal.lib.sim.protocolSim.slave

/** A slave which can be setup to react to writes after a given duration */
trait CanReactToWritesDelayed extends CanReactToWrites with CanDelay {

  /** Defines how to react to data writes
    *
    * @param calls
    *   the number of handler calls (usually clock cycles) before responding
    * @param writer
    *   the action to execute
    */
  def setOnWritesAfter(calls: Long)(writer: (Address, Data) => Unit): Unit =
    _setOnWrites(delay(calls)(fromWriter(writer)))
}

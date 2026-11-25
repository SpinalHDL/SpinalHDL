package spinal.lib.sim.protocolSim.slave

/** A slave which can be setup to react to reads after a given duration */
trait CanReactToReadsDelayed extends CanReactToReads with CanDelay {

  /** Defines how to react to data reads
    *
    * @param calls
    *   the number of handler calls (usually clock cycles) before responding
    * @param reader
    *   the action to execute, returning the read value
    */
  def setOnReadsAfter(calls: Long)(reader: Address => Data): Unit =
    _setOnReads(delay(calls)(fromReader(reader)))
}

package spinal.lib.sim.protocolSim.master

/** Extend it to build a transfer with no returned value */
trait WriteTransfer extends Transfer {
  private var _isDone = false

  /** Run this function to declare the transfer as completed */
  protected final def done(): Unit = _isDone = true

  final def isDone = _isDone
}

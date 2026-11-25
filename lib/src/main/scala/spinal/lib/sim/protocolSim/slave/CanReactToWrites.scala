package spinal.lib.sim.protocolSim.slave

/** A slave which can be setup to react to writes */
trait CanReactToWrites extends ProtocolSlave {

  /** Converts an address-data-basae action to a behavior
    *
    * @param writer
    *   action to execute
    * @return
    *   matching request-to-response behavior
    */
  def fromWriter(writer: (Address, Data) => Unit): Behavior = { req =>
    writer(req.addr, req.wdata.get)
    ready
  }

  /** Defines how to react to data writes
    *
    * @param writer
    *   the action to execute
    */
  def setOnWrites(writer: (Address, Data) => Unit): Unit =
    _setOnWrites(fromWriter(writer))

  /** Defines how to react to data writes
    *
    * @param builder
    *   builder to use
    */
  def _setOnWrites(builder: HandlerBuilder): Unit = {
    onWrites = Some(builder)
    useRwBuilder()
  }

  /** Unset previously defined write handler */
  final def unsetOnWrites(): Unit = onWrites = None
}

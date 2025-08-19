package spinal.lib.sim.protocolSim.slave

/** A slave which can be setup to react to reads */
trait CanReactToReads extends ProtocolSlave {

  /** Converts an address-to-data mapping to a behavior
    *
    * @param reader
    *   action to execute, returning the read value
    * @return
    *   matching request-to-response behavior
    */
  def fromReader(reader: Address => Data): Behavior =
    req => done(reader(req.addr))

  /** Defines how to react to data reads
    *
    * @param reader
    *   action to execute, returning the read value
    */
  final def setOnReads(reader: Address => Data): Unit =
    _setOnReads(fromReader(reader))

  /** Defines how to react to data reads
    *
    * @param builder
    *   builder to use
    */
  def _setOnReads(builder: HandlerBuilder): Unit = {
    onReads = Some(builder)
    useRwBuilder()
  }

  /** Unset previously defined read handler */
  final def unsetOnReads(): Unit = onReads = None
}

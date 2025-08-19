package spinal.lib.sim.protocolSim.slave

/** A slave of a protocol which can delay actions */
trait CanDelay extends ProtocolSlave {

  /** Response to send when a transfer is not ready yet */
  val pending: Resp

  /** Creates a handler builder, which builds the handler to run until the
    * transfer completion.
    *
    * @param calls
    *   number of calls (usually clock cycles) of the returned handler before it
    *   uses the handler built using the builder
    * @param builder
    *   the builder of the handler to use after the delay
    * @return
    *   the final handler builder
    */
  final def delay(
      calls: Long, resp: Resp = pending
  )(builder: HandlerBuilder): HandlerBuilder = { req =>
    val handler = builder(req)
    var remaining = calls;
    { () =>
      if (remaining > 0) {
        remaining -= 1
        resp
      } else {
        handler()
      }
    }
  }
}

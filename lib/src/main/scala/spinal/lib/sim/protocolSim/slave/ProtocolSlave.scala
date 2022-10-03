package spinal.lib.sim.protocolSim.slave

import scala.language.implicitConversions

/** A slave of a protocol
  *
  * It defines the following handling mechanism:
  *
  *   - Define a handler builder to setup slave
  *   - Call the handler builder when a transfer starts giving it the request,
  *     to get the handler
  *   - Call the handler until transfer completion.
  *
  * {{{
  * // Setup a handler which takes a request and returns a handler always ready
  * val handlerBuilder = req => () => ready
  * // A transfer starts, build the handler for it
  * val requestHandler = handlerBuilder(newRequest)
  * // While the transfer is in progress (eg. on clock samplings) apply the response from the handler.
  * val resp = requestHandler()
  * }}}
  */
trait ProtocolSlave extends ProtocolRelated {

  /** Closure which has already captured its request to yield responses
    *
    * Capturing the request once and not takng it as an argument repeatedly
    * makes sure that one handler is not called for different requests as it may
    * contain internal state.
    */
  final type Handler = () => Resp

  /** Takes a request to build its handler */
  final type HandlerBuilder = Req => Handler

  /** Builds a response from a request
    *
    * Can be used to define the behavior of the handler returned by a
    * HandlerBuilder, conversion to HandlerBuilder is implicit. Take care of the
    * conversion if managing internal state, see the documentation of the
    * conversion function.
    *
    * @see
    *   behavior2builder
    */
  final type Behavior = Req => Resp

  /** Builds the response to send when a transfer has completed with a payload
    *
    * To be used for read transfers.
    *
    * @param rdata
    *   data to return
    * @return
    *   the response to send back
    */
  def done(rdata: Data): Resp

  /** Response to send when a transfer has completed without a payload.
    *
    * To be used for write transfers.
    *
    * @return
    *   the response to send back
    */
  def ready: Resp

  /** Creates a `HandlerBuilder` from a block describing a behavior
    *
    * The behavior is taken by name so that internal state is created for each
    * transfer and not shared between transfers. The block must evaluate to a
    * `Behavior` closure.
    *
    * Caution: DO NOT CAPTURE STATE FROM OUTSIDE THE BLOCK unless you REALLY
    * want to share this state variable between transfers (there is little
    * chance you want to do so).
    *
    * @param behavior
    *   a block describing the expected behavior
    * @return
    *   a function to build a handler with this behavior
    */
  final implicit def behavior2builder(
      behavior: => Behavior
  ): HandlerBuilder = { req =>
    val newBehavior = behavior
    () => newBehavior(req)
  }

  /** The builder for read transers, if defined */
  protected var onReads: Option[HandlerBuilder] = None

  /** The builder for write transfers, if defined */
  protected var onWrites: Option[HandlerBuilder] = None

  /** The builder for non-read, non-write transfers, if defined */
  protected var onOthers: Option[HandlerBuilder] = None

  /** To create a handler for read or write transfers
    *
    * Fails when:
    *
    *   - Called with a read request while `onReads` is not defined
    *   - Called with a write request while `onWrites` is not defined
    *   - Called with a non-reaad, non-write transfer while `onOthers` is not
    *     defined
    *
    * @return
    *   handler of the transfer
    */
  final protected val rwBuilder: HandlerBuilder = req =>
    if (req.isRead) {
      assert(
        onReads.isDefined,
        "A read access occured while no behavior was defined for reads"
      )
      (onReads.get)(req)
    } else if (req.isWrite) {
      assert(
        onWrites.isDefined,
        "A write access occured while no behavior was defined for reads"
      )
      (onWrites.get)(req)
    } else {
      assert(
        onOthers.isDefined,
        "A non-read, non-write access occured while no behavior was defined for that"
      )
      (onOthers.get)(req)
    }

  /** To set the builder to `rwBuilder`
    *
    * Called after a read/write action is requested.
    */
  def useRwBuilder(): Unit
}

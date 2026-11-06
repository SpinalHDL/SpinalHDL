package spinal.lib.sim.protocolSim.slave

/** Types related to a protocol
  *
  * Should be extended to the specific protocol, to define general stuff about
  * it.
  */
trait ProtocolRelated {

  /** The type of an address */
  type Address = BigInt

  /** The type of some data */
  type Data = BigInt

  /** The type representing a request */
  type Req <: Request

  /** The type representing a response */
  type Resp

}

package spinal.lib.sim.protocolSim.slave

/** Request of a protocol */
abstract class Request extends ProtocolRelated {

  /** Is it a write?
    *
    * @return
    *   true if the request is a write, else false
    */
  def isWrite: Boolean

  /** Is it a read?
    *
    * @return
    *   true if the request is a read, else false
    */
  def isRead: Boolean = !isWrite

  /** The address of the request
    *
    * @return
    *   address
    */
  def addr: Address

  /** Data of a write request, if any
    *
    * @return
    *   data
    */
  def wdata: Option[Data]
}

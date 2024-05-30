package spinal.lib.wishbone.sim

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.wishbone._
import scala.collection.immutable._
import scala.util.Random

object WishboneDriver {
  def apply(bus: Wishbone, clockdomain: ClockDomain) = new WishboneDriver(bus, clockdomain)
  def apply(bus: Wishbone) = new WishboneDriver(bus, bus.CYC.clockDomain)
}

/** This is a helping class for driving the wishbone bus
  * @param bus the wishbone bus to drive
  * @param clockdomain the clockdomain where the bus reside
  */
class WishboneDriver(val bus: Wishbone, clockdomain: ClockDomain){
  val busStatus = WishboneStatus(bus)

  private def initAsMaster(): this.type = {
    bus.CYC #= false
    bus.STB #= false
    bus.WE #= false
    bus.ADR #= 0
    bus.DAT_MOSI #= 0
    Option(bus.LOCK).foreach(_ #= false)
    Option(bus.SEL).foreach(_ #= 0)
    this
  }

  private def initAsSlave() : this.type = {
    Option(bus.STALL).foreach(_ #= true)
    Option(bus.RTY).foreach(_ #= false)
    Option(bus.ERR).foreach(_ #= false)

    bus.ACK #= false
    bus.DAT_MISO #= 0
    this
  }

  def init() : this.type = {
    if(!bus.isMasterInterface)
      initAsMaster()
    else
      initAsSlave()
  }

  init()

  /** Drive the wishbone bus as master with a transaction.
    * @param transaction The transaction to send.
    */
  private def sendAsMaster(transaction : WishboneTransaction, we: Boolean): Unit = {
    transaction.driveAsMaster(bus,we)
    clockdomain.waitSamplingWhere(busStatus.isRequestAck)
  }

  /** Drive the wishbone bus as master.
    * @param transactions a sequence of transactions that compouse the wishbone cycle
    */
  private def sendBlockAsMaster(transactions: Seq[WishboneTransaction], we: Boolean): Unit = {
    bus.CYC #= true
    transactions.dropRight(1).foreach{ tran =>
      bus.STB #= true
      sendAsMaster(tran, we)
      if(!bus.config.isPipelined){
        bus.STB #= false
        clockdomain.waitSampling()
      }
    }
    bus.STB #= true
    sendAsMaster(transactions.last, we)
    bus.STB #= false
    bus.CYC #= false
  }

  /** Drive the wishbone bus as master in a pipelined way.
    * @param transactions a sequence of transactions that compouse the wishbone cycle
    */
  private def sendPipelinedBlockAsMaster(transactions: Seq[WishboneTransaction], we: Boolean): Unit = {
    bus.CYC #= true
    bus.STB #= true
    val ackCounter = fork{
      var counter = 0
      while(counter < transactions.size){
        //println(s"Wait at ${simTime()}")
        clockdomain.waitSamplingWhere(busStatus.isResponse)
        //println(s"Response at ${simTime()}")
        counter = counter + 1
      }
    }
    transactions.foreach(sendAsMaster(_, we))
    bus.STB #= false
    ackCounter.join()
    bus.CYC #= false
  }

  /** Drive the wishbone bus as slave with a transaction, and acknoledge the master.
    * @param transaction The transaction to send.
    */
  private def sendAsSlave(transaction : WishboneTransaction): Unit = {
    clockdomain.waitSamplingWhere(busStatus.isTransfer)
    transaction.driveAsSlave(bus)
    bus.ACK #= true
    waitUntil(!busStatus.isTransfer)
    bus.ACK #= false
  }

  /** Drive the wishbone bus as a slave.
    * this function can hang if the master require more transactions than specified
    * @param transactions a sequence of transactions that compouse the wishbone cycle
    */
  private def sendBlockAsSlave(transactions: Seq[WishboneTransaction]): Unit = {
    transactions.foreach{ transaction =>
      sendAsSlave(transaction)
    }
  }

  /** Drive the wishbone bus as a slave in a pipelined way.
    * this function can hang if the master require more transactions than specified
    * @param transactions a sequence of transactions that compouse the wishbone cycle
    */
  private def sendPipelinedBlockAsSlave(transactions: Seq[WishboneTransaction]): Unit = {
    bus.STALL #= false
    bus.ACK #= false
    transactions.foreach{ transaction =>
      sendAsSlave(transaction)
    }
    waitUntil(!busStatus.isTransfer)
    bus.ACK #= false
  }

  /** Drive the wishbone bus.
    * This will utomatically selects the wright function to use
    * @param transactions a sequence of transactions that compouse the wishbone cycle
    */
  def drive(transactions: Seq[WishboneTransaction], we: Boolean= true): Unit = {
    (bus.isMasterInterface,bus.config.isPipelined) match {
      case (false,false) => sendBlockAsMaster(transactions,we)
      case (false,true)  => sendPipelinedBlockAsMaster(transactions,we)
      case (true,false)  => sendBlockAsSlave(transactions)
      case (true,true)   => sendPipelinedBlockAsSlave(transactions)
    }
  }

  /** Dumb slave acknowledge.
    */
  def slaveAckResponse(): Unit = {
    clockdomain.waitSamplingWhere(busStatus.masterHasRequest)
    var hadAck = false
    var timeout = 0
    while(busStatus.isCycle && !hadAck) {
      timeout += 1
      hadAck = simRandom.nextBoolean || timeout > 10
      bus.ACK #= hadAck
      clockdomain.waitSampling()
    }
    bus.ACK #= false
  }

  /** Dumb acknowledge, as a pipelined slave.
    */
  def slaveAckPipelinedResponse(): Unit = {
    bus.STALL #= true
    clockdomain.waitSamplingWhere(busStatus.isCycle)

    fork{
      waitUntil(!busStatus.isCycle)
      bus.ACK #= false
      bus.STALL #= false
    }
    var requests = 0
    var acks = 0
    var timeout = 0
    var max_outstanding_requests = 0;
    var total_nonstalls = 0

    while(busStatus.isCycle){
      timeout += 1

      requests += busStatus.isRequestAck.toInt

      val isTimedOut = timeout > 100
      val stall = simRandom.nextBoolean && !isTimedOut
      bus.STALL #= stall
      total_nonstalls += (!stall).toInt
      val ack = (simRandom.nextBoolean || isTimedOut) && (requests > 0)
      requests -= ack.toInt
      max_outstanding_requests = max_outstanding_requests.max(requests)

      acks += ack.toInt
      bus.ACK #= ack
      //println(s"${bus} had ${requests} requests ack ${ack}(${acks}) stall ${stall}(${total_nonstalls}) at ${simTime()}")

      clockdomain.waitSampling()
    }

    assert(requests == 0, s"Bus ended cycle with ${requests} outstanding requests")
//    if(max_outstanding_requests > 1)
//      println(s"${bus} had ${max_outstanding_requests} max requests at once")

  }

  /** Dumb slave acknoledge.
    * This will utomatically selects the wright function to use
    */
  def slaveSink(): Unit = {
    val dummy = fork{
      while(true){
        if(bus.config.isPipelined)  slaveAckPipelinedResponse()
        else                        slaveAckResponse()
      }
    }
  }

}

package spinal.lib.wishbone.sim

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.wishbone._
import scala.collection.immutable._
import scala.util.Random

object WishboneDriver{
  def apply(bus: Wishbone, clockdomain: ClockDomain) = new WishboneDriver(bus,clockdomain)
}

/** This is a helping class for driving the wishbone bus
  * @param bus the wishbone bus to drive
  * @param clockdomain the clockdomain where the bus reside
  */
class WishboneDriver(bus: Wishbone, clockdomain: ClockDomain){
  val busStatus = WishboneStatus(bus)

  /** Drive the wishbone bus as master with a transaction.
    * @param transaction The transaction to send.
    */
  def sendAsMaster(transaction : WishboneTransaction, we: Boolean): Unit@suspendable = {
    transaction.driveAsMaster(bus,we)
    if(!bus.config.isPipelined) clockdomain.waitSamplingWhere(busStatus.isAck)
    else clockdomain.waitSamplingWhere(!busStatus.isStall)
  }

  /** Drive the wishbone bus as master.
    * @param transactions a sequence of transactions that compouse the wishbone cycle
    */
  def sendBlockAsMaster(transactions: Seq[WishboneTransaction], we: Boolean): Unit@suspendable = {
    bus.CYC #= true
    transactions.dropRight(1).suspendable.foreach{ tran =>
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
  def sendPipelinedBlockAsMaster(transactions: Seq[WishboneTransaction], we: Boolean): Unit@suspendable = {
    bus.CYC #= true
    bus.STB #= true
    val ackCounter = fork{
      var counter = 0
      while(counter < transactions.size){
        clockdomain.waitSamplingWhere(busStatus.isAck)
        counter = counter + 1
      }
    }
    transactions.suspendable.foreach(sendAsMaster(_, true))
    bus.STB #= false
    ackCounter.join()
    bus.CYC #= false
  }

  def drive(transactions: Seq[WishboneTransaction], we: Boolean): Unit@suspendable = {
    if(bus.config.isPipelined)  sendPipelinedBlock(transactions,we)
    else                        sendBlock(transactions,we)
  }
//TODO
  // def read(transaction: [Seq[WishboneTransaction],WishboneTransaction]): Unit@suspendable = {
  //   transaction match{
  //     case Seq[WishboneTransaction] => sendBlock(transaction, false)
  //     case WishboneTransaction      => sendSingle(transaction, false)
  //   }
  // }

  // def write[T <: Seq[WishboneTransaction],WishboneTransaction](transaction: T): Unit@suspendable = {
  //   transaction match{
  //     case Seq[WishboneTransaction] => sendBlock(transaction, true)
  //     case WishboneTransaction      => sendSingle(transaction, true)
  //   }
  // }

  def slaveAckResponse(): Unit@suspendable = {
    clockdomain.waitSamplingWhere(busStatus.isTransfer)
    bus.ACK #= true
    waitUntil(!busStatus.isTransfer)
    bus.ACK #= false
  }

  def slaveAckPipelinedResponse(): Unit@suspendable = {
    clockdomain.waitSamplingWhere(busStatus.isCycle)
    val cycle = fork{
      fork{
        waitUntil(!busStatus.isCycle)
        bus.ACK #= false
        bus.STALL #= false
      }
      while(busStatus.isCycle){
        val ack = Random.nextBoolean
        bus.ACK #= ack
        bus.STALL #= Random.nextBoolean && !ack
        clockdomain.waitSampling()
      }
    }
    cycle.join()
  }

  def slaveSink(): Unit@suspendable = {
    val dummy = fork{
      while(true){
        if(bus.config.isPipelined)  slaveAckPipelinedResponse()
        else                        slaveAckResponse()
      }
    }
  }

}

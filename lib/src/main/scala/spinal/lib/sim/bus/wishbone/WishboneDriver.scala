package spinal.lib.wishbone.sim

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.wishbone._
import scala.collection.immutable._
import scala.util.Random
import scala.language.implicitConversions

object WishboneStatus{
  def apply(bus: Wishbone) = new WishboneStatus(bus)
}

class WishboneStatus(bus: Wishbone){
  def isCycle   : Boolean = bus.CYC.toBoolean
  def isStall   : Boolean = if(bus.config.isPipelined)  isCycle && bus.STALL.toBoolean //TODO
                            else                        false
  def isTransfer: Boolean = if(bus.config.isPipelined)  isCycle && bus.STB.toBoolean //&& !bus.STALL.toBoolean //TODO
                            else                        isCycle && bus.STB.toBoolean

  def isAck     : Boolean = if(bus.config.isPipelined)  isCycle &&  bus.ACK.toBoolean //TODO
                            else                        isTransfer &&  bus.ACK.toBoolean

  def isWrite   : Boolean =                             isTransfer &&  bus.WE.toBoolean
  def isRead    : Boolean =                             isTransfer && !bus.WE.toBoolean
}

class WishboneDriver(bus: Wishbone, clockdomain: ClockDomain){
  val busStatus = WishboneStatus(bus)

  def send(transaction : WishboneTransaction, we: Boolean): Unit@suspendable = {
    bus.WE  #= we
    transaction.driveAsMaster(bus)
    if(!bus.config.isPipelined) clockdomain.waitSamplingWhere(busStatus.isAck)
    else clockdomain.waitSamplingWhere(!busStatus.isStall)
  }

  def sendSingle(transaction: WishboneTransaction, we: Boolean): Unit@suspendable = {
    bus.CYC #= true
    bus.STB #= true
    send(transaction, we)
    bus.STB #= false
    val dummy = if(bus.config.isPipelined) clockdomain.waitSamplingWhere(busStatus.isAck)
    bus.CYC #= false
  }

  def sendBlock(transactions: Seq[WishboneTransaction], we: Boolean): Unit@suspendable = {
    bus.CYC #= true
    transactions.dropRight(1).suspendable.foreach{ tran =>
      bus.STB #= true
      send(tran, we)
      if(!bus.config.isPipelined){
        bus.STB #= false
        clockdomain.waitSampling()
      }
    }
    bus.STB #= true
    send(transactions.last, we)
    bus.STB #= false
    //ackCounter.join()
    bus.CYC #= false
  }


  def sendPipelinedBlock(transactions: Seq[WishboneTransaction], we: Boolean): Unit@suspendable = {
    bus.CYC #= true
    bus.STB #= true
    val ackCounter = fork{
      var counter = 0
      while(counter < transactions.size){
        clockdomain.waitSamplingWhere(busStatus.isAck)
        counter = counter + 1
      }
    }
    transactions.suspendable.foreach(send(_, true))
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

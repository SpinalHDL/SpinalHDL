package spinal.lib.wishbone.sim

import spinal.core.log2Up
import spinal.core.sim.simRandom

import scala.collection.immutable._
import spinal.lib.bus.misc._
import spinal.lib.bus.wishbone.{Wishbone, WishboneConfig}

object WishboneSequencer{
  def apply(builder: => Seq[WishboneTransaction]) = new WishboneSequencer(builder)

  def randomGen(busConfig : WishboneConfig, upto : Int = 10) = {
    val upperBound = simRandom.nextInt(upto).max(1)
    WishboneSequencer {
      (0 until upperBound).map(
        _ => WishboneTransaction(
          BigInt(simRandom.nextLong() & (1L << (busConfig.addressWidth - log2Up(busConfig.dataWidth))) - 1),
          BigInt(simRandom.nextLong() & (1L << busConfig.dataWidth) - 1)
        )
      )
    }
  }
}

class WishboneSequencer(builder: => Seq[WishboneTransaction]){
  val transactions = new scala.collection.mutable.Queue[Seq[WishboneTransaction]]()

  def nextTransaction: Seq[WishboneTransaction] = transactions.dequeue()
  def addTransaction(transaction: Seq[WishboneTransaction]): Unit = transactions.enqueue(transaction)

  def generateTransactions(number: Int = 1): Unit = for(i <- 0 to number) addTransaction(builder)

  def isEmpty: Boolean = transactions.isEmpty
}
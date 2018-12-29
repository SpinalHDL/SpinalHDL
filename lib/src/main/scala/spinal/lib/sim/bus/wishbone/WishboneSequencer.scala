package spinal.lib.wishbone.sim

import scala.collection.immutable._
import spinal.lib.bus.misc._

object WishboneSequencer{
  def apply(builder: => Seq[WishboneTransaction]) = new WishboneSequencer(builder)
}

class WishboneSequencer(builder: => Seq[WishboneTransaction]){
  val transactions = new scala.collection.mutable.Queue[Seq[WishboneTransaction]]()

  def nextTransaction: Seq[WishboneTransaction] = transactions.dequeue()
  def addTransaction(transaction: Seq[WishboneTransaction]): Unit = transactions.enqueue(transaction)

  def generateTransactions(number: Int = 1): Unit = for(i <- 0 to number) addTransaction(builder)

  def isEmpty: Boolean = transactions.isEmpty
}
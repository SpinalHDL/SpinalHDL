package spinal.lib.bus.amba3.ahblite.sim

import scala.util.Random


class AhbLite3Sequencer(){

  val transactions = new scala.collection.mutable.ArrayBuffer[Seq[AhbLite3Transaction]]()

  def nextTransaction: Seq[AhbLite3Transaction] = transactions.remove(Random.nextInt(transactions.length))

  def addTransaction(transaction: Seq[AhbLite3Transaction]): Unit = transactions += transaction

  def isEmpty: Boolean = transactions.isEmpty
}
package spinal.lib.wishbone.sim

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.wishbone._
import scala.collection.immutable._
import scala.util.Random

case class AddressRange(base : BigInt, size: Int){
  def inRange(address: BigInt): Boolean = (address >= base) && (address <= base + size)
  //def == (range: AddressRange): Boolean = (base == range.base) && (size == range.size)
  def mask(address: BigInt): BigInt = address - base
  def randomAddressInRange: BigInt = BigInt(Random.nextInt(size)) + base
}

object WishboneTransaction{
  implicit def singleToCycle(transaction : WishboneTransaction): Seq[WishboneTransaction] = List(transaction)

  def sampleAsMaster(bus: Wishbone): WishboneTransaction = {
    val transaction = WishboneTransaction(bus.ADR.toBigInt, bus.DAT_MISO.toBigInt)
    if(bus.config.useTGA) transaction.copy(tga = bus.TGA.toBigInt)
    if(bus.config.useTGC) transaction.copy(tga = bus.TGC.toBigInt)
    if(bus.config.useTGD) transaction.copy(tga = bus.TGD_MISO.toBigInt)
    transaction
  }

  def sampleAsSlave(bus: Wishbone): WishboneTransaction = {
    val transaction = WishboneTransaction(bus.ADR.toBigInt, bus.DAT_MOSI.toBigInt)
    if(bus.config.useTGA) transaction.copy(tga = bus.TGA.toBigInt)
    if(bus.config.useTGC) transaction.copy(tgc = bus.TGC.toBigInt)
    if(bus.config.useTGD) transaction.copy(tgd = bus.TGD_MOSI.toBigInt)
    transaction
  }
}

case class WishboneTransaction( address : BigInt = 0,
                                data : BigInt = 0,
                                tga : BigInt  = 0,
                                tgc : BigInt  = 0,
                                tgd : BigInt  = 0){
  //override def toString : String = "Address\tData\tTGA\tTGC\tTGD\n%d\t%d\t%d\t%d\t%d".format(address,data,tga,tgc,tgd)
  def masked(mask : BigInt) : WishboneTransaction = this.copy(address = this.address & mask)

  def driveAsMaster(bus: Wishbone): Unit = {
    bus.ADR       #= address
    bus.DAT_MOSI  #= data
    if(bus.config.useTGA) bus.TGA       #= tga
    if(bus.config.useTGC) bus.TGC       #= tgc
    if(bus.config.useTGD) bus.TGD_MOSI  #= tgd
  }

  def driveAsSlave(bus: Wishbone): Unit = {
    bus.ADR       #= address
    bus.DAT_MISO  #= data
    if(bus.config.useTGA) bus.TGA       #= tga
    if(bus.config.useTGC) bus.TGC       #= tgc
    if(bus.config.useTGD) bus.TGD_MISO  #= tgd
  }

  def randomizeAddress(max : Int, min : Int = 0) : WishboneTransaction = this.copy(address = Random.nextInt(max - min) + min)
  def randomizeData(max : Int, min : Int = 0) : WishboneTransaction = this.copy(data = Random.nextInt(max - min) + min)
  def randomizeTGA(max : Int, min : Int = 0) : WishboneTransaction = this.copy(tga = Random.nextInt(max - min) + min)
  def randomizeTGC(max : Int, min : Int = 0) : WishboneTransaction = this.copy(tgc = Random.nextInt(max - min) + min)
  def randomizeTGD(max : Int, min : Int = 0) : WishboneTransaction = this.copy(tgd = Random.nextInt(max - min) + min)
}

object WishboneSequencer{
  //def apply(builder: () => Seq[WishboneTransaction]) = new WishboneSequencer(builder)
  def apply(builder: => Seq[WishboneTransaction]) = new WishboneSequencer(builder)
  //implicit def singleTransactionToList(transaction : WishboneTransaction): Seq[WishboneTransaction] = Seq(transaction)

}

class WishboneSequencer(builder: => Seq[WishboneTransaction]){
  val transactions = new scala.collection.mutable.Queue[Seq[WishboneTransaction]]()
  //val builder:() => WishboneTransaction


  def nextTransaction: Seq[WishboneTransaction] = transactions.dequeue()
  def addTransaction(transaction: Seq[WishboneTransaction]): Unit = transactions.enqueue(transaction)

  //def transactionBuilder(builder: () => WishboneTransaction): Unit = this.builder = builder
  def generateTransactions(number: Int = 1): Unit = for(i <- 0 to number) addTransaction(builder)

  def isEmpty: Boolean = transactions.isEmpty
}
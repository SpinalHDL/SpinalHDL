package spinal.lib.wishbone.sim

import scala.util.Random
import scala.collection.immutable._
import spinal.core.sim._
import spinal.lib.bus.wishbone._
import spinal.lib.bus.misc._

object WishboneStatus{
  def apply(bus: Wishbone) = new WishboneStatus(bus)
}

class WishboneStatus(bus: Wishbone){
  def isCycle   : Boolean = bus.CYC.toBoolean
  def isStall   : Boolean = if(bus.config.isPipelined)  isCycle && bus.STALL.toBoolean
                            else                        false
  def isTransfer: Boolean = if(bus.config.isPipelined)  isCycle && bus.STB.toBoolean && !bus.STALL.toBoolean
                            else                        isCycle && bus.STB.toBoolean

  def isAck     : Boolean = if(bus.config.isPipelined)  isCycle &&  bus.ACK.toBoolean
                            else                        isTransfer &&  bus.ACK.toBoolean

  def isWrite   : Boolean =                             isTransfer &&  bus.WE.toBoolean
  def isRead    : Boolean =                             isTransfer && !bus.WE.toBoolean
}

object AddressRange{
  implicit def SizeMapping2AddressRange(sizeMapping: SizeMapping): AddressRange = AddressRange(sizeMapping.base,sizeMapping.size.toInt)
}

case class AddressRange(base : BigInt, size: Int){
  def inRange(address: BigInt): Boolean = (address >= base) && (address <= base + size)
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
  def masked(mask : BigInt) : WishboneTransaction = this.copy(address = this.address & mask)

  def driveAsMaster(bus: Wishbone,we: Boolean = true): Unit = {
    bus.ADR       #= address
    bus.WE        #= we
    if(bus.config.useTGA) bus.TGA #= tga
    if(bus.config.useTGC) bus.TGC #= tgc
    if(we){
      bus.DAT_MOSI  #= data
      if(bus.config.useTGD) bus.TGD_MOSI #= tgd
    }
  }

  def driveAsSlave(bus: Wishbone): Unit = {
    bus.ADR       #= address
    bus.DAT_MISO  #= data
    if(bus.config.useTGA) bus.TGA       #= tga
    if(bus.config.useTGC) bus.TGC       #= tgc
    if(bus.config.useTGD) bus.TGD_MISO  #= tgd
  }

  def randomizeAddress(max : Int, min : Int = 0) : WishboneTransaction = this.copy(address = Random.nextInt(max - min) + min)
  def randomAdressInRange(range: AddressRange): WishboneTransaction = this.copy(address = range.randomAddressInRange)
  def randomizeData(max : Int, min : Int = 0) : WishboneTransaction = this.copy(data = Random.nextInt(max - min) + min)
  def randomizeTGA(max : Int, min : Int = 0) : WishboneTransaction = this.copy(tga = Random.nextInt(max - min) + min)
  def randomizeTGC(max : Int, min : Int = 0) : WishboneTransaction = this.copy(tgc = Random.nextInt(max - min) + min)
  def randomizeTGD(max : Int, min : Int = 0) : WishboneTransaction = this.copy(tgd = Random.nextInt(max - min) + min)
}
package spinal.lib.bus.amba3.ahblite.sim


import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba3.ahblite
import spinal.lib.bus.amba3.ahblite._
import spinal.lib.sim._
import spinal.sim._

import scala.collection.immutable.{List, Seq}
import scala.util.Random

object AhbLite3Transaction{

  implicit def singleToCycle(transaction : AhbLite3Transaction): Seq[AhbLite3Transaction] = List(transaction)


  def createSingleTransaction(n: Int): Seq[AhbLite3Transaction] = {
    for(_ <- 0 until n) yield AhbLite3Transaction(htrans = 2).randomizeData().randomizeAddress()
  }

  def createBurstTransaction(): Seq[AhbLite3Transaction] = {
    Seq(
      AhbLite3Transaction(htrans = 2, hburst = 3).randomizeAddress().randomizeData(),
      //AhbLite3Transaction(htrans = 1, hburst = 3).randomizeAddress().randomizeData(),
      AhbLite3Transaction(htrans = 3, hburst = 3).randomizeAddress().randomizeData(),
      AhbLite3Transaction(htrans = 3, hburst = 3).randomizeAddress().randomizeData(),
      AhbLite3Transaction(htrans = 3, hburst = 3).randomizeAddress().randomizeData()
    )
  }

  /**
    * Sample the bus as a Slave and create a Transaction
    */
  def sampleAsSlave(bus: AhbLite3, clockDomain: ClockDomain): AhbLite3Transaction = {

    /* Phase address */
    val transaction = AhbLite3Transaction(
      haddr    = bus.HADDR.toBigInt,
      htrans   = bus.HTRANS.toInt,
      hwrite   = bus.HWRITE.toBoolean,
      hburst   = bus.HBURST.toInt
    )

    clockDomain.waitActiveEdgeWhere(bus.HREADYOUT.toBoolean)

    /* Data phase */
    if(transaction.hwrite){
      transaction.copy(hwdata = bus.HWDATA.toBigInt)
    }else{
      transaction.copy(hrdata = bus.HWDATA.toBigInt)
    }

    /* Return the transaction */
    transaction
  }
}

case class AhbLite3Transaction(
    haddr  : BigInt  = 0,
    htrans : Int     = 0,
    hwrite : Boolean = false,
    hwdata : BigInt  = 0,
    hrdata : BigInt  = 0,
    hburst : Int     = 0,
    hsize  : Int     = 0
){

  def randomizeAddress() : AhbLite3Transaction = this.copy(haddr  = Random.nextInt(1000))
  def randomizeData()    : AhbLite3Transaction = this.copy(hwdata = Random.nextInt(1000))

  override def toString: String = {
    f"""
       |Transaction:
       |  HAddr  : ${haddr}%X
       |  HTrans : ${htrans}
       |  HWrite : ${hwrite}
       |  HWData : ${hwdata}%X
       |  HRData : ${hrdata}%X
     """.stripMargin
  }
}
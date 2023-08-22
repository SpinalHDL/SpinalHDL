package spinal.lib.misc.plic

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import scala.collection.Seq


case class PlicTarget(id : Int, gateways : Seq[PlicGateway], priorityWidth : Int) extends Area{
  assert(gateways.map(_.id).distinct.length == gateways.length, "PLIC gatways have duplicated ID")
  val ie = Vec.fill(gateways.length)(Bool())
  val threshold = UInt(priorityWidth bits)
  val idWidth = log2Up((gateways.map(_.id) ++ Seq(0)).max + 1)

  def Request(priority : UInt, id : UInt, valid : Bool) = {
    val ret = new Request
    ret.priority := priority
    ret.id := id
    ret.valid := valid
    ret
  }
  case class Request() extends Bundle{
    val priority = UInt(priorityWidth bits)
    val id = UInt(idWidth bits)
    val valid = Bool()
  }

  val requests = Request(U(0),U(0), True) +: gateways.zipWithIndex.sortBy(_._1.id).map(g =>
    Request(
      priority = g._1.priority,
      id       = U(g._1.id),
      valid    = g._1.ip && ie(g._2)
    )
  )

  val bestRequest = RegNext(requests.reduceBalancedTree((a, b) => {
    val takeA = !b.valid || (a.valid && a.priority >= b.priority)
    takeA ? a | b
  }))

  val iep = bestRequest.priority > threshold
  val claim = iep ? bestRequest.id | 0
}


//  def claim(id : UInt): Unit = {
//    switch(id){
//      for(gateway <- gateways){
//        is(gateway.id){
//          gateway.doClaim()
//        }
//      }
//    }
//  }
//  def driveFrom(bus : BusSlaveFactory, ieBase : BigInt, thresholdBase : BigInt): Unit ={
//
//  }
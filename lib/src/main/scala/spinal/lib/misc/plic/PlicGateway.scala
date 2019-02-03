package spinal.lib.misc.plic


import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory


abstract class PlicGateway(val id : Int, priorityWidth : Int) extends Area{
  val ip : Bool
  val priority = UInt(priorityWidth bits)
  def doClaim() : Unit
  def doCompletion() : Unit
  def driveFrom(bus : BusSlaveFactory, offset : Int) : Unit
}

case class PlicGatewayActiveHigh(source : Bool,override val id : Int, priorityWidth : Int) extends PlicGateway(id = id, priorityWidth = priorityWidth){
  val ip = RegInit(False)
  val waitCompletion = RegInit(False)

  when(!waitCompletion){
    ip := source
    waitCompletion := source
  }
  override def doClaim(): Unit = ip := False
  override def doCompletion(): Unit = waitCompletion := False
  override def driveFrom(bus: BusSlaveFactory, offset: Int): Unit = {}
}

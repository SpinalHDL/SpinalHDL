package spinal.lib.bus.amba4.apb.sim

import spinal.core.ClockDomain
import spinal.core.sim._
import spinal.lib.bus.amba4.apb.Apb4
import spinal.lib._

import scala.util.Random

abstract case class Apb4Monitor(apb : Apb4, clockDomain : ClockDomain) {
  var state = 0
  var pselAsserted, penableAsserted = false
  clockDomain.onSamplings{
    apb.PRDATA.randomize()
    apb.PSLVERR.randomize()
    if(pselAsserted) assert(apb.PSEL.toInt == 1)
    if(penableAsserted) assert(apb.PENABLE.toBoolean)

    def updateRsp(): Unit ={
      if(Random.nextBoolean()){
        apb.PREADY #= true
        apb.PSLVERR #= false
        val address = apb.PADDR.toBigInt
        if(apb.PWRITE.toBoolean){
          val bytes = apb.PWDATA.toBigInt.toByteArray.reverse
          val strb  = apb.PSTRB.toInt.toBinInts()
          val prot  = apb.PPROT.toInt
          for(i <- 0 until apb.c.dataWidth/8){
            onWrite(address + i, if(bytes.length > i) bytes(i)*strb(i) else 0, prot)
          }
        } else {
          val bytes = new Array[Byte](apb.c.dataWidth/8 + 1)
          for(i <- 0 until bytes.length-1){
            bytes(i+1) = onRead(address + apb.c.dataWidth/8 - 1  - i)
          }
          apb.PRDATA #= BigInt(bytes)
        }
      } else {
        apb.PREADY #= false
      }
    }

    state match {
      case 0 => {
        assert(apb.PENABLE.toBoolean == false)
        apb.PREADY.randomize()
        if(apb.PSEL.toInt == 1){
          pselAsserted = true
          state = 1
          updateRsp()
        }
      }
      case 1 => {
        if(apb.PENABLE.toBoolean){
          penableAsserted = true
          if(apb.PREADY.toBoolean){
            state = 0
            pselAsserted = false
            penableAsserted = false
          } else {
            updateRsp()
          }
        }
      }
    }
  }

  def onRead(address : BigInt) : Byte
  def onWrite(address : BigInt, value : BigInt, prot: BigInt) : Unit
}


abstract case class Apb4Listener(apb : Apb4, clockDomain : ClockDomain) {
  clockDomain.onSamplings {
    if(apb.PENABLE.toBoolean && apb.PSEL.toBigInt != 0){
      if(apb.PWRITE.toBoolean){
        onWrite(apb.PADDR.toBigInt, apb.PWDATA.toBigInt, apb.PSTRB.toBigInt, apb.PPROT.toInt)
      } else {
        onRead(apb.PADDR.toBigInt, apb.PPROT.toInt)
      }
    }
  }
  def onRead(address : BigInt, prot: Int) : Unit
  def onWrite(address : BigInt, value : BigInt, strb: BigInt, prot: Int) : Unit
}
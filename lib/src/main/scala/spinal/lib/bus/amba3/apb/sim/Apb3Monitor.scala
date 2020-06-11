package spinal.lib.bus.amba3.apb.sim

import spinal.core.ClockDomain
import spinal.core.sim._
import spinal.lib.bus.amba3.apb.Apb3

import scala.util.Random

abstract case class Apb3Monitor(apb : Apb3, clockDomain : ClockDomain) {
  var state = 0
  var pselAsserted, penableAsserted = false
  clockDomain.onSamplings{
    apb.PRDATA.randomize()
    apb.PSLVERROR.randomize()
    if(pselAsserted) assert(apb.PSEL.toInt == 1)
    if(penableAsserted) assert(apb.PENABLE.toBoolean)

    def updateRsp(): Unit ={
      if(Random.nextBoolean()){
        apb.PREADY #= true
        apb.PSLVERROR #= false
        val address = apb.PADDR.toBigInt
        if(apb.PWRITE.toBoolean){
          val bytes = apb.PWDATA.toBigInt.toByteArray.reverse
          for(i <- 0 until apb.config.dataWidth/8){
            onWrite(address + i, if(bytes.length > i) bytes(i) else 0)
          }
        } else {
          val bytes = new Array[Byte](apb.config.dataWidth/8 + 1)
          for(i <- 0 until bytes.length-1){
            bytes(i+1) = onRead(address + apb.config.dataWidth/8 - 1  - i)
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
  def onWrite(address : BigInt, value : Byte) : Unit
}


abstract case class Apb3Listener(apb : Apb3, clockDomain : ClockDomain) {
  clockDomain.onSamplings {
    if(apb.PENABLE.toBoolean && apb.PSEL.toBigInt != 0){
      if(apb.PWRITE.toBoolean){
        onWrite(apb.PADDR.toBigInt, apb.PWDATA.toBigInt)
      } else {
        onRead(apb.PADDR.toBigInt)
      }
    }
  }
  def onRead(address : BigInt) : Unit
  def onWrite(address : BigInt, value : BigInt) : Unit
}
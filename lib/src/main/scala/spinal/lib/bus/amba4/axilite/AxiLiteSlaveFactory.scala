package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer


trait AxiLiteSlaveFactoryElement

case class AxiLiteSlaveFactoryRead(that : Data,
                                   address : BigInt,
                                   bitOffset : Int,
                                   onRsp :  (BigInt) => Unit) extends AxiLiteSlaveFactoryElement
case class AxiLiteSlaveFactoryWrite(that : Data,
                                    address : BigInt,
                                    bitOffset : Int,
                                    onCmd : (BigInt) => Unit) extends AxiLiteSlaveFactoryElement

class AxiLiteSlaveFactory(bus : AxiLite) extends Area{
  val elements = ArrayBuffer[AxiLiteSlaveFactoryElement]()

  def read(that : Data,
           address : BigInt,
           bitOffset : Int = null.asInstanceOf[Int],
           onRsp :  (BigInt) => Unit = (x) => {}) : Unit  = {
    elements += AxiLiteSlaveFactoryRead(that,address,bitOffset,onRsp)
  }
  def write(that : Data,
            address : BigInt,
            bitOffset : Int = null.asInstanceOf[Int],
            onCmd : (BigInt) => Unit = (x) => {}) : Unit  = {
    elements += AxiLiteSlaveFactoryWrite(that,address,bitOffset,onCmd)
  }

  def writeOnlyRegOf[T <: Data](dataType: T, baseAddress: BigInt): T = {
    val ret = Reg(dataType)
    write(ret,baseAddress)
    ret
  }
  def writeReadRegOf[T <: Data](that: T, baseAddress: BigInt): T = {
    val reg = Reg(that)
    write(reg,baseAddress)
    read(reg,baseAddress)
    reg
  }
  def bitsCumulateAndClearOnRead(that : Bits,base : BigInt): Unit ={
    assert(that.getWidth <= bus.config.dataWidth)
    val reg = Reg(that.clone)
    reg := reg | that
    read(reg,base,onRsp = (address) => {
      reg := that
    })
  }

  val writeJoinEvent = StreamJoin(bus.writeCmd,bus.writeData)
  val writeRsp = AxiLiteB(bus.config)
  bus.writeRet <-< writeJoinEvent.translateWith(writeRsp)

  val readDataStage = bus.readCmd.stage()
  val readRsp = AxiLiteR(bus.config)
  bus.readData << readDataStage.translateWith(readRsp)


  component.addPrePopTask(() => {
    //writes
    //TODO writeRsp.resp := OKAY
    when(writeJoinEvent.valid){
      for(e <- elements) e match{
        case e : AxiLiteSlaveFactoryWrite => {
          if(e.bitOffset != null){
            assert(e.bitOffset + e.that.getBitsWidth <= bus.config.dataWidth)
            when(bus.writeCmd.addr === e.address) {
              e.that.assignFromBits(bus.writeData.data(e.bitOffset,e.that.getBitsWidth bits))
              when(writeJoinEvent.ready) {
                e.onCmd(0)
              }
            }
          }else {
            val wordCount = (widthOf(e.that) - 1) / bus.config.dataWidth + 1
            for (wordId <- (0 until wordCount)) {
              when(bus.writeCmd.addr === e.address + wordId * bus.config.dataWidth / 8) {
                e.that.assignFromBits(bus.writeData.data.resized, wordId * bus.config.dataWidth, Math.min(bus.config.dataWidth, widthOf(e.that) - wordId * bus.config.dataWidth) bit)
                when(writeJoinEvent.ready) {
                  e.onCmd(wordId)
                }
              }
            }
          }
        }
      }
    }


    //Reads
    //TODO readRsp.resp := OKEY
    readRsp.data := 0
    for(e <- elements) e match{
      case e : AxiLiteSlaveFactoryRead => {
        if(e.bitOffset == null){
          assert(e.bitOffset + e.that.getBitsWidth <= bus.config.dataWidth)
          when(readDataStage.addr === e.address) {
            readRsp.data(e.bitOffset,e.that.getBitsWidth bits) := e.that.asBits
            when(bus.readData.fire) {
              e.onRsp(0)
            }
          }
        }else{
          val wordCount = (widthOf(e.that) - 1) / bus.config.dataWidth + 1
          val valueBits = e.that.asBits.resize(wordCount*bus.config.dataWidth)
          val words = (0 until wordCount).map(id => valueBits(id * bus.config.dataWidth , bus.config.dataWidth bit))
          for (wordId <- (0 until wordCount)) {
            when(readDataStage.addr === e.address + wordId*bus.config.dataWidth/8) {
              readRsp.data  := words(wordId).resized
              when(bus.readData.fire) {
                e.onRsp(wordId)
              }
            }
          }
        }
      }
    }
  })
}

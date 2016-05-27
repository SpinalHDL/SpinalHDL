package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer


trait AxiLiteSlaveFactoryElement

case class AxiLiteSlaveFactoryRead(that : Data,base : BigInt) extends AxiLiteSlaveFactoryElement
case class AxiLiteSlaveFactoryWrite(that : Data,base : BigInt) extends AxiLiteSlaveFactoryElement



class AxiLiteSlaveFactory(bus : AxiLite) extends Area{
  val elements = ArrayBuffer[AxiLiteSlaveFactoryElement]()

  def read(that : Data,address : BigInt) : Unit  = elements += AxiLiteSlaveFactoryRead(that,address)
  def write(that : Data,address : BigInt) : Unit = elements += AxiLiteSlaveFactoryWrite(that,address)
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
          assert(e.that.isReg,"reg argument must be a Reg")
          val wordCount = (widthOf(e.that) - 1) / bus.config.dataWidth + 1
          for (wordId <- (0 until wordCount)) {
            when(bus.writeCmd.addr === e.base + wordId * bus.config.dataWidth / 8) {
              e.that.assignFromBits(bus.writeData.data.resized,wordId * bus.config.dataWidth, Math.min(bus.config.dataWidth, widthOf(e.that) - wordId * bus.config.dataWidth) bit)
            }
          }
        }
      }
    }


    //Reads
    //TODO readRsp.resp := OKEY
    for(e <- elements) e match{
      case e : AxiLiteSlaveFactoryRead => {
        val wordCount = (widthOf(e.that) - 1) / bus.config.dataWidth + 1
        val valueBits = e.that.asBits.resize(wordCount*bus.config.dataWidth)
        val words = (0 until wordCount).map(id => valueBits(id * bus.config.dataWidth , bus.config.dataWidth bit))
        for (wordId <- (0 until wordCount)) {
          when(readDataStage.addr === e.base + wordId*bus.config.dataWidth/8) {
            readRsp.data  := words(wordId).resized
          }
        }
      }
    }
  })
}

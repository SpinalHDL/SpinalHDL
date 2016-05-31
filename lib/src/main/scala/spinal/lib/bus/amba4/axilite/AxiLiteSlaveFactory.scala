package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer


trait AxiLiteSlaveFactoryElement

case class AxiLiteSlaveFactoryRead(that : Data,
                                   address : BigInt,
                                   bitOffset : Int,
                                   onRsp :  () => Unit) extends AxiLiteSlaveFactoryElement
case class AxiLiteSlaveFactoryWrite(that : Data,
                                    address : BigInt,
                                    bitOffset : Int,
                                    onCmd : () => Unit) extends AxiLiteSlaveFactoryElement

class AxiLiteSlaveFactory(bus : AxiLite) extends Area{
  val elements = ArrayBuffer[AxiLiteSlaveFactoryElement]()

  def read(that : Data,
             address : BigInt,
             bitOffset : Int = 0,
             onRsp :  () => Unit = () => {}) : Unit  = {
    elements += AxiLiteSlaveFactoryRead(that,address,bitOffset,onRsp)
  }
  def write(that : Data,
              address : BigInt,
              bitOffset : Int = 0,
              onCmd : () => Unit = () => {}) : Unit  = {
    elements += AxiLiteSlaveFactoryWrite(that,address,bitOffset,onCmd)
  }


  def multiRead(that : Data,
                address : BigInt,
                onRsp :  (BigInt) => Unit = (x) => {}) : Unit  = {
    val wordCount = (widthOf(that) - 1) / bus.config.dataWidth + 1
    val valueBits = that.asBits.resize(wordCount*bus.config.dataWidth)
    val words = (0 until wordCount).map(id => valueBits(id * bus.config.dataWidth , bus.config.dataWidth bit))
    for (wordId <- (0 until wordCount)) {
      read(words(wordId), address + wordId*bus.config.dataWidth/8,onRsp = () => onRsp(wordId))
    }
  }

  def multiWrite(that : Data,
                 address : BigInt,
                 onCmd : (BigInt) => Unit = (x) => {}) : Unit  = {
    val wordCount = (widthOf(that) - 1) / bus.config.dataWidth + 1
    for (wordId <- (0 until wordCount)) {
      write(
        that = new DataWrapper{
          override def getBitsWidth: Int =
            Math.min(bus.config.dataWidth, widthOf(that) - wordId * bus.config.dataWidth)

          override def assignFromBits(value : Bits): Unit = {
            that.assignFromBits(
              bits     = value.resized,
              offset   = wordId * bus.config.dataWidth,
              bitCount = getBitsWidth bits)
          }
        },
        address = address + wordId * bus.config.dataWidth / 8,0,
        onCmd = () => onCmd(wordId))
    }
  }

  def writeOnlyRegOf[T <: Data](dataType: T, baseAddress: BigInt,bitOffset : Int = 0): T = {
    val ret = Reg(dataType)
    write(ret,baseAddress,bitOffset)
    ret
  }
  def writeReadRegOf[T <: Data](that: T, baseAddress: BigInt,bitOffset : Int = 0): T = {
    val reg = Reg(that)
    write(reg,baseAddress,bitOffset)
    read(reg,baseAddress,bitOffset)
    reg
  }
  def bitsCumulateAndClearOnRead(that : Bits,base : BigInt,bitOffset : Int = 0): Unit ={
    assert(that.getWidth <= bus.config.dataWidth)
    val reg = Reg(that.clone)
    reg := reg | that
    read(reg,base,bitOffset,onRsp = () => {
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
          assert(e.bitOffset + e.that.getBitsWidth <= bus.config.dataWidth)
          when(bus.writeCmd.addr === e.address) {
            e.that.assignFromBits(bus.writeData.data(e.bitOffset,e.that.getBitsWidth bits))
            when(writeJoinEvent.ready) {
              e.onCmd()
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
        assert(e.bitOffset + e.that.getBitsWidth <= bus.config.dataWidth)
        when(readDataStage.addr === e.address) {
          readRsp.data(e.bitOffset,e.that.getBitsWidth bits) := e.that.asBits
          when(bus.readData.fire) {
            e.onRsp()
          }
        }
      }
    }
  })
}

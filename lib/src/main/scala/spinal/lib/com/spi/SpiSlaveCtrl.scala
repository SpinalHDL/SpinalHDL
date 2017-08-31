package spinal.lib.com.spi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
import spinal.lib.bus.misc.BusSlaveFactory

/**
 * Created by PIC32F_USER on 02/08/2017.
 */

case class SpiSlaveCtrlGenerics(dataWidth : Int = 8){
}

case class SpiSlaveCtrlIo(generics : SpiSlaveCtrlGenerics) extends Bundle{
  import generics._
  val kind = in(SpiKind())
  val rx = master Flow(Bits(dataWidth bits))
  val tx = slave Stream(Bits(dataWidth bits))
  val txError = out Bool
  val ssFilted = out Bool
  val spi = master(SpiSlave())

  def driveFrom(bus : BusSlaveFactory)(cmdFifoSize : Int, rspFifoSize : Int) = new Area {
    //TX
    val txLogic = new Area {
      val streamUnbuffered = bus.createAndDriveFlow(Bits(8 bits),address =  0).toStream
      val (stream, fifoAvailability) = streamUnbuffered.queueWithAvailability(cmdFifoSize)
      tx << stream
      bus.read(fifoAvailability, address = 4, 16)
    }

    //RX
    val rxLogic = new Area {
      val listen = bus.createReadAndWrite(Bool, address = 4, bitOffset = 15)
      val (stream, fifoOccupancy) = rx.takeWhen(listen).queueWithOccupancy(rspFifoSize)
      bus.readStreamNonBlocking(stream, address = 0, validBitOffset = 31, payloadBitOffset = 0)
      bus.read(fifoOccupancy, address = 0, 16)
    }

    //Status
    val interruptCtrl = new Area {
      val txIntEnable = bus.createReadAndWrite(Bool, address = 4, 0) init(False)
      val rxIntEnable = bus.createReadAndWrite(Bool, address = 4, 1) init(False)
      val ssLowIntEnable = bus.createReadAndWrite(Bool, address = 4, 2) init(False)
      val ssHighIntEnable = bus.createReadAndWrite(Bool, address = 4, 3) init(False)
      val txInt  = txIntEnable & !txLogic.stream.valid
      val rxInt   = rxIntEnable & rxLogic.stream.valid
      val ssLowInt = ssLowIntEnable & ssFilted
      val ssHighInt = ssHighIntEnable & ssFilted
      val interrupt = rxInt || txInt
      bus.read(txInt, address = 4, 8)
      bus.read(rxInt , address = 4, 9)
      bus.read(ssLowInt, address = 4, 10)
      bus.read(ssHighInt , address = 4, 11)
    }

    //Configs
    bus.drive(kind, 8)
  }
}

case class SpiSlaveCtrl(generics : SpiSlaveCtrlGenerics) extends Component{
  import generics._

  val io = SpiSlaveCtrlIo(generics)

  //Input filter
  val spi = io.spi.slaveResync()
  val normalizedSclkEdges = (spi.sclk ^ io.kind.cpol ^ io.kind.cpha).edges()

  //FSM
  val counter = Counter(dataWidth*2)
  val buffer = Reg(Bits(dataWidth bits))

  when(spi.ss){
    counter.clear()
  } otherwise {
    when(normalizedSclkEdges.rise){
      buffer := spi.mosi ## (buffer >> 1)
    }
    when(normalizedSclkEdges.toogle){
      counter.increment()
    }
  }

  //IO
  io.ssFilted := spi.ss

  io.rx.valid := RegNext(counter.willOverflow)
  io.rx.payload := buffer

  io.tx.ready := counter.willOverflow || spi.ss
  io.txError := io.tx.ready && !io.tx.valid

  val rspBit = io.tx.payload(dataWidth - 1 - (counter >> 1))
  val rspBitSampled = RegNextWhen(rspBit, normalizedSclkEdges.fall)
  spi.miso.writeEnable := !spi.ss
  spi.miso.write := io.kind.cpha ? rspBitSampled | rspBit
}

object SpiSlaveCtrl{
  def main(args: Array[String]) {
    SpinalVerilog({
      new Component{
        val ctrl = new SpiSlaveCtrl(SpiSlaveCtrlGenerics(8))
        val factory = Apb3SlaveFactory(slave(Apb3(8,32)))
        ctrl.io.driveFrom(factory)(cmdFifoSize = 32, rspFifoSize = 32)
        master(cloneOf(ctrl.io.spi)) <> ctrl.io.spi
      }
//      ctrl
    }.setDefinitionName("TopLevel"))
   // SpinalVerilog(new SpiMaster(SpiMasterGenerics(2,0,16)).setDefinitionName("TopLevelV"))
  }
}
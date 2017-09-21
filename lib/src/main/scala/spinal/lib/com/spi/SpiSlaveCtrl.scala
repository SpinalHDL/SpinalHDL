package spinal.lib.com.spi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
import spinal.lib.bus.misc.{BusSlaveFactoryAddressWrapper, BusSlaveFactory}

/**
 * Created by PIC32F_USER on 02/08/2017.
 */




case class SpiSlaveCtrlGenerics(dataWidth : Int = 8){
}

case class SpiSlaveCtrlMemoryMappedConfig(ctrlGenerics : SpiSlaveCtrlGenerics,
                                          rxFifoDepth : Int = 32,
                                          txFifoDepth : Int = 32)

case class SpiSlaveCtrlIo(generics : SpiSlaveCtrlGenerics) extends Bundle{
  import generics._
  val kind = in(SpiKind())
  val rx = master Flow(Bits(dataWidth bits))
  val tx = slave Stream(Bits(dataWidth bits))
  val txError = out Bool
  val ssFilted = out Bool
  val spi = master(SpiSlave())

  def driveFrom(bus : BusSlaveFactory, baseAddress : BigInt)(generics : SpiSlaveCtrlMemoryMappedConfig) = new Area {
    import generics._
    require(rxFifoDepth >= 1)
    require(txFifoDepth >= 1)

    require(rxFifoDepth < 32.kB)
    require(txFifoDepth < 32.kB)

    val busWithOffset = new BusSlaveFactoryAddressWrapper(bus, baseAddress)

    //TX
    val txLogic = new Area {
      val streamUnbuffered = busWithOffset.createAndDriveFlow(Bits(8 bits),address =  0).toStream
      val (stream, fifoAvailability) = streamUnbuffered.queueWithAvailability(rxFifoDepth)
      tx << stream
      busWithOffset.read(fifoAvailability, address = 4, 16)
    }

    //RX
    val rxLogic = new Area {
      val listen = busWithOffset.createReadAndWrite(Bool, address = 4, bitOffset = 15) init(False)
      val (stream, fifoOccupancy) = rx.takeWhen(listen).queueWithOccupancy(txFifoDepth)
      busWithOffset.readStreamNonBlocking(stream, address = 0, validBitOffset = 31, payloadBitOffset = 0)
      busWithOffset.read(fifoOccupancy, address = 0, 16)
    }

    //Status
    val interruptCtrl = new Area {
      val txIntEnable = busWithOffset.createReadAndWrite(Bool, address = 4, 0) init(False)
      val rxIntEnable = busWithOffset.createReadAndWrite(Bool, address = 4, 1) init(False)
      val ssLowIntEnable = busWithOffset.createReadAndWrite(Bool, address = 4, 2) init(False)
      val ssHighIntEnable = busWithOffset.createReadAndWrite(Bool, address = 4, 3) init(False)
      val txInt  = txIntEnable & !txLogic.stream.valid
      val rxInt   = rxIntEnable & rxLogic.stream.valid
      val ssLowInt = ssLowIntEnable & ssFilted
      val ssHighInt = ssHighIntEnable & ssFilted
      val interrupt = rxInt || txInt
      busWithOffset.read(txInt, address = 4, 8)
      busWithOffset.read(rxInt , address = 4, 9)
      busWithOffset.read(ssLowInt, address = 4, 10)
      busWithOffset.read(ssHighInt , address = 4, 11)
    }

    //Configs
    busWithOffset.drive(kind, 8)
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
      buffer := (buffer ## spi.mosi).resized
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
        ctrl.io.driveFrom(factory, 0)(SpiSlaveCtrlMemoryMappedConfig(SpiSlaveCtrlGenerics(8)))
        master(cloneOf(ctrl.io.spi)) <> ctrl.io.spi
      }
//      ctrl
    }.setDefinitionName("TopLevel"))
   // SpinalVerilog(new SpiMaster(SpiMasterGenerics(2,0,16)).setDefinitionName("TopLevelV"))
  }
}
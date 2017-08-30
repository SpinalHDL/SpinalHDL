//package spinal.lib.com.spi
//
//import spinal.core._
//import spinal.lib._
//import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
//import spinal.lib.bus.misc.BusSlaveFactory
//
///**
// * Created by PIC32F_USER on 02/08/2017.
// */
//
//case class SpiSlaveGenerics( sioCount : Int,
//                             dataWidth : Int = 8){
//  require(isPow2(sioCount))
//}
//
//case class SpiSlaveConfig(generics : SpiSlaveGenerics) extends Bundle{
//  val halfDuplex = Bool
//  val sioUsage = UInt(log2Up(log2Up(generics.sioCount) + 1) bits)
//  val kind = SpiKind()
//}
//
////object SpiMasterCmdMode extends SpinalEnum(binarySequential){
////  val DATA, SS = newElement()
////}
////
////case class SpiMasterCmdData(generics : SpiMasterGenerics) extends Bundle{
////  val data = Bits(generics.dataWidth bits)
////  val read = Bool
////  val halfDuplex = Bool
////  val sioUsage = UInt(log2Up(log2Up(generics.sioCount) + 1) bits)
////}
////
////case class SpiMasterCmdSs(generics : SpiMasterGenerics) extends Bundle{
////  val enable = Bool
////  val index = UInt(log2Up(generics.ssWidth) bits)
////}
////
////case class SpiMasterCmd(generics : SpiMasterGenerics) extends Bundle{
////  val mode = if(generics.ssGen) SpiMasterCmdMode() else null
////  val args = Bits(Math.max(widthOf(SpiMasterCmdData(generics)), log2Up(generics.ssWidth) + 1 ) bits)
////
////  def isData = if(generics.ssGen) mode === SpiMasterCmdMode.DATA else True
////
////  def argsData = {
////    val ret = SpiMasterCmdData(generics)
////    ret.assignFromBits(args)
////    ret
////  }
////  def argsSs = {
////    val ret = SpiMasterCmdSs(generics)
////    ret.assignFromBits(args)
////    ret
////  }
////}
//
//case class SpiSlave(generics : SpiSlaveGenerics) extends Component{
//  import generics._
//
//  require(dataWidth % sioWidth == 0)
//
//  val io = new Bundle {
//    val config = in(SpiSlaveConfig(generics))
//    val cmd = master Flow(dataWidth bits)
//    val rsp = slave Stream(Bits(dataWidth bits))
//    val overrun = out Bool
//    val spiSio = slave(SpiSio(sioCount, 1))
//  }
//
//
//
//
//  val correctedClk = io.spiSio.sclk ^ io.config.kind.cpol
//  val samplingCd = ClockDomain(clock = correctedClk)
//  val drivingCd = ClockDomain(clock = correctedClk, config = ClockDomainConfig(clockEdge = FALLING))
//  drivingCd.isSyncronousWith(samplingCd)
//
//
//  val sampling = new ClockingArea(samplingCd){
//
//  }
//
//
//  val driving = new ClockingArea(drivingCd)(new Area{
//
//  })
//
//
//
//  when(io.spiSio.ss.lsb){
//    counter := 0
//  } otherwise {
//
//  }
//
//
//}
//
////case class SpiIoSlave(generics : SpiSlaveGenerics) extends Component{
////  import generics._
////
////  val io = new Bundle {
////    val config = in(SpiSlaveConfig(generics))
////    val spiSio = slave(SpiSio(sioCount, 1, useSclk = false))
////    val cmd = master Flow(sioCount bits)
////    val rsp = slave Stream(Bits(sioCount bits))
////    val overrun = out Bool
////  }
////
//////  val sampleClockDomain
////  when(!io.spiSio.ss.lsb){
////
////  }
////
////
////}
//
//
////object SpiSlave{
////  def main(args: Array[String]) {
////    SpinalVerilog({
////      new Component{
////        val ctrl = new SpiMaster(SpiMasterGenerics(4,8,16)).setDefinitionName("TopLevelV")
////        val factory = Apb3SlaveFactory(slave(Apb3(8,32)))
////        ctrl.io.driveFrom(factory)(cmdFifoSize = 32, rspFifoSize = 32)
////        master(cloneOf(ctrl.io.spiSio)) <> ctrl.io.spiSio
////      }
////    })
////   // SpinalVerilog(new SpiMaster(SpiMasterGenerics(2,0,16)).setDefinitionName("TopLevelV"))
////  }
////}
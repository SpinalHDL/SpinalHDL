package spinal.lib.memory.sdram.xdr.Dfi.CtrlWithBmb

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.xdr.Dfi.Interface._
import spinal.lib.memory.sdram.xdr.Dfi.Alignment._
import spinal.lib.memory.sdram.xdr.Dfi.PHY.Initialize
//import _root_.CtrlWithBmb.
case class DfiAlignment(cpp : CorePortParameter, cpa : CoreParameterAggregate/*, initP : BmbParameter*/) extends Component{
  import cpa._
  val io = new Bundle{
    val inCoreport = slave(CorePort(cpp,cpa))
    val outDfiport = master(Dfi(config))
    val refresh = out Bool()
//    val ctrl = slave(Bmb(initP))
    val initDone = out Bool()
  }
  val ctrlAlignment = CtrlAlignment(cpp, cpa)
  ctrlAlignment.io.inport <> io.inCoreport
  ctrlAlignment.io.refresh <> io.refresh
  ctrlAlignment.io.outport.rden <> io.outDfiport.read.rden

  val initialize = Initialize(cpa)
  initialize.io.initDone <> io.initDone
//  val mapper = BmbSlaveFactory(io.cfg)


  val caAlignment = CAAlignment(config)
  caAlignment.io.ckeN := initialize.io.ckeN.subdivideIn(config.frequencyRatio slices)
  caAlignment.io.cmd <> Mux(initialize.io.initDone,ctrlAlignment.io.outport.cmd,initialize.io.cmd)
  caAlignment.io.address <> Mux(initialize.io.initDone,ctrlAlignment.io.outport.address,initialize.io.address)
  caAlignment.io.output <> io.outDfiport.control
//  caAlignment.io.ckeN.clearAll()
//  caAlignment.io.cmd <> ctrlAlignment.io.outport.cmd
//  caAlignment.io.address <> ctrlAlignment.io.outport.address
//  caAlignment.io.output <> io.outDfiport.control
//  caAlignment.io.initBus.driveFrom(BmbSlaveFactory(io.ctrl).withOffset(0x000))

  val wrAlignment = WrAlignment(config)
  wrAlignment.io.wrdata <> ctrlAlignment.io.outport.wrdata
  if(config.useWrdataCsN)wrAlignment.io.wrcs <> ctrlAlignment.io.outport.wrcs
  wrAlignment.io.output <> io.outDfiport.write

  val rdAlignment = RdAlignment(config)
  rdAlignment.io.rddata <> ctrlAlignment.io.outport.rddata
  if(config.useRddataCsN)rdAlignment.io.rdcs <> ctrlAlignment.io.outport.rdcs
  rdAlignment.io.rd <> io.outDfiport.read.rd
  if(config.useRddataCsN)rdAlignment.io.rdCs <> io.outDfiport.read.rdCs
  rdAlignment.io.phaseclear := False

//  io.outDfiport.control.cke.clearAll()
}

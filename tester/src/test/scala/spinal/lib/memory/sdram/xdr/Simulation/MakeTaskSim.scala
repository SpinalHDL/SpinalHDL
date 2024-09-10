package spinal.lib.memory.sdram.xdr.Simulation

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.memory.sdram.Dfi.CtrlWithBmb._
import spinal.lib.memory.sdram.Dfi.Interface._
import spinal.lib.memory.sdram.Dfi.Tools._

case class MakeTaskSim(x:Int) extends Component{
//  val myclock = in Bool()
//  val myrstn = in Bool()
  val bmbclockDomain = ClockDomain(ClockDomain.current.clock,ClockDomain.current.reset,config=ClockDomainConfig(resetActiveLevel = HIGH))
//  val bmbclockDomain = ClockDomain.current
  val core:CoreParameter = CoreParameter(timingWidth=3,refWidth=7)
  val sdramtime = SdramTiming(3, RFC = 260, RAS = 38, RP = 15, RCD = 15, WTR = 8, WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35)
  val sdram = SdramLayout(SdramGeneration.MYDDR,bankWidth=2,columnWidth=9,rowWidth=12,dataWidth=8,ddrMHZ=100,ddrWrLat=4,ddrRdLat=4,sdramtime=sdramtime)
  val pl:PhyLayout = PhyLayout(sdram = sdram, phaseCount=4,dataRate=SdramGeneration.MYDDR.dataRate,0,0,0,0,transferPerBurst=8)
  val timeConfig = DfiTimeConfig(tPhyWrLat=4,tPhyWrData=0,tPhyWrCsGap=3,dramBurst=pl.transferPerBurst,frequencyRatio=pl.phaseCount,tRddataEn=4,tPhyRdlat=4,tPhyRdCsGap=3)
  val config:DfiConfig = DfiConfig(frequencyRatio=pl.phaseCount,dramAddrWidth=Math.max(pl.sdram.columnWidth,pl.sdram.rowWidth),dramDataWidth=pl.phyIoWidth,
    dramChipselectNumber=2,dramBankWidth=pl.sdram.bankWidth,0,0,1,cmdPhase=0,ddr=DDR(),timeConfig=timeConfig)
  val bmbp:BmbParameter = BmbParameter(addressWidth=pl.sdram.byteAddressWidth+log2Up(config.chipSelectNumber),dataWidth=pl.beatWidth,
    sourceWidth=1,contextWidth=2,lengthWidth=6,alignment= BmbParameter.BurstAlignement.WORD)
  val bmbpp:BmbPortParameter = BmbPortParameter(bmbp,bmbclockDomain,cmdBufferSize=16,dataBufferSize=16,rspBufferSize=128)
  val ctp : CtrlParameter = CtrlParameter(core, bmbpp)
//  ClockDomain.current.clock
  val cpa = CoreParameterAggregate(ctp.core, pl, BmbAdapter.corePortParameter(ctp.port, pl), config)
  val io = new Bundle {
    val bmb = slave(Bmb(ctp.port.bmb))
    val output = master(CoreTasks(cpa))
  }
//  val coreconfig = CoreConfig(cpa)

//  coreconfig.

  val bmbAdapter =  BmbAdapter(ctp.port, cpa)
  bmbAdapter.io.input <>  io.bmb
  bmbAdapter.io.output.writeData.ready := True
  bmbAdapter.io.output.rsp.valid := False
  bmbAdapter.io.output.rsp.last := True
  bmbAdapter.io.output.rsp.data.assignDontCare()
  bmbAdapter.io.output.rsp.context.assignDontCare()

  val maketask = MakeTask(cpa.cpp,cpa)
//  maketask.io.config := coreconfig
  maketask.io.cmd << bmbAdapter.io.output.cmd
  maketask.io.writeDataTockens <> bmbAdapter.io.output.writeDataTocken
  maketask.io.output <> io.output

  val refresher = Refresher(cpa)
//  refresher.io.config := coreconfig
  bmbAdapter.io.refresh <> refresher.io.refresh.valid
  maketask.io.refresh <> refresher.io.refresh
//  io.refresh := refresher.io.refresh.valid

}
object MakeTaskSim {
  def main(args: Array[String]): Unit = {
//    val myclock = Bool()
//    val myrstn = Bool()
//    val bmbclockDomain = ClockDomain(myclock,myrstn,config=ClockDomainConfig(resetActiveLevel = LOW))
//    val core:CoreParameter = CoreParameter(timingWidth=3,refWidth=3)
//    val pl:PhyLayout = PhyLayout(sdram = SdramLayout(SdramGeneration.MYDDR,bankWidth=2,columnWidth=9,rowWidth=12,dataWidth=8),
//      phaseCount=4,dataRate=SdramGeneration.MYDDR.dataRate,0,0,0,0,transferPerBurst=8)
//    val timeConfig = DfiTimeConfig(tPhyWrLat=4,tPhyWrData=0,tPhyWrCsGap=3,dramBurst=pl.transferPerBurst,frequencyRatio=pl.phaseCount,tRddataEn=4,tPhyRdlat=4,tPhyRdCsGap=3)
//    val config:DfiConfig = DfiConfig(frequencyRatio=pl.phaseCount,dramAddrWidth=Math.max(pl.sdram.columnWidth,pl.sdram.rowWidth),dramDataWidth=pl.phyIoWidth,
//      dramChipselectNumber=2,dramBankWidth=pl.sdram.bankWidth,0,0,1,cmdPhase=0,ddr=DDR(),timeConfig=timeConfig)
//    val bmbp:BmbParameter = BmbParameter(addressWidth=pl.sdram.byteAddressWidth+log2Up(config.chipSelectNumber),dataWidth=pl.beatWidth,
//      sourceWidth=1,contextWidth=2,lengthWidth=7,alignment= BmbParameter.BurstAlignement.WORD)
//    val bmbpp:BmbPortParameter = BmbPortParameter(bmbp,bmbclockDomain,cmdBufferSize=16,dataBufferSize=16,rspBufferSize=16)
//    val ctp : CtrlParameter = CtrlParameter(core, bmbpp)
    SimConfig.withWave.compile(MakeTaskSim(1)).doSimUntilVoid {dut =>
      dut.clockDomain.forkStimulus(10)
      import dut._

//      fork{
//        myclock #= false
//        myrstn #= false
//        clockDomain.waitSampling(5)
//        myrstn #= true
//        while (true){
//          myclock #= true
//          clockDomain.waitEdge()
//          myclock #= false
//          clockDomain.waitEdge()
//        }
//      }
      def write(array: Array[Int], address:Int) = {
//        var i = 0
        io.bmb.cmd.address #= address
        io.bmb.cmd.length #= array.length * dut.pl.bytePerBeat - 1
        io.bmb.cmd.opcode #= 1
        io.bmb.cmd.data #= array(0)
        io.bmb.cmd.valid #= true
        clockDomain.waitSampling()
        for(arr <- array.tail){
//          while (!io.bmb.cmd.ready.toBoolean){
//            clockDomain.waitSampling()
//          }
          io.bmb.cmd.data #= arr
          if(arr == array.last) io.bmb.cmd.last #= true
          clockDomain.waitSampling()
        }
        io.bmb.cmd.last #= false
        io.bmb.cmd.valid #= false
      }
      def read(beatCount:Int, address:Int): Unit = {
        io.bmb.cmd.address #= address
        io.bmb.cmd.length #= beatCount * dut.pl.bytePerBeat - 1
        io.bmb.cmd.opcode #= 0
        io.bmb.cmd.valid #= true
        for(i <- 0 to(beatCount)){
//          while (!io.bmb.cmd.ready.toBoolean){
//            clockDomain.waitSampling()
//          }
          if(i == beatCount) io.bmb.cmd.last #= true
          clockDomain.waitSampling()
        }
        io.bmb.cmd.last #= false
        io.bmb.cmd.valid #= false
      }

      val bmbDatas = new Array[Int]((1<<dut.ctp.port.bmb.access.lengthWidth)/dut.pl.bytePerBeat)
      for(i <- 0 until(bmbDatas.length)){
        bmbDatas(i) = i
      }

      clockDomain.waitSampling(10)
      io.bmb.cmd.valid #= false
      io.bmb.cmd.last #= false
      io.bmb.cmd.source #= 0
//      io.bmb.cmd.exclusive #= false
      io.bmb.cmd.opcode.randomize()
      io.bmb.cmd.address #= 0
      io.bmb.cmd.length #= 0
      io.bmb.cmd.data #= 0
      io.bmb.cmd.mask #= 0
      io.bmb.cmd.context #= 0
      clockDomain.waitSampling(5)
      write(array = bmbDatas,address = 64)
      clockDomain.waitSampling(10)
      write(array = bmbDatas,address = 256)
      clockDomain.waitSampling(5)
      write(array = bmbDatas,address = 2048)
      clockDomain.waitSampling(20)
      read(beatCount = bmbDatas.size, address = 128)
      clockDomain.waitSampling(20)

      simSuccess()


    }
  }
}

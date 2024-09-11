package spinal.lib.memory.sdram.xdr.Simulation

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.memory.sdram.Dfi.Interface._
import spinal.lib.memory.sdram.Dfi.CtrlWithBmb._
case class Bmb2DfiSim(x:Int) extends Component{

  val bmbclockDomain = ClockDomain(ClockDomain.current.clock,ClockDomain.current.reset,config=ClockDomainConfig(resetActiveLevel = HIGH))
  val core:TaskParameter = TaskParameter(timingWidth=5,refWidth=23)
  val sdramtime = SdramTiming(3, RFC = 260, RAS = 38, RP = 15, RCD = 15, WTR = 8, WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35)
  val sdram = SdramConfig(SdramGeneration.MYDDR,bankWidth=2,columnWidth=9,rowWidth=12,dataWidth=8,ddrMHZ=100,ddrWrLat=4,ddrRdLat=4,sdramtime=sdramtime)
  val pl:PhyConfig = PhyConfig(sdram = sdram, phaseCount=4,dataRate=SdramGeneration.MYDDR.dataRate,0,0,0,0,transferPerBurst=8)
  val timeConfig = DfiTimeConfig(tPhyWrLat=pl.sdram.tPhyWrlat,tPhyWrData=0,tPhyWrCsGap=3,dramBurst=pl.transferPerBurst,frequencyRatio=pl.phaseCount,tRddataEn=pl.sdram.tRddataEn,tPhyRdlat=4,tPhyRdCsGap=3,tPhyRdCslat = 0,tPhyWrCsLat = 0)
  val config:DfiConfig = DfiConfig(frequencyRatio=pl.phaseCount,dramAddrWidth=Math.max(pl.sdram.columnWidth,pl.sdram.rowWidth),dramDataWidth=pl.phyIoWidth,
    dramChipselectNumber=2,dramBankWidth=pl.sdram.bankWidth,0,0,1,cmdPhase=0,ddr=new DDR(),timeConfig=timeConfig)
  val bmbp:BmbParameter = BmbParameter(addressWidth=pl.sdram.byteAddressWidth+log2Up(config.chipSelectNumber),dataWidth=pl.beatWidth,
    sourceWidth=1,contextWidth=2,lengthWidth=6,alignment= BmbParameter.BurstAlignement.WORD)
//  val ctrlbmbp:BmbParameter = BmbParameter(addressWidth = 5, dataWidth = pl.sdram.chipAddressWidth,
//    sourceWidth=1,contextWidth=2,lengthWidth=6,alignment= BmbParameter.BurstAlignement.WORD)
  val bmbpp:BmbPortParameter = BmbPortParameter(bmbp,bmbclockDomain,cmdBufferSize=64,dataBufferSize=64,rspBufferSize=64)
  val ctp : CtrlParameter = CtrlParameter(core, bmbpp)
  val cpa = TaskParameterAggregate(ctp.core, pl, BmbAdapter.corePortParameter(ctp.port, pl), config)
  val io = new Bundle {
    val bmb = slave(Bmb(ctp.port.bmb))
    val dfi = master(Dfi(config))
  }
  val bmb2dfi = Bmb2Dfi(ctp,pl,config)
  bmb2dfi.io.bmb <> io.bmb
  bmb2dfi.io.dfi <> io.dfi


}
object Bmb2DfiSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile{val dut = Bmb2DfiSim(1)
      dut.bmb2dfi.bmbBridge.bmbAdapter.io.output.rsp.payload.last.simPublic()
      dut}.doSimUntilVoid {dut =>
      dut.clockDomain.forkStimulus(10)
      import dut._

      def write(array: Array[Int], address:Int) = {
        //        var i = 0
        io.bmb.cmd.address #= address
        io.bmb.cmd.length #= array.length * dut.pl.bytePerBeat - 1
        io.bmb.cmd.opcode #= 1
//        io.bmb.cmd.data #= array(0)
        io.bmb.cmd.data.randomize()
        io.bmb.cmd.valid #= true
        println("write command")
        clockDomain.waitSampling()
        for(arr <- array.tail){
          while (!io.bmb.cmd.ready.toBoolean){
            clockDomain.waitSampling()
          }
//          io.bmb.cmd.data #= arr
          io.bmb.cmd.data.randomize()
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
//        for(i <- 0 to(beatCount)){
          //          while (!io.bmb.cmd.ready.toBoolean){
          //            clockDomain.waitSampling()
          //          }
//          if(i == beatCount)
            io.bmb.cmd.last #= true
          clockDomain.waitSampling()
//        }
        io.bmb.cmd.last #= false
        io.bmb.cmd.valid #= false
        io.bmb.cmd.opcode #= 1
        clockDomain.waitSamplingWhere(dut.io.dfi.read.rden(0).toBoolean)
//        clockDomain.waitSampling(2)
        io.bmb.rsp.ready #= true
        println("read command")
      }
      def readdata(beatCount:Int):Unit = {
        for(i <- 0 until beatCount){
          dut.io.dfi.read.rd.foreach(_.rddatavalid #= true)
          dut.io.dfi.read.rd.foreach(_.rddata.randomize())
          clockDomain.waitSampling()
        }
        dut.io.dfi.read.rd.foreach(_.rddatavalid #= false)
        clockDomain.waitSamplingWhere(dut.bmb2dfi.bmbBridge.bmbAdapter.io.output.rsp.payload.last.toBoolean)
        clockDomain.waitSampling()
        io.bmb.rsp.ready #= false
      }

      val bmbDatas = new Array[Int]((1<<dut.ctp.port.bmb.access.lengthWidth)/dut.pl.bytePerBeat)
      for(i <- 0 until(bmbDatas.length)){
        bmbDatas(i) = i
      }

      fork {
        dut.clockDomain.assertReset()
        dut.clockDomain.fallingEdge()
        sleep(10)
        while(true) {
          dut.clockDomain.clockToggle()
          sleep(5)
        }
      }

//      io.bmb.rsp.ready #= false
      io.dfi.read.rd.foreach(_.rddatavalid #= false)
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
//      io.dfi.read.rd.foreach(_.rddatavalid #= false)
//      io.bmb.rsp.ready #= true
//      clockDomain.waitSampling()
//      io.bmb.rsp.ready #= false
//      io.dfi.read.rd.foreach(_.rddata.randomize())
//      io.bmb.rsp.ready #= true
//      clockDomain.waitSamplingWhere(dut.bmb2dfi.dfiAlignment.initialize.io.initDone.toBoolean)
//      clockDomain.waitSamplingWhere(dut.io.initDone.toBoolean)
      clockDomain.waitSampling(5)
      write(array = bmbDatas,address = 64)
      println("writing is OK")
      clockDomain.waitSampling(10)
      write(array = bmbDatas,address = 3145728)
      println("writing is OK")
      clockDomain.waitSampling(10)
      write(array = bmbDatas,address = 2048)
      println("writing is OK")

      clockDomain.waitSampling(10)
      read(beatCount = bmbDatas.size, address = 128)
      clockDomain.waitSampling(2)//The time interval is less than or equal to log2Up((timeConfig.tPhyRdlat + timeConfig.tRddataEn + pl.beatCount-1)/pl.beatCount + 1)
      readdata(bmbDatas.size)
      println("reading is OK")

      clockDomain.waitSampling(5)
      read(beatCount = bmbDatas.size, address = 9437184)
      clockDomain.waitSampling()
      readdata(bmbDatas.size)
      println("reading is OK")

      clockDomain.waitSampling(5)
      write(array = bmbDatas,address = 4120)
      println("writing is OK")

      clockDomain.waitSampling(5)
      write(array = bmbDatas,address = 12280)
      println("writing is OK")
      clockDomain.waitSampling(20)

      simSuccess()


    }
  }
}

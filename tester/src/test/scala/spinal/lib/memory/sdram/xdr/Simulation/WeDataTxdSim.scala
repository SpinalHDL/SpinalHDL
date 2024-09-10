package spinal.lib.memory.sdram.xdr.Simulation

import spinal.core.sim._
import spinal.lib.memory.sdram.Dfi.Alignment._
import spinal.lib.memory.sdram.Dfi.Interface._
import spinal.lib.memory.sdram.Dfi.Tools._

object WeDataTxdSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(WrDataTxd(CoreParameterAggregate(CoreParameter(64,5,5),
      PhyLayout(SdramLayout(SdramGeneration.DDR3,2,9,12,12,100,4,4,sdramtime=SdramTiming(3, RFC = 260, RAS = 38, RP = 15, RCD = 15, WTR = 8, WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35)),1,2,0,0,0,0,8),
      CorePortParameter(contextWidth = 8, writeTockenInterfaceWidth = 1, writeTockenBufferSize = 8, canRead = true, canWrite = true),
      DfiConfig(1,12,24,1,2,2,1,1,0,DDR(),DfiTimeConfig(tPhyWrData = 3,dramBurst = 16,frequencyRatio = 1,tPhyWrLat = 6)))
      )).doSimUntilVoid {dut =>
      dut.clockDomain.forkStimulus(10)
      import dut.cpa.config._
      def inputdata(x:Int) = {
        if(dut.io.coreWrdata.ready.toBoolean){
//          val random = (new util.Random).nextInt(1<<dut.cpa.pl.beatWidth)
//          for(i <- 0 until(dut.config.frequencyRatio)){
//            dut.io.coreWrdata.data(i*dut.config.dataWidth,dut.config.dataWidth bits) #= (new util.Random).nextInt(1<<dut.config.dataWidth)
//            dut.io.coreWrdata.data #= (1199999835.toLong * x.toLong).asInstanceOf[Int].abs
            dut.io.coreWrdata.data #= (119999.toLong * x.toLong).asInstanceOf[Int].abs
//          }
          dut.io.coreWrdata.mask #= 1
        }
        dut.clockDomain.waitSampling()
      }
      def burst(array: Array[Int])={
        for (arr <- array){
          inputdata(arr)
        }
      }

      dut.clockDomain.waitSampling(5)
      dut.io.write #= false
      dut.io.coreWrdata.valid #= false
      dut.io.coreWrdata.payload.data #= 0
      dut.io.coreWrdata.payload.mask #= 0
      dut.clockDomain.waitSampling(5)
      fork{
        dut.io.write #= true
        dut.clockDomain.waitSampling(timeConfig.dfiRWLength)
        dut.io.write #= false
      }
      fork{
        val array = new Array[Int](timeConfig.dfiRWLength)
        for(i <- 1 to array.size){
          array(i-1) = i
        }
        dut.io.coreWrdata.valid #= true
//        dut.io.coreWrdata.data #= 1199888847.toLong.asInstanceOf[Int].abs
        dut.io.coreWrdata.data #= 11998.toLong.asInstanceOf[Int].abs
//        dut.clockDomain.waitSamplingWhere(dut.io.coreWrdata.ready.toBoolean)
        while (true){
          if(dut.io.coreWrdata.ready.toBoolean){
            burst(array)
            dut.clockDomain.waitSampling(5)
            simSuccess()
          }
          dut.clockDomain.waitSampling()
        }
//        burst(array)
//        dut.clockDomain.waitSampling(5)
//        simSuccess()
      }
    }
  }
}

package spinal.lib.memory.sdram.xdr.Simulation

import spinal.core.sim._
import spinal.lib.memory.sdram.Dfi.Alignment._
import spinal.lib.memory.sdram.Dfi.Interface._
object RdAlignmentSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(RdAlignment(DfiConfig(4,8,16,1,2,2,1,1,0,DDR(),
      DfiTimeConfig(tPhyRdlat = 4,dramBurst = 16,frequencyRatio = 4))
      )).doSimUntilVoid {dut =>
      dut.clockDomain.forkStimulus(10)
      import dut.config._
      def inputrddata(phase:Int,data:List[Int]): Unit = {
        dut.io.rd(phase).rddatavalid #= true
        for(i <- data){
          dut.io.rd(phase).rddata #= i
          dut.clockDomain.waitSampling()
        }
        dut.io.rd(phase).rddatavalid #= false
      }
      def lastPhaseinFristBeat(phase:Int,list:List[Int],x:Int): Unit = {
        for(i <- 0 until(dut.config.frequencyRatio) if i <= phase){
          fork{
            inputrddata(i,list.drop(2*i).take(x))
          }
        }
        dut.clockDomain.waitSampling()
        for(i <- 0 until(dut.config.frequencyRatio) if i > phase){
          fork{
            inputrddata(i,list.drop(2*i).take(x))
          }
        }
      }

//      dut.clockDomain.waitSampling(5)

      for(i <- 0 until(dut.config.frequencyRatio)){
        fork{
          dut.io.rd(i).rddatavalid #= false
          dut.io.rddata(i).ready #= false
          dut.clockDomain.waitSampling(5)
          dut.io.rddata(i).ready #= true
        }
      }

      dut.clockDomain.waitSampling(10)
      val indata = new Array[Int](timeConfig.dramBurst)
      for(i <- 0 until(indata.size/2)){
        indata(i * 2) = i
        indata(i * 2 + 1) = i + indata.size/2
      }
      lastPhaseinFristBeat(1,indata.toList,2)
      dut.clockDomain.waitSampling(2)
      lastPhaseinFristBeat(3,indata.map(_+timeConfig.dramBurst).toList,2)
      dut.clockDomain.waitSampling(10)
      lastPhaseinFristBeat(1,indata.toList,2)
      dut.clockDomain.waitSampling(1)
      lastPhaseinFristBeat(0,indata.map(_+timeConfig.dramBurst).toList,2)
      dut.clockDomain.waitSampling(10)
      simSuccess()
    }
  }
}

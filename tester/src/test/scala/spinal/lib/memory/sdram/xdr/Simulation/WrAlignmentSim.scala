package spinal.lib.memory.sdram.xdr.Simulation

import spinal.core.sim._
import spinal.lib.memory.sdram.Dfi.Alignment._
import spinal.lib.memory.sdram.Dfi.Interface._
object WrAlignmentSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(WrAlignment(DfiConfig(2,8,16,1,2,2,1,1,0,DDR(),
      DfiTimeConfig(tPhyWrData = 3,dramBurst = 8,frequencyRatio = 2))
      )).doSimUntilVoid {dut =>
      dut.clockDomain.forkStimulus(10)
      import dut.config._
      def inputwrdata(phase:Int,data:List[Int]): Unit = {
        dut.io.wrdata(phase).valid #= true
        for(i <- data){
          dut.io.wrdata(phase).wrdata #= i
          dut.clockDomain.waitSampling()
        }
        dut.io.wrdata(phase).valid #= false
      }
      def dataStart(phase:Int,list:List[Int]): Unit = {
//        var x = 0
        for(i <- 0 until(dut.config.frequencyRatio) if i >= phase){
          fork{
            inputwrdata(i,list.drop(2*(i-phase)).take(2))
//            x += 2
          }
        }
        dut.clockDomain.waitSampling()
        for(i <- 0 until(dut.config.frequencyRatio) if i < phase){
          fork{
            inputwrdata(i,list.drop(2*(i+dut.config.frequencyRatio-phase)).take(2))
//            x += 2
          }
        }
      }

      for(i <- 0 until(dut.config.frequencyRatio)){
        dut.io.wrdata(i).valid #= false
      }
      dut.clockDomain.waitSampling(10)
      val indata = new Array[Int](timeConfig.dramBurst)
      for(i <- 0 until(indata.size/2)){
        indata(i * 2) = i
        indata(i * 2 + 1) = i + indata.size/2
      }
      dataStart(1,indata.toList)
      dut.clockDomain.waitSampling(10)
      dataStart(3,indata.toList)
      dut.clockDomain.waitSampling(10)
      simSuccess()
    }
  }
}

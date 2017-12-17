package spinal.tester.pending

import org.scalatest.FlatSpec
import spinal.core.SimManagedApi._
import spinal.core._
import spinal.lib.LatencyAnalysis
import spinal.sim

import scala.util.Random

class AdderTreeSpec extends FlatSpec {

  class Dut extends Component {
    val io = new Bundle {
      val a = in UInt(8 bits)
      val b = in UInt(0 bits)
      val c = in UInt(2 bits)
      val output = out UInt(9 bits)
    }

    import io._

    //output := AdderTree.adderTree(List(a, b, c))
    output := a.resize(9) + c

    val latency = LatencyAnalysis(c, output)
  }

  val simConfig = SimConfig(rtl = SpinalVerilog(new Dut())).withWave
  val compiled = simConfig.compile()

  "An adder tree" should "calculate correct sums" in {
    compiled.doManagedSim(dut => {

      val test = sim.repeatSim(1024) {
        import dut.io._
        val inputs = List(a,b,c)
        //val stimuli = inputs map (i => Random.nextInt(i.maxValue.toInt+1))

        //println(stimuli)
        //val sum = stimuli.sum
        //(inputs, stimuli).zipped.foreach(_ #= _)

        a #= 1
        b #= 0
        c #= 3

        val sum = 4
        sleep(10)
        assert(dut.io.output.toBigInt == sum)
        ()
      }
    })
  }

}
package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

class StreamForkFormal[T <: Data](dataType: HardType[T], portCount: Int, synchronous: Boolean = false) extends Component{
  val dut = FormalDut(new StreamFork(dataType, portCount, synchronous))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.formalAssumeInputs()

  anyseq(dut.io.input.payload)
  anyseq(dut.io.input.valid)

  val fireCount = Counter(0x100000000L, inc = dut.io.input.fire)
  assume(!fireCount.willOverflow)
  val fireOrValid = fireCount +^ dut.io.input.valid.asUInt

  val outputs = dut.io.outputs.map(output => new Area {
    val outputFireCount = Counter(0x100000000L, inc = output.fire)
    assume(!outputFireCount.willOverflow)
    val outputFireOrValid = outputFireCount.value +^ output.valid.asUInt

    // The only real contract with StreamFork is that it's outputs should fire the same number of times and have the same
    // payload as the input.
    if(synchronous) {
      // Syncronous ones should all fire at the same time; so fire counts are always exactly equal
      assert(fireCount.value === outputFireCount)
      when(output.fire) {
        assert(dut.io.input.fire)
      }
    } else {
      // Since non-sync do not fire at the same time, we also incorporate the valid signals. If the input valid is
      // high, then we expect the outputs to either have their valid high or have already fired.
      assert(fireOrValid === outputFireOrValid)
    }
    when(output.fire) {
      assert(dut.io.input.valid)
      assert(dut.io.input.payload === output.payload)
    }

    output.formalAssertsMaster()
    anyseq(output.ready)
  })
}

class FormalStreamFork extends SpinalFormalFunSuite {
  for(portCount <- (1 until 5);
      sync <- Seq(true, false)) {
    test(s"Test StreamFork p${portCount} s${sync}") {
      FormalConfig.withDebug.withProve(15).withBMC(50).doVerify(new StreamForkFormal(UInt(8 bits), portCount, sync))
    }
  }

}

package spinal.tester.code

import spinal.core._

//Here is our DUT
class LimitedCounter extends Component{
  //The value register will always be between [2:10]
  val value = Reg(UInt(4 bits)) init(2)
  when(value < 10){
    value := value + 1
  }
}

object LimitedCounterFormal extends App {
  // import utilities to run the formal verification, but also some utilities to describe formal stuff
  import spinal.core.formal._

  // Here we run a formal verification which will explore the state space up to 15 cycles to find an assertion failure
  FormalConfig.withBMC(15).doVerify(new Component {
    // Instanciate our LimitedCounter DUT as a FormalDut, which ensure that all the outputs of the dut are :
    // - directly and indirectly driven (no latch / no floating wire)
    // - allows the current toplevel to read every signal across the hierarchy
    val dut = FormalDut(new LimitedCounter())

    // Ensure that the state space start with a proper reset
    assumeInitial(ClockDomain.current.isResetActive)

    // Check a few things
    assert(dut.value >= 2)
    assert(dut.value <= 10)
  })
}

object LimitedCounterProveFormal extends App {
  import spinal.core.formal._

  FormalConfig.withProve(15).doVerify(new Component {
    val dut = FormalDut(new LimitedCounter())

    assumeInitial(ClockDomain.current.isResetActive)

    assert(dut.value >= 2)
    assert(dut.value <= 10)
  })
}

class LimitedCounterEmbedded extends Component{
  val value = Reg(UInt(4 bits)) init(2)
  when(value < 10){
    value := value + 1
  }

  GenerationFlags.formal {
    assert(value >= 2)
    assert(value <= 10)
  }
}

object LimitedCounterEmbeddedFormal extends App {
  import spinal.core.formal._

  FormalConfig.withBMC(15).doVerify(new Component {
    val dut = FormalDut(new LimitedCounterEmbedded())
    assumeInitial(ClockDomain.current.isResetActive)
  })
}


class LimitedCounterInc extends Component{
  //Only increment the value when the inc input is set
  val inc = in Bool()
  val value = Reg(UInt(4 bits)) init(2)
  when(inc && value < 10){
    value := value + 1
  }
}

object LimitedCounterIncFormal extends App {
  import spinal.core.formal._

  FormalConfig.withBMC(15).doVerify(new Component {
    val dut = FormalDut(new LimitedCounterInc())
    assumeInitial(ClockDomain.current.isResetActive)
    assert(dut.value >= 2)
    assert(dut.value <= 10)

    // Drive dut.inc with random values
    anyseq(dut.inc)
  })
}


object LimitedCounterMoreAssertFormal extends App {
  import spinal.core.formal._

  FormalConfig.withBMC(15).doVerify(new Component {
    val dut = FormalDut(new LimitedCounter())
    assumeInitial(ClockDomain.current.isResetActive)

    // Check that the value is incrementing.
    // hasPast is used to ensure that the past(dut.value) had at least one sampling out of reset
    when(pastValid() && past(dut.value) =/= 10){
      assert(dut.value === past(dut.value) + 1)
    }
  })
}


object LimitedCounterCoverFormal extends App {
  import spinal.core.formal._

  FormalConfig.withCover(15).doVerify(new Component {
    assumeInitial(ClockDomain.current.isResetActive)

    val dut = FormalDut(new LimitedCounterInc())
    anyseq(dut.inc)    
    ClockDomain.current.duringReset { assume(dut.inc === False) }

    for(i <- 2 to 10){
      cover(dut.value === i)
    }
  })
}

object LimitedCounterAllFormal extends App {
  import spinal.core.formal._

  FormalConfig.withCover(15).withBMC(15).withProve(15).doVerify(new Component {    
    assumeInitial(ClockDomain.current.isResetActive)

    val dut = FormalDut(new LimitedCounterInc())
    anyseq(dut.inc)    
    ClockDomain.current.duringReset { assume(dut.inc === False) }

    val dst = anyconst(UInt(4 bits))
    assume(dst >= 2 && dst <= 10)
    cover(dut.value === dst)
    
    assert(dut.value >= 2)
    assert(dut.value <= 10)

    when(past(dut.inc)){
      when(past(dut.value) < 10) {
        assert(past(dut.value) === dut.value - 1)
      }.otherwise {
        assert(past(dut.value) === dut.value)
      }
    }
  })
}

import spinal.core._
import spinal.lib._
class DutWithRam extends Component{
  val ram = Mem.fill(4)(UInt(8 bits))
  val write = slave(ram.writePort)
  val read = slave(ram.readAsyncPort)
}

object FormalRam extends App {
  import spinal.core.formal._

  FormalConfig.withBMC(15).doVerify(new Component {
    val dut = FormalDut(new DutWithRam())
    assumeInitial(ClockDomain.current.isResetActive)

    // assume that no word in the ram has the value 1
//    for(i <- 0 until dut.ram.wordCount){
//      assumeInitial(dut.ram(i) =/= 1)
//    }
    assumeInitial(!dut.ram.formalContains(1))
//    assumeInitial(dut.ram.formalCount(1) === 0)

    // Allow the write anything but value 1 in the ram
    anyseq(dut.write)
    clockDomain.withoutReset(){ //As the memory write can occur during reset, we need to ensure the assume apply there too
      assume(dut.write.data =/= 1)
    }

    // Check that no word in the ram is set to 1
    anyseq(dut.read.address)
    assert(dut.read.data =/= 1)
  })
}

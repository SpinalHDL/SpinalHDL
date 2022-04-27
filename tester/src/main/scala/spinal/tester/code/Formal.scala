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
    when(withPast() && past(dut.value) =/= 10){
      assert(dut.value === past(dut.value) + 1)
    }
  })
}

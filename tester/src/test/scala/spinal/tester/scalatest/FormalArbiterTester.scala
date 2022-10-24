package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

class FormalArbiterTester extends SpinalFormalFunSuite {
  def noneFragmentContext() = new Area {
    val portCount = 5
    val dataType =Bits(8 bits) 
    val reset = ClockDomain.current.isResetActive
    val notReset = !(reset || past(reset))
    cover(notReset)
    assumeInitial(reset)

    val select = UInt(log2Up(portCount) bit)
    val selectOH = Bits (portCount bit)
    val inputs = Vec(slave(Stream(dataType)), portCount)
    val output = master(Stream(dataType))
    val inputsValid = Vec(inputs.map(_.valid))
    val selStableCond = past(output.isStall) && notReset

    when(reset || past(reset)) {
        for (i <- 0 until portCount) {
          assume(inputs(i).valid === False)
        }
        assert(select === 0)
      }

    for (i <- 0 until portCount) {
      inputs(i).formalAssumesSlave()
    }
    assert(select < portCount)
    assert(select === OHToUInt(selectOH))
    assert(output.fire === inputs(select).fire)
    assert(output.payload === inputs(select).payload)

    output.formalCovers(2)
    assert(selectOH === OHMasking.first(selectOH))
  }
  def fragmentContext() = new Area {
    val portCount = 5
    val dataType = Fragment(Bits(8 bits))  
    val reset = ClockDomain.current.isResetActive
    val notReset = !(reset || past(reset))
    cover(notReset)
    assumeInitial(reset)

    val select = UInt(log2Up(portCount) bit)
    val selectOH = Bits (portCount bit)
    val inputs = Vec(slave(Stream(dataType)), portCount)
    val output = master(Stream(dataType))
    val inputsValid = Vec(inputs.map(_.valid))
    val selStableCond = !past((inputsValid.asBits === 0) || output.last || reset)

    when(reset || past(reset)) {
        for (i <- 0 until portCount) {
          assume(inputs(i).valid === False)
        }
        assert(select === 0)
      }

    for (i <- 0 until portCount) {
      inputs(i).formalAssumesSlave()
    }
    assert(select < portCount)
    assert(select === OHToUInt(selectOH))
    assert(output.fire === inputs(select).fire)
    assert(output.payload === inputs(select).payload)

    output.formalCovers(2)
    assert(selectOH === OHMasking.first(selectOH))
  }
  def withLockAssertVerify[T <: Data](output: Stream[T]) = new Area{      
    output.formalAssertsMaster()
  }
  def withoutLockAssertVerify[T <: Data](outisStall: Bool,inputsValid: Vec[Bool],output: Stream[T]) = new Area{      
    val stallStableSel = outisStall && stable(inputsValid)
    cover(stallStableSel)
    when(stallStableSel){
      output.formalAssertsMaster()
    } 
  }
  def selStableVerify(selectOH: Bits ,selStableCond: Bool) = new Area{
    cover(selStableCond)
    when(selStableCond){
      assert(stable(selectOH))
    }    
  }
  def sequentialOrderVerify(select: UInt ,portCount: Int) = new Area {      
    val d1 = anyconst(UInt(log2Up(portCount) bit))
    assume(d1 < portCount)
    val d2 = UInt(log2Up(portCount) bit)
    d2 := (d1 + 1) % portCount

    val cntSeqCheck = changed(select) && (select === d2)
    cover(cntSeqCheck)
    when(cntSeqCheck) {
      assert(past(select) === d1)
    }
  }
  def lowFirstVerify(selectOH: Bits ,inputsvalid: Vec[Bool],unLockCond: Bool) = new Area {    
    val inputsLowerFirst = OHMasking.first(inputsvalid).asBits
    cover(unLockCond)
    when(unLockCond){
      assert(selectOH === inputsLowerFirst)
    }
  }
 
  def roundRobinVerify(selectOH: Bits ,inputsValid: Vec[Bool],portCount: Int, maskLocked: Vec[Bool],unLockCond: Bool) = new Area {
    val requests = inputsValid.asBits.asUInt
    val uGranted = selectOH.asUInt
    val doubleRequests = requests @@ requests
    val doubleGrant = doubleRequests & ~(doubleRequests-uGranted)
    val masked = doubleGrant(portCount,portCount bits) | doubleGrant(0,portCount bits)
    val inputsRoundRobinOH = masked.asBits
    cover(unLockCond)
    when(unLockCond){
      assert(selectOH === inputsRoundRobinOH)
    }
    //used for Prove
    assert(maskLocked === OHMasking.first(maskLocked))
  }

  test("Arbiter-sequentialOrder-verify"){
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val context = noneFragmentContext()
        val dut = FormalDut(new StreamArbiter(context.dataType,context.portCount)(StreamArbiter.Arbitration.sequentialOrder,StreamArbiter.Lock.none))
        
        context.select := dut.io.chosen
        context.selectOH := dut.io.chosenOH
        context.output << dut.io.output
        for (i <- 0 until context.portCount) {
          context.inputs(i) >> dut.io.inputs(i)
        }
    
        val withAssert = withLockAssertVerify(context.output)
        val sequentialVerify = sequentialOrderVerify(context.select ,context.portCount)
      })
  }

  test("Arbiter-lowerfirst-none-verify"){
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val context = noneFragmentContext()
        val dut = FormalDut(new StreamArbiter(context.dataType,context.portCount)(StreamArbiter.Arbitration.lowerFirst,StreamArbiter.Lock.none))
        
        context.select := dut.io.chosen
        context.selectOH := dut.io.chosenOH
        context.output << dut.io.output
        for (i <- 0 until context.portCount) {
          context.inputs(i) >> dut.io.inputs(i)
        }

        val selectUnLockCond = True
        val lowFirst = lowFirstVerify(context.selectOH,context.inputsValid,selectUnLockCond)        
        val withAssert = withoutLockAssertVerify(context.output.isStall,context.inputsValid,context.output)        
      })
  }

  test("Arbiter-lowerfirst-transactionLock-verify"){
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {

        val context = noneFragmentContext()
        val dut = FormalDut(new StreamArbiter(context.dataType,context.portCount)(StreamArbiter.Arbitration.lowerFirst,StreamArbiter.Lock.transactionLock))

        context.select := dut.io.chosen
        context.selectOH := dut.io.chosenOH
        context.output << dut.io.output
        for (i <- 0 until context.portCount) {
          context.inputs(i) >> dut.io.inputs(i)
        }
        val withAssert = withLockAssertVerify(context.output)
        val selectUnLockCond = !dut.locked || past(context.output.fire)
        val lowFirst = lowFirstVerify(context.selectOH,context.inputsValid,selectUnLockCond)
        val selStable= selStableVerify(context.selectOH,context.selStableCond)

        //used for Prove
        when(context.reset || past(context.reset)) {
          for (i <- 0 until context.portCount) {
            assert(!dut.locked)
          }
        }
        when(context.notReset){
          assert(past(context.output.isStall) === dut.locked)
        }  
      })
  }

  test("Arbiter-lowerfirst-fragment-verify"){
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val context = fragmentContext()
        val dut = FormalDut(new StreamArbiter(context.dataType,context.portCount)(StreamArbiter.Arbitration.lowerFirst,StreamArbiter.Lock.fragmentLock))

        context.select := dut.io.chosen
        context.selectOH := dut.io.chosenOH
        context.output << dut.io.output
        for (i <- 0 until context.portCount) {
          context.inputs(i) >> dut.io.inputs(i)
        }

        when(context.reset || past(context.reset)) {
          for (i <- 0 until context.portCount) {
            assert(!dut.locked)
          }
        }
        val withAssert = withLockAssertVerify(context.output)
        val selectUnLockCond = !dut.locked || past(context.output.last&& context.output.fire)
        val lowFirst = lowFirstVerify(context.selectOH,context.inputsValid,selectUnLockCond)        
        val selStable= selStableVerify(context.selectOH,context.selStableCond)
      })
  }

  test("Arbiter-roundrobin-none-verify"){
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {        
        val context = noneFragmentContext()
        val dut = FormalDut(new StreamArbiter(context.dataType,context.portCount)(StreamArbiter.Arbitration.roundRobin,StreamArbiter.Lock.none))
        
        context.select := dut.io.chosen
        context.selectOH := dut.io.chosenOH
        context.output << dut.io.output
        for (i <- 0 until context.portCount) {
          context.inputs(i) >> dut.io.inputs(i)
        }

        val selectUnLockCond = True
        val roundRobin = roundRobinVerify(context.selectOH,context.inputsValid,context.portCount,dut.maskLocked,selectUnLockCond)
        val withAssert = withoutLockAssertVerify(context.output.isStall,roundRobin.masked.asBools,context.output)
      })
  }

  test("Arbiter-roundrobin-transactionLock-verify"){
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      .withDebug
      .doVerify(new Component {
        val context = noneFragmentContext()
        val dut = FormalDut(new StreamArbiter(context.dataType,context.portCount)(StreamArbiter.Arbitration.roundRobin,StreamArbiter.Lock.transactionLock))
        
        context.select := dut.io.chosen
        context.selectOH := dut.io.chosenOH
        context.output << dut.io.output
        for (i <- 0 until context.portCount) {
          context.inputs(i) >> dut.io.inputs(i)
        }

        val selectUnLockCond = True
        val roundRobin = roundRobinVerify(context.selectOH,context.inputsValid,context.portCount,dut.maskLocked,selectUnLockCond)
        val withAssert = withoutLockAssertVerify(context.output.isStall,roundRobin.masked.asBools,context.output)
        val selStable= selStableVerify(context.selectOH,context.selStableCond)
      })
  }
 
  test("Arbiter-roundrobin-fragment-verify"){
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val context = fragmentContext()
        val dut = FormalDut(new StreamArbiter(context.dataType,context.portCount)(StreamArbiter.Arbitration.roundRobin,StreamArbiter.Lock.fragmentLock))

        context.select := dut.io.chosen
        context.selectOH := dut.io.chosenOH
        context.output << dut.io.output
        for (i <- 0 until context.portCount) {
          context.inputs(i) >> dut.io.inputs(i)
        }

        when(context.reset || past(context.reset)) {
          for (i <- 0 until context.portCount) {
            assert(!dut.locked)
          }
        }
        val withAssert = withLockAssertVerify(context.output)
        val selectUnLockCond = !dut.locked || past(context.output.last&& context.output.fire)
        val roundRobin = roundRobinVerify(context.selectOH,context.inputsValid,context.portCount,dut.maskLocked,selectUnLockCond)
        val selStable= selStableVerify(context.selectOH,context.selStableCond)
      })
  }
}

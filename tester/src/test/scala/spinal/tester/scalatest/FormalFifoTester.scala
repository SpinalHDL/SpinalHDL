package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib.{StreamFifo, History, OHToUInt}
import spinal.lib.formal._

class FormalFifoTester extends SpinalFormalFunSuite {

  // @NOTE Passes BMC and Cover test for all StreamFifo configurations
  // @TODO Passes induction Prove only for forFMax=true, fix for forFMax=false
  // this requires assertions to establish pop/push relationship with counters for all cases
  // @TODO Stream.formalCheckRam() should check Vec on useVec
  def formalTestStreamFifo(backend: FormalBackend,
                           depth: Int,
                           withAsyncRead: Boolean,
                           withBypass: Boolean,
                           forFMax : Boolean,
                           allowExtraMsb : Boolean): Unit = test(s"StreamFifo_depth.$depth-withAsyncRead.$withAsyncRead-withBypass.$withBypass-forFMax.$forFMax-allowExtraMsb.$allowExtraMsb-backend.$backend") {

    // it's not really testing much logic, so not used as argument
    val useVec = false

    val initialCycles = 2
    val inOutDelay = 1 + (!withAsyncRead).toInt + 1
    val coverCycles = depth * 2 + initialCycles + inOutDelay

    // @TODO .withProve() still needs more assertions for some StreamFifo configurations
    //FormalConfig
    //  .withBMC(coverCycles + 2)
    //  .withProve(14)
    //  .withCover(coverCycles)

    val definitionName = s"StreamFifo_depth_%d%s%s%s%s"
    .format(depth,
      if (withAsyncRead) "_withAsyncRead" else "",
      if (withBypass) "_withBypass" else "",
      if (forFMax) "_forFMax" else "",
      if (allowExtraMsb) "_allowExtraMsb" else ""
    )

    val dataWidth = 7

    // @TODO Passes BMC and Cover test for isPow2(depth >= 4) StreamFifo configurations
    var formalCfg = FormalConfig
      .withBackend(backend)
      .withBMC(coverCycles + 2)
      .withCover(coverCycles)
    // @TODO Passes Prove only for forFMax=true, TODO forFMax=false cases
    if (forFMax==true) formalCfg.withProve(coverCycles)
    formalCfg
      // .withDebug
      .doVerify(new Component {
        val dut = FormalDut(new StreamFifo(UInt(dataWidth bits), depth, withAsyncRead=withAsyncRead, withBypass=withBypass, forFMax=forFMax, allowExtraMsb=allowExtraMsb, useVec=useVec & withAsyncRead))
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val inValue = anyseq(UInt(dataWidth bits))
        val inValid = anyseq(Bool())
        val outReady = anyseq(Bool())
        dut.io.push.payload := inValue
        dut.io.push.valid := inValid
        dut.io.pop.ready := outReady

        // assume no valid while reset and one clock later.
        when(reset || past(reset)) {
          assume(inValid === False)
        }

        dut.io.push.formalAssumesSlave()
        dut.io.pop.formalAssertsMaster()

        dut.io.push.formalCovers()
        // back to back transaction cover test.

        // sync read requires a minimum depth (3?) for back-to-back
        if (withAsyncRead || depth >= 4) {
          dut.io.pop.formalCovers(coverCycles - initialCycles - inOutDelay - 1)
        }

        val d1 = anyconst(UInt(dataWidth bits))
        val d2 = anyconst(UInt(dataWidth bits))

        // assume first d1, then d2 enters the StreamFifo
        val (d1_in, d2_in) = dut.io.push.formalAssumesOrder(d1, d2)
        // assert first d1, then d2 leaves the StreamFifo
        val (d1_out, d2_out) = dut.io.pop.formalAssertsOrder(d1, d2)

        // make sure d1 is not already in the StreamFifo before it enters
        when(!d1_in) { assume(!dut.formalContains(d1)) }
        // make sure d1 only exists once inside the StreamFifo after it entered and before it left
        when(d1_in && !d1_out) { assert(dut.formalCount(d1) === 1) }

        when(!d2_in) { assume(!dut.formalContains(d2)) }
        when(d2_in && !d2_out) { assert(dut.formalCount(d2) === 1) }

        // make sure that d2 does not leave before d1 leaves the StreamFifo
        when(d1_in && d2_in && !d1_out) { assert(!d2_out) }

        // cover the case that FIFO first goes to full, then to empty
        dut.formalFullToEmpty()

        // get order index of x
        // x with lowest index leaves StreamFifo first
        def getCompId(x: UInt): UInt = {
          // index in RAM
          val id = OHToUInt(dut.formalCheckRam(_ === x.pull()).asBits)
          // offset all RAM order indexes by 1, as order index 0 is reserved for m2sPipe()
          // assume wrapped; add RAM size (depth) to location
          val extId = id +^ U(depth + 1)
          val compId = CombInit(extId)
          // for all non wrapped locations
          when(id >= (dut.logic.ptr.pop.resize(log2Up(depth)))) {
            // offset all RAM order indexes by 1, as order index 0 is reserved for m2sPipe()
            compId := (id +^ U(1)).resized
          }
          if (!withAsyncRead) {
            when(dut.formalCheckOutputStage(_ === x.pull())) {
              // order index 0 for m2sPipe register
              compId := 0
            }
          }
          compId
        }
        when(d1_in && d2_in && !d1_out && !d2_out) {
          assert(getCompId(d1) < getCompId(d2))
        }
      }.setDefinitionName(definitionName))
  }
  // @TODO [info] spinal.tester.scalatest.FormalFifoTester *** ABORTED ***
  // [info]   java.lang.StackOverflowError:
  // for (depth <- List(0, 1, 2, 3, 4, 15, 16, 17, 24);

  // @NOTE depth < 2 will fail due to the formal test assuming FIFO RAM
  // @TODO useVec is not yet supported in formal, should be easy to add
  for (backend <- List(SymbiYosysFormalBackend, GhdlFormalBackend);
    depth <- List(3, 4, 7);
    withAsyncRead <- List(false, true);
    withBypass <- List(false, true);
    forFMax <- List(false, true);
    allowExtraMsb <- List(false, true);

  if !(!withAsyncRead && withBypass)) {
    formalTestStreamFifo(backend,
      depth = depth,
      withAsyncRead = withAsyncRead,
      withBypass = withBypass,
      forFMax = forFMax,
      allowExtraMsb = allowExtraMsb
    )
  }
}

class StreamFifoFormal[T <: Data](val dataType: HardType[T],
                                  val depth: Int,
                                  val withAsyncRead : Boolean = false,
                                  val withBypass : Boolean = false,
                                  val allowExtraMsb : Boolean = true,
                                  val forFMax : Boolean = false,
                                  val useVec : Boolean = false) extends Component {
  val dut = FormalDut(new StreamFifo(dataType, depth, withAsyncRead, withBypass, allowExtraMsb, forFMax, useVec))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.formalAssumeInputs()
  dut.io.pop.formalAssertsMaster()

  anyseq(dut.io.push.valid)
  anyseq(dut.io.push.payload)

  anyseq(dut.io.flush)
  anyseq(dut.io.pop.ready)
  when(dut.io.flush) {
    assume(dut.io.pop.ready)
  }
}

class StreamFifoFormalProveTest extends SpinalFormalFunSuite {
  val configs = {for(
    // Don't bother testing 0 and 1 here; they have no meaningful asserts
    depth <- Seq(2, 6, 8);
    withAsynRead <- Seq(true, false);
    withBypass <- Seq(true, false);
    allowExtraMsb <- Seq(true, false);
    forFMax <- Seq(true, false);
    useVec <- Seq(true, false)
  ) yield {
    if(!withAsynRead && (useVec || withBypass)) {
      null
    } else {
      (s"fifo_prove_${depth}_withAsynRead${withAsynRead}_withBypass${withBypass}_allowExtraMsb${allowExtraMsb}_forFMax${forFMax}_useVec${useVec}", () => new StreamFifoFormal(UInt(2 bits), depth, withAsynRead, withBypass, allowExtraMsb, forFMax, useVec))
    }
  }}.filter(_ != null)


  for((name, dut_ctor) <- configs) {
    test(name) {
      FormalConfig
        .withProve(20)
        .doVerify(dut_ctor().setDefinitionName(name))
    }
  }

}
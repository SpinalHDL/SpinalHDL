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
  def formalTestStreamFifo(depth: Int,
                     withAsyncRead: Boolean,
                     withBypass: Boolean,
                     forFMax : Boolean,
                     allowExtraMsb : Boolean): Unit = test(s"StreamFifo_depth.$depth-withAsyncRead.$withAsyncRead-withBypass.$withBypass-forFMax.$forFMax-allowExtraMsb.$allowExtraMsb") {

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
    var formalCfg = FormalConfig.withBMC(coverCycles + 2).withCover(coverCycles)
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

        if (dut.withExtraMsb) {
          assert(dut.logic.ptr.pop <= ((depth << 1) - 1))
          assert(dut.logic.ptr.push <= ((depth << 1) - 1))
        } else {
          assert(dut.logic.ptr.pop <= (depth-1))
          assert(dut.logic.ptr.push <= (depth-1))
        }

        // TODO for .withProve() induction prove to pass, relationship between push/pop distance and some fill rate
        // must be asserted. Currently only withFMax=true use cases are supported.
        if (forFMax & (!isPow2(depth) || !dut.withExtraMsb) & withAsyncRead) {
          // initially (1 << log2Up(depth)), minus what is inside the StreamFifo
          assert(dut.logic.ptr.arb.fmax.emptyTracker.value === ((1 << log2Up(depth)) - ((depth +^ dut.logic.ptr.push - dut.logic.ptr.pop) % depth) - U(dut.logic.ptr.full & (dut.logic.ptr.push === dut.logic.ptr.pop)) * depth))
          // initially (1 << log2Up(depth)) - depth, plus what is inside the StreamFifo and the optional output stage
          assert(dut.logic.ptr.arb.fmax.fullTracker.value ===  ((1 << log2Up(depth)) - depth +^ ((depth  +^ dut.logic.ptr.push - dut.logic.ptr.pop) % depth)) +^ (U(dut.logic.ptr.full & (dut.logic.ptr.push === dut.logic.ptr.pop)) * depth) +^ U(dut.io.pop.valid & !Bool(withAsyncRead)))
        } else if (forFMax & (!isPow2(depth) || !dut.withExtraMsb) && !withAsyncRead) {
          // initially (1 << log2Up(depth)), minus what is inside the StreamFifo
          assert(dut.logic.ptr.arb.fmax.emptyTracker.value === ((1 << log2Up(depth)) - ((depth +^ dut.logic.ptr.push - dut.logic.ptr.pop) % depth)))
          // initially (1 << log2Up(depth)) - depth, plus what is inside the StreamFifo and the optional output stage
          assert(dut.logic.ptr.arb.fmax.fullTracker.value ===  ((1 << log2Up(depth)) - depth +^ ((depth  +^ dut.logic.ptr.push - dut.logic.ptr.pop) % depth)) +^ U(dut.io.pop.valid & !Bool(withAsyncRead)))
        // depth is power of two, and we use modulo (depth*2) arithmetic
        } else if (forFMax & dut.withExtraMsb) {
          // initially (1 << log2Up(depth)), minus what is inside the StreamFifo
          assert(dut.logic.ptr.arb.fmax.emptyTracker.value === ((1 << log2Up(depth)) - ((depth*2 +^ dut.logic.ptr.push - dut.logic.ptr.pop) % (depth*2))))
          // initially (1 << log2Up(depth)) - depth, plus what is inside the StreamFifo and the optional output stage
          assert(dut.logic.ptr.arb.fmax.fullTracker.value ===  ((1 << log2Up(depth)) - depth +^ ((depth*2 +^ dut.logic.ptr.push - dut.logic.ptr.pop) % (depth*2)) +^ U(dut.io.pop.valid & !Bool(withAsyncRead))))
        // TODO broken?
        } else if (dut.withExtraMsb) {
          //assert(false)
          assert(dut.logic.ptr.occupancy === (dut.logic.ptr.push - dut.logic.ptr.popOnIo))
          assert(dut.logic.ptr.full === ((dut.logic.ptr.push ^ dut.logic.ptr.popOnIo ^ depth) === 0))
          assert(dut.logic.ptr.empty === (dut.logic.ptr.push === dut.logic.ptr.pop))
        }
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
  for (depth <- List(3, 4, 7);
    withAsyncRead <- List(false, true);
    withBypass <- List(false, true);
    forFMax <- List(false, true);
    allowExtraMsb <- List(false, true);

  if !(!withAsyncRead && withBypass)) {
    formalTestStreamFifo(depth = depth,
      withAsyncRead = withAsyncRead,
      withBypass = withBypass,
      forFMax = forFMax,
      allowExtraMsb = allowExtraMsb
    )
  }
}
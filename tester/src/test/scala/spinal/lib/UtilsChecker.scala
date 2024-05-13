package spinal.lib

import spinal.core._
import spinal.core.formal._
// import spinal.lib.{StreamFifo, History, OHToUInt}
import spinal.lib.formal._

class PriorityMuxChecker extends SpinalFormalFunSuite {
  def testMain(width: Int, msbFirst: Boolean){
    FormalConfig
      .withBMC(3)
      .withCover(3)
      .doVerify(new Component {
        val sel = in Bits(width bits)
        val inputWidth = log2Up(width)
        val inputs = in Vec(Bits(inputWidth bits), width)
        val output = out(PriorityMux(sel, inputs, msbFirst))

        for(id <- 0 until inputs.size){
          assume(inputs(id) === id)
        }
        
        val selReorder = if(!msbFirst) sel else sel.asBools.reverse.asBits
        val inputsReorder = if(!msbFirst) inputs else inputs.reverse

        val selected = OhMux.or(OHMasking.firstV2(selReorder), inputsReorder)
        val expected = cloneOf(output)
        when(sel === 0){
          expected := inputsReorder.last
        }.otherwise{
          expected := selected
        }
        assert(expected === output)

        cover(sel === 0)        
        cover(sel =/= 0)
        cover(sel === sel.getAllTrue)
      })
  }

  test("check in out basic") {
    testMain(2, false)
  }

  test("check in out reverse") {
    testMain(2, true)
  }

  test("check 3 in out basic") {
    testMain(3, false)
  }

  test("check 3 in out reverse") {
    testMain(3, true)
  }
}

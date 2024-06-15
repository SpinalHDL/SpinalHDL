package spinal.core

import spinal.core.formal.{FormalConfig, SpinalFormalConfig, FormalDut}
import spinal.lib.formal.SpinalFormalFunSuite
import spinal.tester.SpinalAnyFunSuite

class SubdivideInTestErrors extends SpinalAnyFunSuite {
  import CheckTester._
  def testInvalidStrict(width: Int, sliceWidth: BitCount): Unit =
    test(s"$width bit Bits can't be divided into ${sliceWidth.value} bits evenly (strict)") {
      generationShouldFail(new Component {
        val x = in port Bits(width bit)
        x.subdivideIn(sliceWidth)
      })
    }
  def testInvalidStrict(width: Int, sliceCount: SlicesCount): Unit =
    test(s"$width bit Bits can't be divides into ${sliceCount.value} slices evenly (strict)") {
      generationShouldFail(new Component {
        val x = in port Bits(width bit)
        x.subdivideIn(sliceCount)
      })
    }

  testInvalidStrict(8, 7 bit)
  testInvalidStrict(9, 4 bit)
  testInvalidStrict(8, 3 slices)
  testInvalidStrict(9, 4 slices)
  testInvalidStrict(2, 3 slices) // more slices than possible
}

class SubdivideInTest extends SpinalFormalFunSuite {
  case class SliceDutBits(width: Int, sliceWidth: BitCount) extends Component {
    val i = in Bits(width bit)
    val o = out Bits(width bit)
    val split = i.subdivideIn(sliceWidth, strict = false)

    if (width % sliceWidth.value == 0)
      split.foreach(s => assert(s.getWidth == sliceWidth.value))
    else
      split.dropRight(1).foreach(s => assert(s.getWidth == sliceWidth.value))
    
    o := Cat(split.asInstanceOf[Iterable[Data]])    
    def formalAsserts() ={
      i <> in(cloneOf(i))
      assert(i === o)
    }
  }

  case class SliceDutSlices(width: Int, slicesCount: SlicesCount) extends Component {
    val i = in Bits(width bit)
    val o = out Bits(width bit)
    val split = i.subdivideIn(slicesCount, strict = false)    
    assert(split.size == slicesCount.value, s"split into ${split.size} slices, not ${slicesCount.value}")

    o := Cat(split.asInstanceOf[Iterable[Data]])
    def formalAsserts() = {
      i <> in(cloneOf(i))
      assert(i === o)
    }
  }
  
  test("mix of slicing operations") {
    SpinalFormalConfig(_keepDebugInfo = true)
      .withBMC(5)
      .withProve(5)
      .doVerify(new Component {
        val dut_8_2bit = FormalDut(SliceDutBits(8, 2 bit)) // default case
        dut_8_2bit.formalAsserts()
        val dut_8_5bit = FormalDut(SliceDutBits(8, 5 bit)) // uneven case, https://github.com/SpinalHDL/SpinalHDL/issues/1049
        dut_8_5bit.formalAsserts()
        val dut_9_4bit = FormalDut(SliceDutBits(9, 4 bit)) // odd width
        dut_9_4bit.formalAsserts()
        val dut_2_3bit = FormalDut(SliceDutBits(2, 3 bit)) // width smaller than sliceWidth
        dut_2_3bit.formalAsserts()
        val dut_8_2slices = FormalDut(SliceDutSlices(8, 2 slices)) // default case
        dut_8_2slices.formalAsserts()
        val dut_8_3slices = FormalDut(SliceDutSlices(8, 3 slices)) // uneven case
        dut_8_3slices.formalAsserts()
        val dut_9_4slices = FormalDut(SliceDutSlices(9, 4 slices)) // odd width
        dut_9_4slices.formalAsserts()
      }.setDefinitionName("BitVectorTests_SubdivideInTest"))
  }
}

package spinal.core

import spinal.core.formal.{FormalConfig, SpinalFormalConfig}
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
    val i = Bits(width bit)
    val o = Bits(width bit)
    val split = i.subdivideIn(sliceWidth, strict = false)

    if (width % sliceWidth.value == 0)
      split.foreach(s => assert(s.getWidth == sliceWidth.value))
    else
      split.dropRight(1).foreach(s => assert(s.getWidth == sliceWidth.value))

    o := Cat(split.asInstanceOf[Iterable[Data]])
    assert(i === o)
  }

  case class SliceDutSlices(width: Int, slicesCount: SlicesCount) extends Component {
    val i = Bits(width bit)
    val o = Bits(width bit)
    val split = i.subdivideIn(slicesCount, strict = false)
    assert(split.size == slicesCount.value, s"split into ${split.size} slices, not ${slicesCount.value}")

    o := Cat(split.asInstanceOf[Iterable[Data]])
    assert(i === o)
  }
  
  test("mix of slicing operations") {
    SpinalFormalConfig(_keepDebugInfo = true)
      .withBMC(5)
      .withProve(5)
      .doVerify(new Component {
        val dut_8_2bit = SliceDutBits(8, 2 bit) // default case
        val dut_8_5bit = SliceDutBits(8, 5 bit) // uneven case, https://github.com/SpinalHDL/SpinalHDL/issues/1049
        val dut_9_4bit = SliceDutBits(9, 4 bit) // odd width
        val dut_2_3bit = SliceDutBits(2, 3 bit) // width smaller than sliceWidth
        val dut_8_2slices = SliceDutSlices(8, 2 slices) // default case
        val dut_8_3slices = SliceDutSlices(8, 3 slices) // uneven case
        val dut_9_4slices = SliceDutSlices(9, 4 slices) // odd width
      }.setDefinitionName("BitVectorTests_SubdivideInTest"))
  }
}

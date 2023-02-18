package spinal.tester.code

import spinal.core._
import spinal.lib._

object PlayDevErrorReport{
  class Top extends Component {
    val and = out(False)
    withoutKeywords()
  }

  def main(args: Array[String]) {
    SpinalVerilog(new Top)
  }
}


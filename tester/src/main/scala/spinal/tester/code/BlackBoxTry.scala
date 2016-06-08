package spinal.tester.code

import spinal.core._
import spinal.lib._

/**
 * Created by PIC32F_USER on 20/06/2015.
 */

case class LamdaBlackBoxA(_bitsWidth: Int) extends BlackBoxULogic {
  val generic = new Generic {
    val bitsWidth = _bitsWidth
    val myTime = 2.3 ns
    val myTime2 = 42 ms
  }

  val io = new Bundle {
    val src = slave Stream (Bits(_bitsWidth bit))
    val dst = master Stream (Bits(_bitsWidth bit))
  }

}

case class LamdaBlackBoxB(_bitsWidth: Int) extends BlackBox {
  val src = slave Stream (Bits(_bitsWidth bit))
  val dst = master Stream (Bits(_bitsWidth bit))
}

case class LamdaComponentC(_bitsWidth: Int) extends Component {
  val io = new Bundle {
    val src = slave Stream (Bits(_bitsWidth bit))
    val dst = master Stream (Bits(_bitsWidth bit))
  }

  io.dst << io.src
}

case class LambdaComponent(bitsWidth: Int) extends Component {
  val io = new Bundle {
    val src = slave Stream (Bits(bitsWidth bit))
    val dst = master Stream (Bits(bitsWidth bit))

    val xx = ifGen(true)(
      new Bundle {
        val a: Bool = in Bool
        val b: Bool = out Bool
      }
    )
  }
  val blackBoxA = LamdaBlackBoxA(bitsWidth)
  val blackBoxB = LamdaBlackBoxB(bitsWidth)
  val componentC = LamdaComponentC(bitsWidth).io

  blackBoxA.io.src << io.src
  blackBoxB.src << blackBoxA.io.dst
  componentC.src << blackBoxB.dst

  io.dst << componentC.dst

  io.xx.b := io.xx.a


  import scala.concurrent.duration._

  Duration
}

object BlackBoxTry {
  def main(args: Array[String]) {
    SpinalVhdl(LambdaComponent(8))
  }
}

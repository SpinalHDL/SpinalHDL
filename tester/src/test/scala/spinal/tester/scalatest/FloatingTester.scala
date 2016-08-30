package spinal.tester.scalatest

import spinal.core._
import spinal.lib._
import spinal.lib.experimental.math._

class FloatingTester extends Component {
  val io = new Bundle {
    val inp = in(Floating(8, 23))
    val outp = out Bool
    val zero = out Bool
    val qnan = out Bool
    val snan = out Bool
    val positive = out Bool
    val infinite = out Bool
    val recoded_sign = out Bool
    val recoded_exp = out Bits(9 bits)
    val recoded_mantissa = out Bits(23 bits)

    val dec_sign = out Bool
    val dec_exponent = out Bits(8 bits)
    val dec_mantissa = out Bits(23 bits)
  }
  io.outp := io.inp.isZero
  val rec = io.inp.toRecFloating
  io.zero := rec.isZero
  io.qnan := rec.isQNaN
  io.snan := rec.isSNaN
  io.positive := rec.isPositive
  io.infinite := rec.isInfinite
  io.recoded_sign := rec.sign
  io.recoded_exp := rec.exponent
  io.recoded_mantissa := rec.mantissa

  val dec = rec.toFloating
  io.dec_exponent := dec.exponent
  io.dec_mantissa := dec.mantissa
  io.dec_sign := dec.sign
}

class FloatingTesterCocotb extends SpinalTesterCocotbBase {
  override def getName: String = "FloatingTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/FloatingTester"
  override def createToplevel: Component = new FloatingTester
  override def backendConfig(config: SpinalConfig): SpinalConfig = config.dumpWave()
}


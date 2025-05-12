package spinal.lib.experimental.math

import scala.math.BigDecimal

import spinal.core._
import spinal.lib._
import spinal.tester.SpinalTesterCocotbBase

class FloatingTester2 extends Component {
  val io = new Bundle {
    // Test Case: BigDecimal("0.0")
    val f_zero_bits = out Bits (32 bits)
    val rf_zero_sign = out Bool ()
    val rf_zero_exp = out Bits (9 bits)
    val rf_zero_mant = out Bits (23 bits)

  }

  // Test Case: BigDecimal("0.0")
  val f_zero = Floating32()
  f_zero := BigDecimal("0.0")
  io.f_zero_bits := f_zero.asBits

  val rf_zero = RecFloating32()
  rf_zero := BigDecimal("0.0")
  io.rf_zero_sign := rf_zero.sign
  io.rf_zero_exp := rf_zero.exponent
  io.rf_zero_mant := rf_zero.mantissa

}

class FloatingTester2Cocotb extends SpinalTesterCocotbBase {
  override def getName: String = "FloatingTester2"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/FloatingTester2"
  override def createToplevel: Component = new FloatingTester2
  override def backendConfig(config: SpinalConfig): SpinalConfig = config.dumpWave()
}

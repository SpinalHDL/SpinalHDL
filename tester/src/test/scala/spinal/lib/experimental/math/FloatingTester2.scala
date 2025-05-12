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

    // Test Case: BigDecimal("1.5")
    val f_one_point_five_bits = out Bits(32 bits)
    val rf_one_point_five_sign = out Bool()
    val rf_one_point_five_exp = out Bits(9 bits)
    val rf_one_point_five_mant = out Bits(23 bits)

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

  // Test Case: BigDecimal("1.5")
  val f_one_point_five = Floating32()
  f_one_point_five := BigDecimal("1.5")
  io.f_one_point_five_bits := f_one_point_five.asBits

  val rf_one_point_five = RecFloating32()
  rf_one_point_five := BigDecimal("1.5")
  io.rf_one_point_five_sign := rf_one_point_five.sign
  io.rf_one_point_five_exp := rf_one_point_five.exponent
  io.rf_one_point_five_mant := rf_one_point_five.mantissa

}

class FloatingTester2Cocotb extends SpinalTesterCocotbBase {
  override def getName: String = "FloatingTester2"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/FloatingTester2"
  override def createToplevel: Component = new FloatingTester2
  override def backendConfig(config: SpinalConfig): SpinalConfig = config.dumpWave()
}

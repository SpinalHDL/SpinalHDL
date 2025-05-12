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
    val f_one_point_five_bits = out Bits (32 bits)
    val rf_one_point_five_sign = out Bool ()
    val rf_one_point_five_exp = out Bits (9 bits)
    val rf_one_point_five_mant = out Bits (23 bits)

    // Test Case: BigDecimal("-2.0")
    val f_neg_two_bits = out Bits (32 bits)
    val rf_neg_two_sign = out Bool ()
    val rf_neg_two_exp = out Bits (9 bits)
    val rf_neg_two_mant = out Bits (23 bits)

    // Test Case: BigDecimal("0.125") (1/8)
    val f_one_eighth_bits = out Bits (32 bits)
    val rf_one_eighth_sign = out Bool ()
    val rf_one_eighth_exp = out Bits (9 bits)
    val rf_one_eighth_mant = out Bits (23 bits)

    // Test Case: A value that might stress the current normalization loop
    val small_val_num = BigDecimal("0.0009765625")
    val f_small_val_bits = out Bits (32 bits)
    val rf_small_val_sign = out Bool ()
    val rf_small_val_exp = out Bits (9 bits)
    val rf_small_val_mant = out Bits (23 bits)
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

  // Test Case: BigDecimal("-2.0")
  val f_neg_two = Floating32()
  f_neg_two := BigDecimal("-2.0")
  io.f_neg_two_bits := f_neg_two.asBits

  val rf_neg_two = RecFloating32()
  rf_neg_two := BigDecimal("-2.0")
  io.rf_neg_two_sign := rf_neg_two.sign
  io.rf_neg_two_exp := rf_neg_two.exponent
  io.rf_neg_two_mant := rf_neg_two.mantissa

  // Test Case: BigDecimal("0.125")
  val f_one_eighth = Floating32()
  f_one_eighth := BigDecimal("0.125")
  io.f_one_eighth_bits := f_one_eighth.asBits

  val rf_one_eighth = RecFloating32()
  rf_one_eighth := BigDecimal("0.125")
  io.rf_one_eighth_sign := rf_one_eighth.sign
  io.rf_one_eighth_exp := rf_one_eighth.exponent
  io.rf_one_eighth_mant := rf_one_eighth.mantissa

  // Test Case: BigDecimal from io.small_val_num
  val f_small_val = Floating32()
  f_small_val := io.small_val_num
  io.f_small_val_bits := f_small_val.asBits

  val rf_small_val = RecFloating32()
  rf_small_val := io.small_val_num
  io.rf_small_val_sign := rf_small_val.sign
  io.rf_small_val_exp := rf_small_val.exponent
  io.rf_small_val_mant := rf_small_val.mantissa
}

class FloatingTester2Cocotb extends SpinalTesterCocotbBase {
  override def getName: String = "FloatingTester2"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/FloatingTester2"
  override def createToplevel: Component = new FloatingTester2
  override def backendConfig(config: SpinalConfig): SpinalConfig = config.dumpWave()
}

package spinal.lib.experimental.math

import spinal.core._
import spinal.lib._
import spinal.tester.SpinalTesterCocotbBase

class FloatingTester extends Component {
  val io = new Bundle {
    val inp = in(Floating(8, 23))
    val outp = out Bool()
    val zero = out Bool()
    val qnan = out Bool()
    val snan = out Bool()
    val positive = out Bool()
    val infinite = out Bool()
    val recoded_sign = out Bool()
    val recoded_exp = out Bits(9 bits)
    val recoded_mantissa = out Bits(23 bits)

    val dec_sign = out Bool()
    val dec_exponent = out Bits(8 bits)
    val dec_mantissa = out Bits(23 bits)

    // UInt to Float conversion test
    val in_uint = in UInt(32 bits)
    val out_float = out(Floating32())
    val out_recfloat = out(RecFloating32())
    val out_uint_bits = out Bits()

    // SInt to Float conversion test
    val in_sint = in SInt(32 bits)
    val out_sint_bits = out Bits()

    // Float to UInt conversion
    val out_to_UInt = out UInt(32 bits)

    // Float to SInt conversion
    val out_to_SInt = out SInt(32 bits)

    val out_const = out(Floating32())
  }
  io.outp := io.inp.isZero

  // Recoded number information
  val rec = io.inp.toRecFloating
  io.zero := rec.isZero
  io.qnan := rec.isQNaN
  io.snan := rec.isSNaN
  io.positive := rec.isPositive
  io.infinite := rec.isInfinite

  // FP to recoded
  io.recoded_sign := rec.sign
  io.recoded_exp := rec.exponent
  io.recoded_mantissa := rec.mantissa

  // Recoded to normal FP
  val dec = rec.toFloating
  io.dec_exponent := dec.exponent
  io.dec_mantissa := dec.mantissa
  io.dec_sign := dec.sign

  // UInt to float test
  val recFloatImport = RecFloating32().fromUInt(io.in_uint)
  io.out_float := recFloatImport.toFloating
  io.out_recfloat := recFloatImport
  io.out_uint_bits := recFloatImport.toFloating.asBits
  io.out_to_UInt := recFloatImport.toUInt(32)

  // SInt to float test
  val recSIntFloatImport = RecFloating32().fromSInt(io.in_sint)
  io.out_sint_bits := recSIntFloatImport.toFloating.asBits
  io.out_to_SInt := recSIntFloatImport.toSInt(32)

  // BigDecimal test
  val floatBigint = Floating32()
  floatBigint := 446.0625
  io.out_const := floatBigint
}

class FloatingTesterCocotb extends SpinalTesterCocotbBase {
  override def getName: String = "FloatingTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/FloatingTester"
  override def createToplevel: Component = new FloatingTester
  override def backendConfig(config: SpinalConfig): SpinalConfig = config.dumpWave()
}


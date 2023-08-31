package spinal.core

import spinal.core.sim._
import spinal.tester.SpinalAnyFunSuite

import java.math.MathContext
import scala.collection.mutable
import scala.math.BigDecimal.RoundingMode
import scala.util.Random

class SpinalSimAFixTester extends SpinalAnyFunSuite {
  def check(max : Int, min : Int, exp : Int)(f : AFix): Unit ={
    assert(f.maxRaw == max && f.minRaw == min && f.exp == exp)
  }
  test("instantiate") {
    SpinalVerilog(new Component{

      check(4095,0,0)(AFix.U(12 bits)) //Q12.0
      check(4095,0,-4)(AFix.U(8 exp, 12 bits)) //Q8.4
      check(4095,0,-4)(AFix.U(8 exp, -4 exp)) //Q8.4
      check(4095,0,-4)(AFix.UQ(8 bits, 4 bits)) //Q8.4
//      check(4095,0,-4)(AFix.U(255, -4 exp)) //Q8.4
//      check(4095,2048,-4)(AFix.U(255, 128, -4 exp)) //Q8.4
      check(2047,-2048,0)(AFix.S(12 bits)) //Q11.0 + sign bit
      check(2047,-2048,-4)(AFix.S(7 exp, 12 bits)) //Q7.4  + sign bit
      check(2047,-2048,-4)(AFix.S(7 exp, -4 exp)) //Q7.4  + sign bit
      check(2047,-2048,-4)(AFix.SQ(7 bits, 4 bits)) //Q8.4 + sign bit
//      check(2047,-2048,-4)(AFix.S(127, -128, -4 exp)) //Q7.4 + sign bit
    })
  }

  test("assign") {
    SpinalVerilog(new Component{
      val u_8_4_a = AFix.U(8 exp, -4 exp)
      val u_8_4_b = AFix.U(8 exp, -4 exp)
      val u_10_4_b = AFix.U(10 exp, -4 exp)
      val u_8_6_b = AFix.U(8 exp, -6 exp)
      val u_6_4_b = AFix.U(6 exp, -4 exp)
      val u_8_2_b = AFix.U(8 exp, -2 exp)

      u_8_4_b := u_8_4_a
      u_10_4_b := u_8_4_a
      u_8_6_b := u_8_4_a
      u_6_4_b := u_8_4_a.truncated(saturation = true, overflow = false, rounding = RoundType.FLOOR)
      u_8_2_b := u_8_4_a.truncated(saturation = true, overflow = false, rounding = RoundType.FLOOR)
    })
  }

  test("random") {
    SimConfig.compile(new AFixTester()).doSim(seed = 0) { dut =>
      val OPS = 0 to 2
      val ROUNDS = 0 to 9
      for(op <- OPS) {
        for(round <- ROUNDS) {
          for(i <- 0 until 1000) {
            dut.io.inFix.foreach(_.randomize())
            dut.io.opMode #= op
            dut.io.roundMode #= round
            sleep(1)
            assert(checkCalc(dut), dutStateString(dut))
          }
        }
      }
    }
  }

  test("sweep") {
    SimConfig.compile(new AFixTester()).doSim(seed = 0) { dut =>
      val OPS = 0 to 2
      val ROUNDS = 0 to 9
      for(op <- OPS) {
        for(round <- ROUNDS) {
          for(x <- -BigInt(2).pow(dut.io.inFix(0).fracWidth)*3 until BigInt(2).pow(dut.io.inFix(0).fracWidth)*3) {
            for (y <- -BigInt(2).pow(dut.io.inFix(1).fracWidth)*3 until BigInt(2).pow(dut.io.inFix(1).fracWidth)*3) {
              dut.io.inFix(0) #= BigDecimal(x) / BigDecimal(2).pow(dut.io.inFix(0).fracWidth)
              dut.io.inFix(1) #= BigDecimal(y) / BigDecimal(2).pow(dut.io.inFix(1).fracWidth)
              dut.io.opMode #= op
              dut.io.roundMode #= round
              sleep(1)
              assert(checkCalc(dut), dutStateString(dut))
            }
          }
        }
      }
    }
  }

  def dutStateString(dut: AFixTester): String = {
    val model = AFixTesterModel(dut)

    val OP_CHAR = List("+", "-", "*")
    val ROUNDING_NAME = List(
      "CEIL",
      "FLOOR",
      "CEIL_INF",
      "FLOOR_ZERO",
      "HALF_UP",
      "HALF_DOWN",
      "HALF_INF",
      "HALF_ZERO",
      "HALF_EVEN",
      "HALF_ODD"
    )

    val a_bin = model.a_int.toString(2)
    val b_bin = model.b_int.toString(2)
    val r_bin = model.result_int.toString(2)
    val a_deci_str = model.a.toString()
    val b_deci_str = model.b.toString()
    val r_deci_str = model.result.toString()

    val padWidth = List(
        a_bin,
        b_bin,
        r_bin,
        a_deci_str,
        b_deci_str,
        r_deci_str).map(_.length).max

    val a_bin_pad = a_bin.reverse.padTo(dut.io.inFix(0).bitWidth, '0').padTo(padWidth, ' ').reverse
    val b_bin_pad = b_bin.reverse.padTo(dut.io.inFix(1).bitWidth, '0').padTo(padWidth, ' ').reverse
    val r_bin_pad = r_bin.reverse.padTo(dut.io.outFix.bitWidth, '0').padTo(padWidth, ' ').reverse
    val a_deci_pad = a_deci_str.reverse.padTo(padWidth, ' ').reverse
    val b_deci_pad = b_deci_str.reverse.padTo(padWidth, ' ').reverse
    val r_deci_pad = r_deci_str.reverse.padTo(padWidth, ' ').reverse

    val resStringBuilder = new mutable.StringBuilder()
    resStringBuilder ++= "\n"
    resStringBuilder ++= s"$a_bin_pad ${OP_CHAR(model.op)} $b_bin_pad = $r_bin_pad\n"
    resStringBuilder ++= s"$a_deci_pad ${OP_CHAR(model.op)} $b_deci_pad = $r_deci_pad\n"

    val addRaw = dut.io.outAdd.raw.toBigInt
    val subRaw = dut.io.outSub.raw.toBigInt
    val mulRaw = dut.io.outMul.raw.toBigInt

    resStringBuilder ++= s"Add = ${addRaw.toString(2).reverse.padTo(dut.io.outAdd.bitWidth, '0').reverse}\n"
    resStringBuilder ++= s"Sub = ${subRaw.toString(2).reverse.padTo(dut.io.outSub.bitWidth, '0').reverse}\n"
    resStringBuilder ++= s"Mul = ${mulRaw.toString(2).reverse.padTo(dut.io.outMul.bitWidth, '0').reverse}\n"

    resStringBuilder ++= s"c = ${model.c.toString()}\n"
    resStringBuilder ++= s"rounding = ${ROUNDING_NAME(model.mode)} (${model.mode})\n"
    resStringBuilder ++= s"ceil = ${model.c.setScale(0, RoundingMode.CEILING)}\n"
    resStringBuilder ++= s"floor = ${model.c.setScale(0, RoundingMode.FLOOR)}\n"
    resStringBuilder ++= s"half_up = ${model.c.setScale(0, RoundingMode.HALF_UP)}\n"
    resStringBuilder ++= s"half_down = ${model.c.setScale(0, RoundingMode.HALF_DOWN)}\n"

    resStringBuilder ++= s"rounded = ${model.rounded.toString()}\n"

    resStringBuilder.toString()
  }

  def checkCalc(dut: AFixTester): Boolean = {
    val model = AFixTesterModel(dut)
    model.result.equals(model.rounded)
  }

  case class AFixTesterModel(dut: AFixTester) {
    val a_int = dut.io.inFix(0).raw.toBigInt.abs
    val a = if (dut.io.inFix(0).raw.toBigInt.testBit(dut.io.inFix(0).numWidth))
      -BigDecimal(BigInt(a_int.toString(2).map(c => if (c == '0') '1' else '0'), 2) + 1) * BigDecimal(2).pow(dut.io.inFix(0).exp)
    else
      BigDecimal(a_int)*BigDecimal(2).pow(dut.io.inFix(0).exp)

    val b_int = dut.io.inFix(1).raw.toBigInt.abs
    val b = if (dut.io.inFix(1).raw.toBigInt.testBit(dut.io.inFix(1).numWidth))
      -BigDecimal(BigInt(b_int.toString(2).map(c => if (c == '0') '1' else '0'), 2) + 1)*BigDecimal(2).pow(dut.io.inFix(1).exp)
    else
      BigDecimal(b_int)*BigDecimal(2).pow(dut.io.inFix(1).exp)

    val result_int = dut.io.outFix.raw.toBigInt.abs
    val result = if (dut.io.outFix.raw.toBigInt.testBit(dut.io.outFix.numWidth))
      -BigDecimal(BigInt(result_int.toString(2).map(c => if (c == '0') '1' else '0'), 2) + 1)*BigDecimal(2).pow(dut.io.outFix.exp)
    else
      BigDecimal(result_int)*BigDecimal(2).pow(dut.io.outFix.exp)

    val op = dut.io.opMode.toInt
    val mode = dut.io.roundMode.toInt

    assert(op >= 0 && op < 3, "DUT op mode out of range!")
    assert(mode >= 0 && mode < 10, "DUT rounding mode out of range!")

    val c: BigDecimal = op match {
      case 0 => (a + b) % BigDecimal(2).pow(dut.io.outAdd.bitWidth)
      case 1 => (a - b) % BigDecimal(2).pow(dut.io.outSub.bitWidth)
      case 2 => (a * b) % BigDecimal(2).pow(dut.io.outMul.bitWidth)
    }

    val rounded = doAFixSimRound(c, mode)
  }

  test("round") {
    val ROUNDING_EXAMPLES = List(-16.5625, -16.5, -16.4375, -0.0625, 0, 0.0625, 16.4375, 16.5, 16.5625)
    val ROUNDING_EXP = 4
    val ROUNDING_MODES = 0 until 9
    SimConfig.compile(new Component {
      for (mode <- ROUNDING_MODES) {
        for (example <- ROUNDING_EXAMPLES) {
          val deciBounds = List(example, example - 5)
          val fixedBounds = deciBounds.map(_ * BigDecimal(2).pow(ROUNDING_EXP)).map(_.toBigInt)
          val fixNum = new AFix(fixedBounds.max, fixedBounds.min, -ROUNDING_EXP)

          val roundedNum = mode match {
            case 0 => fixNum.ceil(0)
            case 1 => fixNum.floor(0)
            case 2 => fixNum.floorToZero(0)
            case 3 => fixNum.ceilToInf(0)
            case 4 => fixNum.roundHalfUp(0)
            case 5 => fixNum.roundHalfDown(0)
            case 6 => fixNum.roundHalfToZero(0)
            case 7 => fixNum.roundHalfToInf(0)
            case 8 => fixNum.roundHalfToEven(0)
            case 9 => fixNum.roundHalfToOdd(0)
          }

          val roundedBounds = deciBounds.map(b => doAFixSimRound(b, mode))

          assert(roundedNum.maxValue == roundedBounds.max || roundedNum.minValue == roundedBounds.min,
          s"\nRounded bounds check failed. AFix [${roundedNum.minValue} - ${roundedNum.maxValue}] vs Model [${roundedBounds.min} - ${roundedBounds.max}]. Rounding mode = ${mode}")
        }
      }
    })
  }

  def doAFixSimRound(c: BigDecimal, roundMode: Int): BigDecimal = {
    roundMode match {
      case 0 => c.setScale(0, RoundingMode.CEILING)
      case 1 => c.setScale(0, RoundingMode.FLOOR)
      case 2 => if (c.signum == 1) c.setScale(0, RoundingMode.FLOOR) else c.setScale(0, RoundingMode.CEILING)
      case 3 => if (c.signum == 1) c.setScale(0, RoundingMode.CEILING) else c.setScale(0, RoundingMode.FLOOR)
      case 4 => if (c.signum == 1) c.setScale(0, RoundingMode.HALF_UP) else c.setScale(0, RoundingMode.HALF_DOWN)
      case 5 => if (c.signum == 1) c.setScale(0, RoundingMode.HALF_DOWN) else c.setScale(0, RoundingMode.HALF_UP)
      case 6 => c.setScale(0, RoundingMode.HALF_DOWN)
      case 7 => c.setScale(0, RoundingMode.HALF_UP)
      case 8 => if (c.signum == 1)
        if (c % 2 < 1) c.setScale(0, RoundingMode.HALF_DOWN) else c.setScale(0, RoundingMode.HALF_UP)
      else
        if (-c % 2 < 1) c.setScale(0, RoundingMode.HALF_DOWN) else c.setScale(0, RoundingMode.HALF_UP)
      case 9 => if (c.signum == 1)
        if (c % 2 < 1) c.setScale(0, RoundingMode.HALF_UP) else c.setScale(0, RoundingMode.HALF_DOWN)
      else
        if (-c % 2 < 1) c.setScale(0, RoundingMode.HALF_UP) else c.setScale(0, RoundingMode.HALF_DOWN)
    }
  }
}

class AFixTester extends Component {
  val io = new Bundle {
    val inFix = in(Vec(Seq(new AFix(32767, -32768, -4),
      new AFix(32767, -32768, -6))))
    val outRaw = out(new AFix(68719476735L, -68719476736L, -13))
    val outFix = out(new AFix(8388607, -8388608, 0))
    val opMode = in(Bits(2 bit))
    val roundMode = in(Bits(4 bit))

    val outAdd = out((inFix(0) + inFix(1)).clone)
    val outSub = out((inFix(0) - inFix(1)).clone)
    val outMul = out((inFix(0) * inFix(1)).clone)
  }

  val opResultsSeq = Seq(
    io.inFix(0) + io.inFix(1),
    io.inFix(0) - io.inFix(1),
    io.inFix(0) * io.inFix(1)
  )
  io.outAdd := opResultsSeq(0)
  io.outSub := opResultsSeq(1)
  io.outMul := opResultsSeq(2)

  for (res <- opResultsSeq) {
    println(res)
  }
  val rangeExpMin = opResultsSeq.minBy(_.exp).exp
  val rangeMax = opResultsSeq.map(af => af.maxRaw*BigInt(2).pow(af.exp - rangeExpMin)).max
  val rangeMin = opResultsSeq.map(af => af.minRaw*BigInt(2).pow(af.exp - rangeExpMin)).min
  val opResults = Vec(opResultsSeq.map(af => {
    val resized_af = new AFix(rangeMax, rangeMin, rangeExpMin)
    resized_af := af
    resized_af
  }))

  val chosenOp = opResults(io.opMode.asUInt)
  println(chosenOp)
  io.outRaw := chosenOp.saturated

  val roundResults = Vec(Seq(
    chosenOp.ceil(0),
    chosenOp.floor(0),
    chosenOp.floorToZero(0),
    chosenOp.ceilToInf(0),
    chosenOp.roundHalfUp(0),
    chosenOp.roundHalfDown(0),
    chosenOp.roundHalfToZero(0),
    chosenOp.roundHalfToInf(0),
    chosenOp.roundHalfToEven(0),
    chosenOp.roundHalfToOdd(0)
  ))

  io.outFix := roundResults(io.roundMode.asUInt).sat(io.outFix)
}

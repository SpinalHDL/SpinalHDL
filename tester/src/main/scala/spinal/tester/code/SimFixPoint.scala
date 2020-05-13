package spinal.tester.code

import spinal.core._
import spinal.core.sim._
import spinal.lib.dsptool._
import spinal.core.RoundType._

class SIntRoundTest(roundType: RoundType, sym: Boolean) extends Component{
  val din = in SInt(16 bits)
  val d1_0  = out(din.fixTo(1  downto 0, roundType, sym))
  val d7_0  = out(din.fixTo(7  downto 0, roundType, sym))
  val d15_8 = out(din.fixTo(15 downto 8, roundType, sym))
  val d13_4 = out(din.fixTo(13 downto 4, roundType, sym))
  val d4_2  = out(din.fixTo(4  downto 2, roundType, sym))
  val d15_0 = out(din.fixTo(15 downto 0, roundType, sym))
  val d14_1 = out(din.fixTo(14 downto 1, roundType, sym))
  val d20_3 = out(din.fixTo(20 downto 3, roundType, sym))
}

class SIntFixTo(inQ: QFormat, outQ: QFormat, roundType: RoundType, sym: Boolean) extends Component{
  val din = in SInt(inQ.width bits)
  val dout = out SInt(outQ.width bits)

  dout := din.tag(inQ).fixTo(outQ, roundType, sym)
}

class UIntRoundTest(roundType: RoundType) extends Component{
  val din = in UInt(16 bits)
  val d0    = out(din.fixTo(0  downto 0, roundType))
  val d7_0  = out(din.fixTo(7  downto 0, roundType))
  val d15_8 = out(din.fixTo(15 downto 8, roundType))
  val d13_4 = out(din.fixTo(13 downto 4, roundType))
  val d4_2  = out(din.fixTo(4  downto 2, roundType))
  val d15_0 = out(din.fixTo(15 downto 0, roundType))
  val d14_1 = out(din.fixTo(14 downto 1, roundType))
  val d20_3 = out(din.fixTo(20 downto 3, roundType))
}

class UIntFixTo(inQ: QFormat, outQ: QFormat, roundType: RoundType) extends Component{
  val din = in UInt(inQ.width bits)
  val dout = out UInt(outQ.width bits)

  dout := din.tag(inQ).fixTo(outQ, roundType)
}

object SimFP {
  val spinalConfig = SpinalConfig(
    mode = Verilog,
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
      clockEdge = RISING,
      resetActiveLevel = LOW),
    defaultClockDomainFrequency = FixedFrequency(200 MHz),
    targetDirectory="rtl/")

  val Simfp = SimConfig
    .withConfig(spinalConfig)
    .withWave
    .allOptimisation
    .workspacePath("./simWorkspace")
}

object fixToSectionRegression {
  def main(args: Array[String]): Unit = {
    val roundList = List(CEIL,FLOOR,FLOORTOZERO,CEILTOINF,ROUNDUP,ROUNDDOWN,ROUNDTOZERO,ROUNDTOINF)
    //UInt-test
    for(roundType <- roundList){
      SimFP.Simfp
        .compile(new UIntRoundTest(roundType))
        .doSim{ dut =>
          println(s"Sign: UnSigned, roundType: ${roundType}")
          val rand = new scala.util.Random(seed = 0)
          for(i <- 0 to 2000){
            val source = FixData(rand.nextGaussian*scala.math.pow(2,15),UQ(16,0),RoundType.ROUNDTOINF)
            val ret = if(source.isNegative) -source else source
            dut.din #=  ret.asLong
            sleep(5)
            assert(dut.d0.toLong    == (ret>>0).fixTo(UQ(1, 0),roundType).asLong)
            assert(dut.d7_0.toLong  == (ret>>0).fixTo(UQ(8, 0),roundType).asLong)
            assert(dut.d15_8.toLong == (ret>>8).fixTo(UQ(8, 0),roundType).asLong)
            assert(dut.d13_4.toLong == (ret>>4).fixTo(UQ(10,0),roundType).asLong)
            assert(dut.d4_2.toLong  == (ret>>2).fixTo(UQ(3, 0),roundType).asLong)
            assert(dut.d15_0.toLong == (ret>>0).fixTo(UQ(16,0),roundType).asLong)
            assert(dut.d14_1.toLong == (ret>>1).fixTo(UQ(14,0),roundType).asLong)
            assert(dut.d20_3.toLong == (ret>>3).fixTo(UQ(18,0),roundType).asLong)
          }
        }
    }
    //SInt-test
    for(symmetric <- List(true, false)){
      for(roundType <- roundList){
        SimFP.Simfp
          .compile(new SIntRoundTest(roundType,symmetric))
          .doSim{ dut =>
            println(s"Sign: Signed, roundType: ${roundType}, sym: ${symmetric}")
            val rand = new scala.util.Random(seed = 0)
            for(i <- 0 to 2000){
              val source = FixData(rand.nextGaussian*scala.math.pow(2,15),SQ(16,0))
              val ret = source
              dut.din #=  ret.asLong
              sleep(5)
              assert(dut.d1_0.toLong  == (ret>>0).fixTo(SQ(2, 0),roundType, symmetric).asLong)
              assert(dut.d7_0.toLong  == (ret>>0).fixTo(SQ(8, 0),roundType, symmetric).asLong)
              assert(dut.d15_8.toLong == (ret>>8).fixTo(SQ(8, 0),roundType, symmetric).asLong)
              assert(dut.d13_4.toLong == (ret>>4).fixTo(SQ(10,0),roundType, symmetric).asLong)
              assert(dut.d4_2.toLong  == (ret>>2).fixTo(SQ(3, 0),roundType, symmetric).asLong)
              assert(dut.d15_0.toLong == (ret>>0).fixTo(SQ(16,0),roundType, symmetric).asLong)
              assert(dut.d14_1.toLong == (ret>>1).fixTo(SQ(14,0),roundType, symmetric).asLong)
              assert(dut.d20_3.toLong == (ret>>3).fixTo(SQ(18,0),roundType, symmetric).asLong)
            }
          }
        //todo: SInt.roundDown detail test
      }
    }
  }
}

object fixToQFormatRegression extends  App{
  val roundList = List(CEIL,FLOOR,FLOORTOZERO,CEILTOINF,ROUNDUP,ROUNDDOWN,ROUNDTOZERO,ROUNDTOINF)
  //fixTo(SQ()) test
  for(symmetric <- List(true, false)) {
    for (roundType <- roundList) {
      val dinQ = SQ(16,8)
      val doutQs = List(SQ(9,8), SQ(16,8), SQ(10,4), SQ(10,0), SQ(5,2))
//      val doutQs = List(SQ(10,4))
      for(doutQ <- doutQs){
        SimFP.Simfp
          .compile(new SIntFixTo(dinQ, doutQ, roundType,symmetric))
          .doSim { dut =>
            SpinalProgress(s"Signed, ${roundType}, sym: ${symmetric}, ${dinQ}, ${doutQ}")
            val rand = new scala.util.Random(seed = 0)
            for(i <- 0 to 2000) {
              val data = FixData(rand.nextGaussian * (1 << dinQ.amplify), dinQ)
              dut.din #= data.asLong
              sleep(1)
//              println(data.value, toFixData(dut.dout.toLong.toInt, doutQ).value, data.fixTo(doutQ, roundType, symmetric).value)
              assert(dut.dout.toLong == data.fixTo(doutQ, roundType, symmetric).asLong)
            }
          }
      }
    }
  }
  //fixTo(UQ()) test
  for (roundType <- roundList) {
    val dinQ = UQ(16,8)
    val doutQs = List(UQ(8,8), UQ(16,8), UQ(10,4), UQ(10,0), UQ(5,2))
//    val doutQs = List(UQ(10,4))
    for(doutQ <- doutQs){
      SimFP.Simfp
        .compile(new UIntFixTo(dinQ, doutQ, roundType))
        .doSim { dut =>
          SpinalProgress(s"UnSigned, roundType: ${roundType}, ${dinQ}, ${doutQ}")
          val rand = new scala.util.Random(seed = 0)
          for(i <- 0 to 2000) {
            import scala.math.abs
            val data = FixData(abs(rand.nextGaussian) * (1 << (dinQ.amplify-1)), dinQ)
            dut.din #= data.asLong
            sleep(1)
//            println(data.value, toFixData(dut.dout.toLong.toInt, doutQ).value)
            assert(dut.dout.toLong == data.fixTo(doutQ, roundType).asLong)
          }
        }
    }
  }
}

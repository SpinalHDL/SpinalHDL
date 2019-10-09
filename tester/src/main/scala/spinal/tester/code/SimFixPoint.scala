package spinal.tester.code

import spinal.core._
import spinal.core.sim._
import spinal.lib.dsptool._
import spinal.core.RoundType._

class SIntRoundTest(roundType: RoundType, sym: Boolean) extends Component{
  val din = in SInt(16 bits)
  val d7_0  = out(din.fixTo(7  downto 0, roundType, sym))
  val d15_8 = out(din.fixTo(15 downto 8, roundType, sym))
  val d13_4 = out(din.fixTo(13 downto 4, roundType, sym))
  val d4_2  = out(din.fixTo(4  downto 2, roundType, sym))
  val d15_0 = out(din.fixTo(15 downto 0, roundType, sym))
  val d14_1 = out(din.fixTo(14 downto 1, roundType, sym))
}

class UIntRoundTest(roundType: RoundType) extends Component{
  val din = in UInt(16 bits)
  val d7_0  = out(din.fixTo(7  downto 0, roundType))
  val d15_8 = out(din.fixTo(15 downto 8, roundType))
  val d13_4 = out(din.fixTo(13 downto 4, roundType))
  val d4_2  = out(din.fixTo(4  downto 2, roundType))
  val d15_0 = out(din.fixTo(15 downto 0, roundType))
  val d14_1 = out(din.fixTo(14 downto 1, roundType))
}

object fixTests {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
        clockEdge = RISING,
        resetActiveLevel = LOW),
      defaultClockDomainFrequency = FixedFrequency(200 MHz),
      targetDirectory="rtl/")
    val roundList = List(Ceil,Floor,FloorToZero,CeilToInf,RoundUp,RoundDown,RoundToZero,RoundToInf)
    //UInt-test
    for(roundType <- roundList){
      val signed = false
      SimConfig
        .withConfig(spinalConfig)
        .withWave
        .allOptimisation
        .workspacePath("./simWorkspace")
        .compile(new UIntRoundTest(roundType))
        .doSim{ dut =>
          println(s"Sign: ${signed}, roundType: ${roundType}")
          val rand = new scala.util.Random(seed = 0)
          for(i <- 0 to 2000){
            val source = FixData(rand.nextGaussian*scala.math.pow(2,15),UQ(16,0))
            val ret = if(source.isNegative) -source else source
            dut.din #=  ret.asLong
            sleep(1)
            assert(dut.d7_0.toLong  == (ret>>0).fixTo(QFormat(8, 0,signed),roundType).asLong)
            assert(dut.d15_8.toLong == (ret>>8).fixTo(QFormat(8, 0,signed),roundType).asLong)
            assert(dut.d13_4.toLong == (ret>>4).fixTo(QFormat(10,0,signed),roundType).asLong)
            assert(dut.d4_2.toLong  == (ret>>2).fixTo(QFormat(3, 0,signed),roundType).asLong)
            assert(dut.d15_0.toLong == (ret>>0).fixTo(QFormat(16,0,signed),roundType).asLong)
            assert(dut.d14_1.toLong == (ret>>1).fixTo(QFormat(14,0,signed),roundType).asLong)
          }
        }
    }
    //SInt-test
    for(roundType <- roundList){
      val signed = true
      SimConfig
        .withConfig(spinalConfig)
        .withWave
        .allOptimisation
        .workspacePath("./simWorkspace")
        .compile(new SIntRoundTest(roundType,false))
        .doSim{ dut =>
          println(s"Sign: ${signed}, roundType: ${roundType}")
          val rand = new scala.util.Random(seed = 0)
          for(i <- 0 to 2000){
            val source = FixData(rand.nextGaussian*scala.math.pow(2,15),SQ(16,0))
            val ret = source
            dut.din #=  ret.asLong
            sleep(1)
            assert(dut.d7_0.toLong  == (ret>>0).fixTo(QFormat(8, 0,signed),roundType).asLong)
            assert(dut.d15_8.toLong == (ret>>8).fixTo(QFormat(8, 0,signed),roundType).asLong)
            assert(dut.d13_4.toLong == (ret>>4).fixTo(QFormat(10,0,signed),roundType).asLong)
            assert(dut.d4_2.toLong  == (ret>>2).fixTo(QFormat(3, 0,signed),roundType).asLong)
            assert(dut.d15_0.toLong == (ret>>0).fixTo(QFormat(16,0,signed),roundType).asLong)
            assert(dut.d14_1.toLong == (ret>>1).fixTo(QFormat(14,0,signed),roundType).asLong)
          }
        }
      //todo: SInt.roundDown detail test
    }
  }
}

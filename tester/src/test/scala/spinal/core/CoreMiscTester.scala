package spinal.core

import spinal.core.sim._
import spinal.lib._
import spinal.tester.SpinalAnyFunSuite
import scala.util.Random

object CoreMiscTesterObj extends AreaObject{
  val miaou = new Area{

  }
}

object CoreMiscTesterObj2 extends AreaRoot{
  val miaou = new Area{

  }
}

class CoreMiscTester extends SpinalAnyFunSuite{

  test("AreaObject") {
    assert(CoreMiscTesterObj.miaou.getName() == "CoreMiscTesterObj_miaou")
    assert(CoreMiscTesterObj2.miaou.getName() == "miaou")
  }

  test("SlowArea"){
    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(4000 Hz))).compile(new Component{
      val counter = out(RegInit(U"0000"))
      counter := counter + 1
      assert(clockDomain.samplingRate.getValue.toInt == 4000)

      val slowArea = new SlowArea(4){
        val counter = out(RegInit(U"0000"))
        counter := counter + 1
        assert(clockDomain.samplingRate.getValue.toInt == 1000)
      }

    }).doSim{dut =>
      dut.clockDomain.forkStimulus(10)

      for(i <- 0 until 1000){
        dut.clockDomain.waitSampling()
        assert(dut.counter.toInt == i % 16)
        assert(dut.slowArea.counter.toInt == (i-1)/4 % 16)
      }
    }
  }

  test("reg_nextValue"){
    SimConfig.compile(new Component{
      val conds = in(Vec.fill(8)(Bool()))
      val a, b, c, d, e = out(Reg(UInt(8 bits)) init(0))

      when(conds(0)){
        a := 1
        when(conds(1)){
          a := 2
          b := 11
        } otherwise {
          a := 3
          c := 21
        }
      }
      when(conds(2)){
        a := 4
        b := 12
        c := 22
        d := 31
      }

      val x = out(a.getAheadValue)
      val y = out(a.getAheadValue)
      val z = out(b.getAheadValue)

      when(d.getAheadValue() === 0){
        e := 1
      }
    }).doSim(seed = 42){dut =>
      var an,bn,cn,a,b,c = 0
      dut.conds.foreach(_ #= false)
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling()

      for(i <- 0 until 1000){
        if(dut.conds(0).toBoolean){
          an = 1
          if(dut.conds(1).toBoolean){
            an = 2
            bn = 11
          } else {
            an = 3
            cn = 21
          }
        }
        if(dut.conds(2).toBoolean){
          an = 4
          bn = 12
          cn = 22
        }
        assert(dut.x.toInt == an)
        assert(dut.y.toInt == an)
        assert(dut.z.toInt == bn)
        assert(dut.a.toInt == a)
        assert(dut.b.toInt == b)
        assert(dut.c.toInt == c)
        a = an
        b = bn
        c = cn
        dut.clockDomain.waitSampling()
        dut.conds.foreach(_.randomize())
      }
    }
  }

  test("uint_wrap_comparison"){
    val width = 8
    SimConfig.compile(new Component{
      val smaller, bigger, eq_smaller, eq_bigger = out(Bool())
      val x, y = in(UInt(width bits))

      smaller := x.wrap < y
      bigger := x.wrap > y
      eq_smaller := x.wrap <= y
      eq_bigger := x.wrap >= y
    }).doSim(seed = 42){dut =>      
      val quarter = 1<<(width - 2)
      for(i <- 0 until 2000){
        dut.x.randomize()
        sleep(1)
        var x = dut.x.toInt
        var value = x + Random.nextInt(2*quarter) - quarter // ensure two datas' distance are not beyond a quarter.
        value = if (value < 0) -value else value
        value %= 4*quarter
        dut.y #= value
        sleep(1)
        val y = dut.y.toInt
        val needReverse = (x - y).abs > 3* quarter

        assert(dut.smaller.toBoolean == (if(needReverse) x > y else x < y))
        assert(dut.bigger.toBoolean == (if(needReverse) x < y else x > y))
        assert(dut.eq_smaller.toBoolean == (if(needReverse) x >= y else x <= y))
        assert(dut.eq_bigger.toBoolean == (if(needReverse) x <= y else x >= y))
        sleep(1)
      }
    }
  }

  test("reverse_by_shuffle"){
    SimConfig.compile(new Component{
      val data = in(Bits(64 bits))
      val outData = out(UInt(64 bits))

      val outSet = data.subdivideIn(8 bits)
      outData := outSet.shuffle{ (i) =>
        outSet.length - 1 - i
      }.asBits.asUInt
    }).doSim(seed = 42){dut =>
      for(j <- 0 until 1000) {
        dut.data.randomize()
        sleep(1)
        val data = dut.data.toBigInt
        val outData = dut.outData.toBigInt
        for ( i <- 0 until 8) {
          val in = (data >> i * 8) & 0xFF
          val out = (outData >> (7-i) *8) & 0xFF
          assert(in == out)
        }
      }
    }
  }

  test("reverse_by_shuffle_with_size"){
    SimConfig.compile(new Component{
      val data = in(Bits(64 bits))
      val outData = out(UInt(64 bits))

      outData := data.subdivideIn(8 bits).shuffleWithSize{ (total, i) =>
        total - 1 - i
      }.asBits.asUInt
    }).doSim(seed = 42){dut =>
      for(j <- 0 until 1000) {
        dut.data.randomize()
        sleep(1)
        val data = dut.data.toBigInt
        val outData = dut.outData.toBigInt
        for ( i <- 0 until 8) {
          val in = (data >> i * 8) & 0xFF
          val out = (outData >> (7-i) *8) & 0xFF
          assert(in == out)
        }
      }
    }
  }

}

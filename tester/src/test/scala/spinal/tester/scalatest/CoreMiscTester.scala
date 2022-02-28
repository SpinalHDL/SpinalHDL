package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
class CoreMiscTester extends AnyFunSuite{
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
}

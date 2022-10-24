package spinal.tester.code

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import spinal.lib.bus.regif._

class RegFileIntrExample extends Component{
  val io = new Bundle{
    val apb = slave(Apb3(Apb3Config(16,32)))
    val int_pulse0, int_pulse1, int_pulse2 = in Bool()
    val int_level0, int_level1 = in Bool()
    val sys_int = out Bool()
    val s2 = out Bool()
    val gpio_int = out Bool()

  }

  val busif = BusInterface(io.apb,  (0x000,1 KiB), 0, regPre = "AP")
  io.sys_int  := busif.interruptFactory("SYS",io.int_pulse0, io.int_pulse1, io.int_pulse2)
  io.s2  := busif.interruptFactory("SYS",io.int_pulse0, io.int_pulse1, io.int_pulse2)
  io.gpio_int := busif.interruptLevelFactory("GPIO",io.s2, io.sys_int, io.int_level0, io.int_level1,io.sys_int)

  def genDoc() = {
    busif.accept(CHeaderGenerator("intrreg.html","Intr"))
    busif.accept(HtmlGenerator("intrreg.html", "Interupt Example"))
    busif.accept(JsonGenerator("intrreg.json"))
    this
  }
}

class RegFileIntrSim extends RegFileIntrExample{
  def Init() = {
    this.clockDomain.forkStimulus(2)
    this.reset()
    this.io.int_pulse0  #= false
    this.io.int_pulse1  #= false
    this.io.int_pulse2  #= false
    this.io.int_level0  #= false
    this.io.int_level1  #= false
  }

  def reset() = {
    this.clockDomain.assertReset()
    sleep(10)
    this.clockDomain.deassertReset()
  }

  def write(address: Long, data: Long) = {
    val apbDriver = Apb3Driver(this.io.apb, this.clockDomain)
    apbDriver.write(address, data)
  }

  def read(address: Long): BigInt = {
    val apbDriver = Apb3Driver(this.io.apb, this.clockDomain)
    apbDriver.read(address)
  }

  def trigger(x: Bool) = {
    x #= true
    this.clockDomain.waitSampling()
    x #= false
  }

  def testPulse() = {
    this.trigger(io.int_pulse1)
    this.clockDomain.waitSampling(40)
    this.write(0x000, 0xFFFF)
    this.clockDomain.waitSampling(20)
    this.trigger(io.int_pulse1)
    this.write(0x008, 0x0000)
    this.write(0x018, 0x0000) //OPEN MASK
    this.clockDomain.waitSampling(40)
    this.write(0x000, 0xFFFF) //W1C
    this.clockDomain.waitSampling(40)
    this.write(0x004, 0x0002)
    this.clockDomain.waitSampling(20)
    this.write(0x004, 0x0000)
    this.clockDomain.waitSampling(20)
    this.trigger(io.int_pulse0)
  }

  def testLevel()  = {
    this.io.int_level1  #= true
    this.write(0x020, 0x0000)  //OPEN MASK
    this.clockDomain.waitSampling(20)
    this.write(0x010, 0xFFFF) //W1C
    this.write(0x020, 0xFFFF) //CLOSE MASK
    this.clockDomain.waitSampling(20)
    this.write(0x020, 0x0000)  //OPEN MASK
    this.clockDomain.waitSampling(20)
    this.write(0x000, 0xFFFF) //W1C
    this.clockDomain.waitSampling(40)
    this.io.int_level1  #= false
    this.clockDomain.waitSampling(20)
  }
}

object regintSim extends App{
  SpinalSimConfig()
    .withFstWave
    .compile(new RegFileIntrSim()) .doSimUntilVoid("xx"){ dut =>
    dut.genDoc()
    dut.Init
    dut.clockDomain.waitSampling(10)
    dut.testPulse()
    dut.testLevel()
    dut.clockDomain.waitSampling(200)
    simSuccess()
  }
}
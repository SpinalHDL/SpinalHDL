package spinal.tester.generator.regif

import spinal.core.{out, _}
import spinal.core.sim._
import spinal.lib.bus.regif._
import spinal.lib.bus.regif.AccessType._
import spinal.lib._
import spinal.lib.bus.amba4.apb.sim.Apb4Driver
import spinal.lib.bus.amba4.apb.{Apb4, Apb4Config}

import scala.collection.immutable._
import scala.util.Random

class RegIfTester(seed: Int = 0) extends Component with RegIfRef {
  val bus = slave(Apb4(Apb4Config(32, 32)))
  val drv = Apb4Driver(bus, this.clockDomain)

  val io = new Bundle{
    val o   = out(Vec(Bits(32 bit), ACCESS_LIST.size))
    val o_z = out(Vec(Bits(32 bit), ACCESS_LIST.size))
    val i   = in(Vec(Bits(32 bit), ACCESS_LIST.size))
    val t   = in(Bool())
  }

  val regIf = BusInterface(bus, (0, maps.size*32), 0)

  val rand = new Random(seed = 0)

  ACCESS_LIST.zipWithIndex.foreach{ case(acc, i) =>
    val reg = regIf.newRegAt(i*4, s"Register_0x${(i*4).hexString(16)}_${acc}").field(Bits(32 bit), acc, resetValue = BigInt.probablePrime(32, rand), s"${acc}")
    reg.setName(s"fd_0x${(i*4).hexString(16)}_${acc}".toLowerCase())
    acc match{
      case `RO`  => reg := "abcdedf".asHex; io.o(i).clearAll(); io.o_z(i).clearAll()
      case `ROV` => io.o(i).clearAll();io.o_z(i).clearAll()
      case _ => {
        when(io.t) {
          reg := io.i(i)
        }
        io.o(i) := reg
        io.o_z(i) := RegNext(io.o(i))
      }
    }
  }

  def dowrite(addr: Long, verbose: Boolean = false): Unit = {
    val idx: Int = (addr%(NUM_REGISTERS*4) / 4).toInt
    val acc = maps.get(idx).get

    val wdata = BigInt.probablePrime(32, rand)
    val wstrb = BigInt.probablePrime(4, rand)

    val oldhwdata: BigInt = {sleep(0);this.io.o(idx).toBigInt}
    drv.write(addr, wdata, wstrb)
    val newhwdata: BigInt = {sleep(0);this.io.o(idx).toBigInt}

    val refdata   = onWrite(acc, oldhwdata, wdata, wstrb, !writeHistory.contains(addr))
    writeHistory.add(addr)
    val msg = s", wirte '0x${wdata.hexString(32)}' on '0x${oldhwdata.hexString(32)}' strb '0x${wstrb.binString(4)}'"
    putMsg(compare(acc, addr, newhwdata, refdata, msg))
    if (verbose) {
      SpinalInfo(f"0x${addr.hexString(16)}[${acc}%5s]${msg} => 0x${newhwdata.hexString(32)}")
    }
  }

  def doread(addr: Long, verbose: Boolean = false) = {
    val idx: Int = (addr / 4).toInt
    val acc = maps.get(idx).get

    val oldhwdata: BigInt = {sleep(0); io.o(idx).toBigInt}
    drv.read(addr)
    val newhwdata: BigInt = {sleep(0); io.o(idx).toBigInt}
    val refdata   = onRead(acc, oldhwdata)
    val msg = s", read on '0x${oldhwdata.hexString(32)}'"
    if(verbose){
      SpinalInfo(f"0x${addr.hexString(16)}[${acc}%5s]${msg}")
    }
    putMsg(compare(acc, addr, newhwdata, refdata, msg))
  }

  def randomtest(n: Int, verbose: Boolean = false) = {
    this.io.t #= true
    this.clockDomain.waitSampling()
    this.io.t #= false
    (0 until n).foreach{ i =>
      val addr  = scala.math.abs(rand.nextInt(NUM_REGISTERS))*4
      if(rand.nextBoolean()){
        dowrite(addr, verbose)
      } else {
        doread(addr, verbose)
      }
    }
  }

  def sim(n: Int, verbose: Boolean = false) = {
    this.clockDomain.forkStimulus(2)
    this.io.i.foreach { t => t #= BigInt.probablePrime(32, rand)}
    this.io.t #= false
    this.io.t #= true
    this.clockDomain.waitSampling()
    this.io.t #= false
    this.debug()
    this.clockDomain.waitSampling(10)
    this.typical()
    this.clockDomain.waitSampling(10)
    this.randomtest(n, verbose)
    this.clockDomain.waitSampling(10)
    if(isFail){
      simFailure("RegIf test Failed: \n" + FailMsg.mkString("\n"))
    } else {
      simSuccess()
    }
  }

  def typical() = {
    (0 until NUM_REGISTERS).foreach { i =>
      dowrite(i * 4, true)
    }
    (0 until NUM_REGISTERS).foreach { i =>
      doread(i * 4, true)
    }
  }
  def debug() = {
    dowrite(0x068, true)
    dowrite(0x068, true)
    dowrite(0x06c, true)
    dowrite(0x06c, true)
  }
}

object RegIfStrbTesterSim {
  def sim(n: Int = 1000, withwave: Boolean = false) ={
    if(withwave){
      SpinalSimConfig()
        .withFstWave
    } else SpinalSimConfig()
      .compile(new RegIfTester(seed = 0))
      .doSimUntilVoid("test") { dut =>
        dut.sim(10000, true)
      }
  }
}

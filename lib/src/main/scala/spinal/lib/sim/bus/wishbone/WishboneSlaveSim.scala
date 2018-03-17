package spinal.lib.wishbone.sim

import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.wishbone._
import spinal.core._
import spinal.core.sim._
import spinal.sim._
import scala.collection.mutable._
import scala.util.Random


trait WishboneSlaveCommon{
  val wishbone : Wishbone
  val clockdomain : ClockDomain
  val triggers = Map[AddressRange,(Wishbone) => Unit]()

  def addTrigger(address: AddressRange)(trigger: (Wishbone) => Unit): Unit = {
    triggers += (address-> trigger)
  }

  //def addTrigger(address: AddressRange)(trigger: (Wishbone) => Unit) : Unit = addTrigger(address,trigger)
}

case class WishboneSlave(wishbone: Wishbone, clockdomain: ClockDomain) extends WishboneSlaveCommon{
  fork{
    while(true){
      //clockdomain.waitActiveEdgeWhere(wishbone.isStrobe.toBoolean)
      clockdomain.waitSamplingWhere(wishbone.CYC.toBoolean && wishbone.STB.toBoolean) //TODO: support ERR
      //wishbone.ACK #= true
      triggers.suspendable.foreach{ trigger => if(trigger._1.inRange(wishbone.ADR.toBigInt)) trigger._2(wishbone)}
      wishbone.ACK #= true
      waitUntil(!(wishbone.STB.toBoolean && wishbone.CYC.toBoolean))
      wishbone.ACK #= false
    }
  }
}

/*
 In pipelined mode you need to send an acknoledge
 */
case class WishbonePipelinedSlave(wishbone: Wishbone, clockdomain: ClockDomain) extends WishboneSlaveCommon{
  fork{
    while(true){
      clockdomain.waitSamplingWhere(wishbone.CYC.toBoolean)
      while(wishbone.STB.toBoolean){
        triggers.suspendable.foreach{ trigger =>
          if(trigger._1.inRange(wishbone.ADR.toBigInt)){
            trigger._2(wishbone)
            wishbone.ACK #= true
          }
        }
        clockdomain.waitSampling()
      }
      wishbone.ACK #= false
    }
  }
}

case class AddressRange(base : BigInt, size: Int){
  def inRange(address: BigInt): Boolean = (address >= base) && (address <= base + size)
  def == (range: AddressRange): Boolean = (base == range.base) && (size == range.size)
  def mask(address: BigInt): BigInt = address - base
}


class WishboneSlaveTest extends Component {
  val io = new Bundle{
    val bus = master(Wishbone(WishboneConfig(8,8)))
    val start = in Bool
  }

  io.bus.CYC := False
  io.bus.STB := False
  io.bus.ADR := 0
  io.bus.WE := False
  io.bus.DAT_MOSI := 0

  val fsm = new StateMachine{
    val  idle : State = new State with EntryPoint{
      whenIsActive{
        io.bus.CYC := False
        io.bus.WE := False
        when(io.start){
          goto(send)
        }
      }
    }
    val send : State = new State{
      whenIsActive{
        io.bus.CYC := True
        io.bus.STB := True
        io.bus.DAT_MOSI := 42
        io.bus.ADR := 10
        when(io.bus.ACK){
          goto(finish)
        }
      }
    }
    val finish : State = new State{
      whenIsActive{
        io.bus.CYC := True
        io.bus.STB := False
        when(io.start){
          goto(send)
        }.otherwise{
          io.bus.CYC := False
          goto(idle)
        }
      }
    }
  }
}

object SlaveTest{
  def main(args: Array[String]): Unit = {
    SimConfig(rtl = new WishboneSlaveTest).withWave.doManagedSim{ dut =>
      dut.clockDomain.forkStimulus(period=10)
      dut.io.start #= false
      dut.io.bus.CYC #= false
      dut.io.bus.STB #= false
      dut.io.bus.ACK #= false
      sleep(1000)
      dut.io.start #= true
      val wbs = new WishboneSlave(dut.io.bus, dut.clockDomain)

      wbs.addTrigger(AddressRange(10,10)){bus =>
       //dut.clockDomain.waitRisingEdge(10)
        bus.DAT_MISO #= Random.nextInt(256)
      }

      dut.clockDomain.waitRisingEdge(99)
      dut.io.start #= false
      dut.clockDomain.waitRisingEdge(100)
    }
  }
}

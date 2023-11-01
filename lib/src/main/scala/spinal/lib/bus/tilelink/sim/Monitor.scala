package spinal.lib.bus.tilelink.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink._
import spinal.lib.sim.{StreamDriver, StreamDriverOoo, StreamMonitor, StreamReadyRandomizer}

import scala.collection.mutable.ArrayBuffer


trait MonitorSubscriber{
  def onA(a : TransactionA) : Unit = { }
  def onB(b : TransactionB) : Unit = { }
  def onC(c : TransactionC) : Unit = { }
  def onD(d : TransactionD) : Unit = { }
  def onE(e : TransactionE) : Unit = { }

  def onBeatC(c: TransactionC): Unit = { }
}

class Monitor (val bus : Bus, cd : ClockDomain) {
  var debug = false
  var counterA = 0
  var counterD = 0

  def add(s : MonitorSubscriber) : this.type = {
    subscribers += s
    this
  }

  val subscribers = ArrayBuffer[MonitorSubscriber]()
  def onA(f : TransactionA) : Unit = {
    subscribers.foreach(_.onA(f))
    counterA += 1
  }
  def onB(f : TransactionB) : Unit = subscribers.foreach(_.onB(f))
  def onC(f : TransactionC) : Unit = subscribers.foreach(_.onC(f))
  def onD(f : TransactionD) : Unit = {
    subscribers.foreach(_.onD(f))
    counterD += 1
  }
  def onE(f : TransactionE) : Unit = subscribers.foreach(_.onE(f))

  val faa = new TransactionAggregator[TransactionA](bus.p.dataBytes)(onA)
  val fab = bus.p.withBCE generate new TransactionAggregator[TransactionB](bus.p.dataBytes)(onB)
  val fac = bus.p.withBCE generate new TransactionAggregator[TransactionC](bus.p.dataBytes)(onC)
  val fad = new TransactionAggregator[TransactionD](bus.p.dataBytes)(onD)

  val aToD = Array.fill(1 << bus.p.sourceWidth)(BigInt(0))
  val cToD = Array.fill(1 << bus.p.sourceWidth)(BigInt(0))
  val a = StreamMonitor(bus.a, cd){p =>
    val f = TransactionA(p)
    if(faa.beat == 0) aToD(f.source) = f.address
    faa.push(f)
  }
  val b = bus.p.withBCE generate StreamMonitor(bus.b, cd)(p => fab.push(TransactionB(p)))
  val c = bus.p.withBCE generate StreamMonitor(bus.c, cd){p =>
    val f = TransactionC(p)
    if(fac.beat == 0) p.opcode.toEnum match {
      case Opcode.C.RELEASE | Opcode.C.RELEASE_DATA => cToD(f.source) = f.address
      case Opcode.C.PROBE_ACK | Opcode.C.PROBE_ACK_DATA =>
    }

    subscribers.foreach(_.onBeatC(f))

    fac.push(f)
  }
  val e = bus.p.withBCE generate StreamMonitor(bus.e, cd)(p => onE(TransactionE(p)))
  val d = StreamMonitor(bus.d, cd) {p =>
    val address = p.opcode.toEnum match {
      case Opcode.D.ACCESS_ACK | Opcode.D.ACCESS_ACK_DATA | Opcode.D.GRANT | Opcode.D.GRANT_DATA=> {
        val v = aToD(p.source.toInt)
        aToD(p.source.toInt) += bus.p.dataBytes
        v
      }
      case Opcode.D.RELEASE_ACK =>{
        val v = cToD(p.source.toInt)
        cToD(p.source.toInt) += bus.p.dataBytes
        v
      }
    }
    fad.push(TransactionD(p, address))
  }
}

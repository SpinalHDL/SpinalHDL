package spinal.debugger

import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer


object LogicAnalyser {

}

class LogicAnalyserParameter {
  var memAddressWidth = 8

  val dataList = ArrayBuffer[Data]()
  def probe(that: Data) {
    dataList += that
  }
}

class LogicAnalyser(p: LogicAnalyserParameter) extends Component {
  val io = new Bundle {
    val packetSlave = slave Flow Fragment(Bits(8 bit))
    val packetMaster = master Handshake Fragment(Bits(8 bit))
  }
  //io.packetSlave.isFirst

  val trigger = new Area {
    val event = CounterFreeRun(16) === UInt(0 lit)
  }

  val probe = Cat(p.dataList.map(_.pull))

  val packetSlaves = FlowFragmentRouter(io.packetSlave,1)

  val logger = new LogicAnalyserLogger(p,probe)
  logger.io.packetSlave << packetSlaves(0)
  logger.io.trigger := trigger.event
  logger.io.probe := probe
  //<< logger.io.log
}


object LogicAnalyserLoggerState extends SpinalEnum {
  val sWaitTrigger, sSample, sPush, Nil = Value
}

class LogicAnalyserLogger(p: LogicAnalyserParameter, probeType: Bits) extends Component {

  import LogicAnalyserLoggerState._

  val io = new Bundle {
    val packetSlave = slave Flow Fragment(Bits(8 bit))

    val trigger = in Bool()
    val probe = in cloneOf(probeType)

    val log = master Handshake Fragment(probe)
  }

  val mem = Mem(probeType, 1 << p.memAddressWidth)
  val memWriteAddress = Reg(mem.addressType)
  val memReadAddress = Reg(mem.addressType)


  val config = new Bundle {
    val samplesLeftAfterTrigger = mem.addressType
  }


  val state = RegInit(sWaitTrigger)
  val pushCounter = Reg(mem.addressType)


  val sampler = new Area {
    val preEnable = Bool(false)
    val postEnable = Bool(false)
    val counter = Reg(mem.addressType)

    when(postEnable){
      counter := counter - UInt(1 lit)
    } otherwise{
      counter := config.samplesLeftAfterTrigger
    }

    when(preEnable || postEnable) {
      mem(memWriteAddress) := io.probe
      memWriteAddress := memWriteAddress + UInt(1 lit)
    }

    val done = counter === UInt(0 lit)
  }


  val memReadCmd = Handshake(mem.addressType)
  val memReadCmdIsLast = Bool(false)

  memReadCmd.valid := Bool(false)
  memReadCmd.data := memReadAddress


  when(state === sWaitTrigger) {
    sampler.preEnable := Bool(true)
    when(io.trigger) {
      state := sSample
      memReadAddress := memWriteAddress + config.samplesLeftAfterTrigger + UInt(2 lit)
    }
  }
  when(state === sSample) {
    sampler.postEnable := Bool(true)
    when(sampler.done) {
      state := sPush
      pushCounter := UInt(0 lit)
    }
  }
  when(state === sPush) {
    memReadCmd.valid := Bool(true)
    when(memReadCmd.ready) {
      memReadAddress := memReadAddress + UInt(1 lit)
      pushCounter := pushCounter + UInt(1 lit)
    }
    when(pushCounter === UInt((1 << pushCounter.getWidth) - 1 lit)) {
      memReadCmdIsLast := Bool(true)
      when(memReadCmd.ready) {
        state := sWaitTrigger
      }
    }
  }


  val memReadPort = mem.handshakeReadSync(memReadCmd, memReadCmdIsLast)
  io.log.connectFrom(memReadPort)((to, from) => {
    to.last := from.linked
    to.fragment := from.value
  })

}
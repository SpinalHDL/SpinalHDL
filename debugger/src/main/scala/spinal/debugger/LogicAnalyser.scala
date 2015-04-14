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
  val fragmentWidth = 8
  val io = new Bundle {
    val slavePort = slave Flow Fragment(Bits(fragmentWidth bit))
    val masterPort = master Handshake Fragment(Bits(fragmentWidth bit))
  }
  //io.packetSlave.isFirst

  val trigger = new Area {
    val event = CounterFreeRun(1000) === U(999)
  }

  val probe = Cat(p.dataList.map(_.pull))

  val packetSlaves = FlowFragmentRouter(io.slavePort,1)

  val logger = new LogicAnalyserLogger(p,probe)
  logger.io.packetSlave << packetSlaves(0)
  logger.io.trigger := trigger.event
  logger.io.probe := probe

  //TODO better toFragmentBits with internal cat header to logger stream and remove insertHeader (less ressource usage)
  io.masterPort << logger.io.log.toFragmentBits(fragmentWidth).insertHeader(0xAA)
}


object LogicAnalyserLoggerState extends SpinalEnum {
  val sWaitTrigger, sSample, sPush, Nil = Value
}

class LogicAnalyserLogger(p: LogicAnalyserParameter, probeType: Bits) extends Component {

  import LogicAnalyserLoggerState._

  val io = new Bundle {
    val packetSlave = slave Flow Fragment(Bits(8 bit))

    val trigger = in Bool
    val probe = in cloneOf(probeType)

    val log = master Handshake Fragment(probe)
  }

  val mem = Mem(probeType, 1 << p.memAddressWidth)
  val memWriteAddress = Reg(mem.addressType) randBoot
  val memReadAddress = Reg(mem.addressType)


  val config = new Bundle {
    val samplesLeftAfterTrigger = mem.addressType
  }
  config.samplesLeftAfterTrigger := 10

  val state = RegInit(sWaitTrigger)
  val pushCounter = Reg(mem.addressType)


  val sampler = new Area {
    val preEnable = False
    val postEnable = False
    val counter = Reg(mem.addressType)

    when(postEnable){
      counter := counter - U(1)
    } otherwise{
      counter := config.samplesLeftAfterTrigger
    }

    when(preEnable || postEnable) {
      mem(memWriteAddress) := io.probe
      memWriteAddress := memWriteAddress + U(1)
    }

    val done = counter === U(0)
  }


  val memReadCmd = Handshake(mem.addressType)
  val memReadCmdIsLast = False

  memReadCmd.valid := False
  memReadCmd.data := memReadAddress


  when(state === sWaitTrigger) {
    sampler.preEnable := True
    when(io.trigger) {
      state := sSample
      memReadAddress := memWriteAddress + config.samplesLeftAfterTrigger + U(2)
    }
  }
  when(state === sSample) {
    sampler.postEnable := True
    when(sampler.done) {
      state := sPush
      pushCounter := U(0)
    }
  }
  when(state === sPush) {
    memReadCmd.valid := True
    when(memReadCmd.ready) {
      memReadAddress := memReadAddress + U(1)
      pushCounter := pushCounter + U(1)
    }
    when(pushCounter === U((1 << pushCounter.getWidth) - 1)) {
      memReadCmdIsLast := True
      when(memReadCmd.ready) {
        state := sWaitTrigger
      }
    }
  }


  val memReadPort = mem.handshakeReadSync(memReadCmd, memReadCmdIsLast)
  io.log.connectFrom2(memReadPort)((to, from) => {
    to.last := from.linked
    to.fragment := from.value
  })

}
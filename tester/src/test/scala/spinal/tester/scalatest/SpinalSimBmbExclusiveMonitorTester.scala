package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core.sim._
import spinal.lib.Fragment
import spinal.lib.bus.amba3.apb.Apb3Config
import spinal.lib.bus.amba3.apb.sim.Apb3Monitor
import spinal.lib.bus.bmb.{Bmb, BmbAccessParameter, BmbCmd, BmbDownSizerBridge, BmbExclusiveMonitor, BmbInvalidationParameter, BmbParameter, BmbRsp, BmbSourceParameter, BmbToApb3Bridge}
import spinal.lib.bus.bmb.sim.{BmbBridgeTester, BmbMasterAgent, BmbMemoryAgent, BmbRegionAllocator}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.sim._

import scala.collection.mutable
import scala.util.Random



private object IDLE
private object READ_CMD
private object READ_RSP
private object MODIFY
private object WRITE_CMD
private object WRITE_RSP

class SpinalSimBmbExclusiveMonitorTester extends FunSuite{

  SimConfig.compile(
    new BmbExclusiveMonitor(
      inputParameter = BmbParameter(
        BmbAccessParameter(
          addressWidth                    = 32,
          dataWidth                       = 32
        ).addSources(4, BmbSourceParameter(
          lengthWidth                     = 8,
          contextWidth                    = 8,
          alignment                       = BmbParameter.BurstAlignement.LENGTH,
          canRead                         = true,
          canWrite                        = true,
          canExclusive                    = true
        )),
        BmbInvalidationParameter(
          canInvalidate                   = true,
          canSync                         = true,
          invalidateLength                = 8,
          invalidateAlignment             = BmbParameter.BurstAlignement.LENGTH
        )
      ),
      pendingWriteMax = 8
    )
  ).doSim(seed = 42) { dut =>
    dut.clockDomain.forkStimulus(10)

    var endSim = false

    val masterAgent = for (i <- dut.inputParameter.access.sources.keys) yield new {
      val sourceId = i
      var state: Any = IDLE
      var read, write = BigInt(0)
      var successCounter = 0
      var conflictCounter = 0
      var data = 0l

      dut.clockDomain.onSamplings {
        def rspEvent = dut.io.input.rsp.valid.toBoolean && dut.io.input.rsp.ready.toBoolean && dut.io.input.rsp.source.toInt === sourceId
        state match {
          case IDLE => {
            if (!endSim && Random.nextFloat() < 0.1) {
              state = READ_CMD
            }
          }
          case READ_RSP => {
            if(rspEvent){
              data = dut.io.input.rsp.data.toLong
              state = MODIFY
            }
          }
          case MODIFY => {
            if (Random.nextFloat() < 0.1) {
              data += sourceId + 1
              state = WRITE_CMD
            }
          }
          case WRITE_RSP => {
            if(rspEvent){
              if(dut.io.input.rsp.exclusive.toBoolean) {
                successCounter = successCounter + 1
              } else {
                conflictCounter = conflictCounter + 1
              }
              state = IDLE
            }
          }
          case _ =>
        }
      }
    }

    def genInputCmd(payload: Fragment[BmbCmd]): Boolean = {
      val requests = masterAgent.filter(a => a.state == READ_CMD || a.state == WRITE_CMD)
      if(requests.isEmpty) return false
      val request = requests.toSeq.randomPick()

      request.state match {
        case READ_CMD =>{
          payload.fragment.opcode #=  Bmb.Cmd.Opcode.READ
          request.state = READ_RSP
        }
        case WRITE_CMD => {
          payload.fragment.opcode #= Bmb.Cmd.Opcode.WRITE
          payload.fragment.data #= request.data
          payload.fragment.mask #= 15
          request.state = WRITE_RSP
        }
      }

      payload.last #= true
      payload.fragment.source #= request.sourceId
      payload.fragment.exclusive #= true
      payload.fragment.address #= 0x80
      payload.fragment.length #= 3
//      payload.fragment.context
      true
    }

    val outputRspQueue = mutable.ArrayBuffer[() => Unit]()
    var memory = 0l
    val outputCmdMonitor = StreamMonitor(dut.io.output.cmd, dut.clockDomain){ payload =>
      val transaction = SimData.copy(payload.fragment)

      val write = payload.fragment.opcode.toInt == Bmb.Cmd.Opcode.WRITE
//      println(memory)
      outputRspQueue += (() => {
        if(write && transaction.mask.asInstanceOf[BigInt] == 0xF) {
          val v  = transaction.data.asInstanceOf[BigInt].toLong
          if(v <= memory) println(s"??? $v <= $memory")
          memory = v
        }
        dut.io.output.rsp.opcode #= Bmb.Rsp.Opcode.SUCCESS
        dut.io.output.rsp.context #= transaction.context.asInstanceOf[BigInt]
        dut.io.output.rsp.source #= transaction.source.asInstanceOf[BigInt]
        if(!write) dut.io.output.rsp.data #= memory
        dut.io.output.rsp.last #= true
      })
    }


    def genOutputRsp(payload: Fragment[BmbRsp]): Boolean = {
      if(outputRspQueue.isEmpty) return false
      outputRspQueue.randomPop().apply()
      true
    }

    val inputCmdAgent = StreamDriver(dut.io.input.cmd, dut.clockDomain)(genInputCmd)
    val inputRspAgent = StreamReadyRandomizer(dut.io.input.rsp, dut.clockDomain)

    val outputCmdAgent = StreamReadyRandomizer(dut.io.output.cmd, dut.clockDomain)
    val outputRspAgent = StreamDriver(dut.io.output.rsp, dut.clockDomain)(genOutputRsp)


    dut.clockDomain.waitSampling(1000000)
    endSim = true
    dut.clockDomain.waitSampling(1000)

    for(a <- masterAgent){
      println(s"${a.state} ${a.successCounter}")
    }

    val memoryExpected = masterAgent.map(a => a.successCounter*(a.sourceId + 1)).sum
    println(s"memory:$memory expected:${memoryExpected}")

    assert(memory == memoryExpected && memory > 10000 && !masterAgent.exists(a => a.successCounter < 5000 || a.conflictCounter < 100))
  }
}

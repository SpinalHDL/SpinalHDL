package spinal.lib.bus.tilelink

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.sim._

class MemoryAgentTester extends AnyFunSuite {
  lazy val compiled = {
    DebugId.setup(16)
    SimConfig.compile(new Component {
      val p = NodeParameters(
        m = M2sParameters(
          addressWidth = 12,
          dataWidth = 64,
          masters = List(M2sAgent(
            name = null,
            mapping = List(M2sSource(
              id = SizeMapping(0, 2),
              emits = M2sTransfers(
                acquireT = SizeRange(64),
                acquireB = SizeRange(64),
                putFull = SizeRange(8),
                arithmetic = SizeRange(8),
                logical = SizeRange(8)
              )
            ))
          ))
        ),
        s = S2mParameters(List(S2mAgent(
          name = null,
          sinkId = SizeMapping(0, 2),
          emits = S2mTransfers(probe = SizeRange(64))
        )))
      )
      val io = new Bundle {
        val up = slave(Bus(p))
        val down = master(Bus(p))
      }
      io.down << io.up
    })
  }

  for(withData <- List(false, true)) {
    test(if(withData) "ReleaseData param" else "Release param") {
      compiled.doSim(seed = 42) { dut =>
        SimTimeout(10000)
        dut.clockDomain.forkStimulus(10)
        implicit val idAllocator = new IdAllocator(DebugId.width)
        implicit val idCallback: IdCallback = null
        val master = new MasterAgent(dut.io.up, dut.clockDomain)
        val memory = new MemoryAgent(dut.io.down, dut.clockDomain, seed = 42)
        Checker(master.monitor)

        var releases = 0
        var acks = 0
        master.monitor.add(new MonitorSubscriber {
          override def onC(c: TransactionC): Unit = {
            if(c.opcode == Opcode.C.RELEASE || c.opcode == Opcode.C.RELEASE_DATA) {
              assert(c.opcode == (if(withData) Opcode.C.RELEASE_DATA else Opcode.C.RELEASE))
              assert(c.param == Param.Prune.TtoN)
              assert(c.param != 0)
              releases += 1
            }
          }
          override def onD(d: TransactionD): Unit = {
            if(d.opcode == Opcode.D.RELEASE_ACK) {
              assert(d.param == 0)
              acks += 1
            }
          }
        })

        val block = master.acquireBlock(0, Param.Grow.NtoT, 0x100, 64)
        assert(block.cap == Param.Cap.toT)
        if(withData) {
          block.makeDataDirty()
          master.releaseData(0, Param.Cap.toN, block)
        } else {
          master.release(0, Param.Cap.toN, block)
        }
        dut.clockDomain.waitSampling(10)
        assert(releases == 1)
        assert(acks == 1)
      }
    }
  }

  for(arithmetic <- List(false, true)) {
    test(if(arithmetic) "ArithmeticData response param" else "LogicalData response param") {
      compiled.doSim(seed = 42) { dut =>
        SimTimeout(10000)
        dut.clockDomain.forkStimulus(10)
        implicit val idAllocator = new IdAllocator(DebugId.width)
        implicit val idCallback: IdCallback = null
        val master = new MasterAgent(dut.io.up, dut.clockDomain)
        val memory = new MemoryAgent(dut.io.down, dut.clockDomain, seed = 42)
        Checker(master.monitor)

        val data = Seq.fill[Byte](8)(1)
        val d = if(arithmetic) {
          master.arithmeticData(0, 0x100, Param.Arithmetic.ADD, data)
        } else {
          master.logicalData(0, 0x100, Param.Logical.SWAP, data)
        }
        assert(d.opcode == Opcode.D.ACCESS_ACK_DATA)
        assert(d.param == 0)
      }
    }
  }
}

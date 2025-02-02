package spinal.tester.scalatest

import spinal.core._
import spinal.lib._
import spinal.lib.formal._
import spinal.core.formal._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}
import spinal.lib.bus.simple._

class PipelinedMemoryBusConnect() extends Component {
  val busIn = slave(PipelinedMemoryBus(PipelinedMemoryBusConfig(32, 32)))
  val busOut = master(PipelinedMemoryBus(PipelinedMemoryBusConfig(32, 32)))

  busIn.assertBusEquivalence(busOut)

  busIn <> busOut
}

class PipelinedMemoryBusConnectFormal(withAssumptions : Boolean) extends Component {
  val dut = FormalDut(new PipelinedMemoryBusConnect())
  assumeInitial(ClockDomain.current.isResetActive)

  anyseq(dut.busIn.cmd.payload)
  anyseq(dut.busIn.cmd.valid)
  anyseq(dut.busOut.rsp)
  anyseq(dut.busOut.cmd.ready)

  withAutoPull()

  if(withAssumptions) {
    dut.busIn.formalAssumesSlave()
    dut.busOut.formalAssumesMaster()
  }
  cover(dut.busIn.formalContract.outstandingReads.value > 3)

  setDefinitionName(s"PipelinedMemoryBusConnectFormal_assumes${withAssumptions}")
}


class PipelinedMemoryBusArbiterFormal(portCount : Int, pendingRspMax : Int, rspRouteQueue : Boolean, transactionLock : Boolean) extends Component {
  val dut = FormalDut(new PipelinedMemoryBusArbiter(PipelinedMemoryBusConfig(32,32), portCount, pendingRspMax, rspRouteQueue, transactionLock))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.io.inputs.foreach(bus => {
    anyseq(bus.cmd.payload)
    anyseq(bus.cmd.valid)
  })

  anyseq(dut.io.output.rsp)
  anyseq(dut.io.output.cmd.ready)
}

class PipelinedMemoryBusDecoderFormal(mappings: Seq[AddressMapping], pendingRspMax : Int) extends Component {
  val dut = FormalDut(new PipelinedMemoryBusDecoder(PipelinedMemoryBusConfig(32, 32), mappings, pendingRspMax))
  assumeInitial(ClockDomain.current.isResetActive)

  anyseq(dut.io.input.cmd.payload)
  anyseq(dut.io.input.cmd.valid)

  if(!dut.hasDefault && mappings.size > 1) {
    cover(dut.logic.noHit)
    dut.io.outputs.foreach(output => cover(output.rsp.valid))
    cover(dut.logic.rspPendingCounter.mayOverflow)
  }

  for(bus <- dut.io.outputs) {
    anyseq(bus.rsp)
    anyseq(bus.cmd.ready)
  }
}

class PipelinedMemoryBusInterconnectComponent(mappings: Seq[AddressMapping], pendingRspMax : Int, portCount : Int, rspRouteQueue : Boolean) extends ComponentWithFormalAsserts {
  val config = PipelinedMemoryBusConfig(32, 32)

  val io = new Bundle {
    val masters = Array.fill(portCount)(slave(PipelinedMemoryBus(config)))
    val slaves = mappings.map(_ => master(PipelinedMemoryBus(config)))
  }

  val interconnect = PipelinedMemoryBusInterconnect()
  interconnect.arbitrationPendingRspMaxDefault = pendingRspMax
  interconnect.arbitrationRspRouteQueueDefault = rspRouteQueue
  io.slaves.zip(mappings).foreach { case (slave, mapping) => {
    interconnect.addSlave(slave, mapping)
  }}

  io.masters.foreach(master => {
    interconnect.addMaster(master, io.slaves)
  })
}

class PipelinedMemoryBusInterconnectFormal(mappings: Seq[AddressMapping], pendingRspMax : Int, portCount : Int, rspRouteQueue : Boolean) extends Component {
  val dut = FormalDut(new PipelinedMemoryBusInterconnectComponent(mappings, pendingRspMax, portCount, rspRouteQueue))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.io.slaves.foreach(output => {
    anyseq(output.rsp)
    anyseq(output.cmd.ready)
    output.formalAssumesMaster()
  })

  dut.io.masters.foreach(input => {
    anyseq(input.cmd.payload)
    anyseq(input.cmd.valid)
    input.cmd.formalAssumesSlave()
  })
}

object FormalPipelinedMemoryBus {
  def runFormalTest(dut : => Component): Unit = {
    val config = FormalConfig._spinalConfig.copy(defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = HIGH,
      resetKind = SYNC),
      mergeAsyncProcess = true
    )

    FormalConfig.withDebug.withConfig(config).withProve(15).withCover(15).
      doVerify(dut)(className)
  }
}

class FormalPipelinedMemoryBusConnection extends SpinalFormalFunSuite {
  test("Connect test pass") {
    FormalPipelinedMemoryBus.runFormalTest(new PipelinedMemoryBusConnectFormal(withAssumptions = true))
  }
  test("Connect test fail") {
    shouldFail(FormalPipelinedMemoryBus.runFormalTest(new PipelinedMemoryBusConnectFormal(withAssumptions = false)))
  }
}

class FormalPipelinedMemoryBusArbiter extends SpinalFormalFunSuite {
  for(portCount <- 1 until 5;
      pendingRsp <- 0 until 5) {
    test(s"Test Arbiter p${portCount} q${pendingRsp}") {
      FormalPipelinedMemoryBus.runFormalTest(new PipelinedMemoryBusArbiterFormal(portCount, pendingRsp.max(1), pendingRsp > 0, true))
    }
  }
}

class FormalPipelinedMemoryBusDecoder extends SpinalFormalFunSuite {
  for((mappingName, mapping) <- Map(
    "Single" -> Seq(SizeMapping(0, 0xabc)),
    "Multi" -> Seq(SizeMapping(0, 0xabc), SizeMapping(0x1000, 0x1123), SizeMapping(0x30000, 1)),
    "Default" -> Seq(DefaultMapping),
    "MultiDefault" -> Seq(SizeMapping(0, 1), DefaultMapping));
      pendingRsp <- 1 until 5
      ) {
    test(s"Test decoder ${mappingName} p${pendingRsp}") {
      FormalPipelinedMemoryBus.runFormalTest(new PipelinedMemoryBusDecoderFormal(mapping, pendingRsp).setDefinitionName(s"PipelinedMemoryBusDecoderFormal_${mappingName}_${pendingRsp}"))
    }
  }
}

class FormalPipelinedMemoryBusInterconnect extends SpinalFormalFunSuite {
  test("Test interconnect arbiter") {
    FormalPipelinedMemoryBus.runFormalTest(new PipelinedMemoryBusInterconnectFormal(Seq(DefaultMapping), 2, 2, true))
  }
//  test("Test interconnect pass through") {
//    FormalPipelinedMemoryBus.runFormalTest(new PipelinedMemoryBusInterconnectFormal(Seq(DefaultMapping), 5, 1, true))
//  }

  for((mappingName, mapping) <- Map(
    "Single" -> Seq(SizeMapping(0, 0xabc)),
    "Multi" -> Seq(SizeMapping(0, 0xabc), SizeMapping(0x1000, 0x1123), SizeMapping(0x30000, 1)),
    "Default" -> Seq(DefaultMapping),
    "Multi Default" -> Seq(SizeMapping(0, 1), DefaultMapping));
      portCount <- 1 until 3;
      pendingRsp <- 1 until 3
      ) {
    test(s"Test interconnect ${mappingName} p${portCount} r${pendingRsp}") {
      FormalPipelinedMemoryBus.runFormalTest(new PipelinedMemoryBusInterconnectFormal(mapping, portCount, pendingRsp, true))
    }
  }

}

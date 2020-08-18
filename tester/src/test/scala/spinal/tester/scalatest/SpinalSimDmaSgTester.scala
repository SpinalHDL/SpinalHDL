package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core.HardType
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbSlaveFactory}
import spinal.lib.bus.bmb.sim.{BmbDriver, BmbMemoryAgent}
import spinal.lib.system.dma.sg.{DmaSg, DmaSgTester, SgDmaTestsParameter}
import spinal.core.sim._

class SpinalSimDmaSgTester extends FunSuite{

  for((name, p) <- SgDmaTestsParameter(allowSmallerStreams = false)) test(name){

    val pCtrl = BmbParameter(
      addressWidth = 12,
      dataWidth    = 32,
      sourceWidth  = 0,
      contextWidth = 4,
      lengthWidth  = 2
    )

    SimConfig.allOptimisation.compile(new DmaSg.Core[Bmb](p, ctrlType = HardType(Bmb(pCtrl)), BmbSlaveFactory(_))).doSim(seed=42){ dut =>
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.forkSimSpeedPrinter(1.0)

      var writeNotificationHandle : (Long, Byte) => Unit = null

      val memory = new BmbMemoryAgent{
        override def writeNotification(address: Long, value: Byte): Unit = {
          writeNotificationHandle(address,value)
          super.setByte(address, value)
        }
      }
      if(p.canSgRead) memory.addPort(dut.io.sgRead, 0, dut.clockDomain, true)
      if(p.canSgWrite) memory.addPort(dut.io.sgWrite, 0, dut.clockDomain, true)
      if(p.canRead) memory.addPort(dut.io.read, 0, dut.clockDomain, true)
      if(p.canWrite) memory.addPort(dut.io.write, 0, dut.clockDomain, true)

      val ctrl = BmbDriver(dut.io.ctrl, dut.clockDomain)

      val tester = new DmaSgTester(
        p            = p,
        clockDomain  = dut.clockDomain,
        inputsIo     = dut.io.inputs,
        outputsIo    = dut.io.outputs,
        interruptsIo = dut.io.interrupts,
        memory       = memory.memory,
        dut
      ) {
        override def ctrlWriteHal(data: BigInt, address: BigInt): Unit = ctrl.write(data, address)
        override def ctrlReadHal(address: BigInt): BigInt = ctrl.read(address)
        writeNotificationHandle = writeNotification
      }

      tester.waitCompletion()
    }
  }
}

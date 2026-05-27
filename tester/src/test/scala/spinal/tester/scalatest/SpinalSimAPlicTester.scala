package spinal.tester.scalatest

import spinal.core._
import spinal.core.fiber.Fiber
import spinal.core.sim._
import spinal.sim._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.misc.aia._
import spinal.lib.misc._
import spinal.lib.sim._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class TilelinkAPlicReadPortFiber() extends Area {
  val node = tilelink.fabric.Node.down()

  val m2sParams = tilelink.M2sParameters(
    addressWidth = 32,
    dataWidth = 64,
    masters = List(
      tilelink.M2sAgent(
        name = TilelinkAPlicReadPortFiber.this,
        mapping = List(
          tilelink.M2sSource(
            id = SizeMapping(0, 4),
            emits = tilelink.M2sTransfers(
              get = tilelink.SizeRange(1, 64),
              putFull = tilelink.SizeRange(1, 64)
            )
          )
        )
      )
    )
  )

  var bus: Option[tilelink.Bus] = None

  val fiber = Fiber build new Area {
    node.m2s forceParameters m2sParams

    node.s2m.supported load tilelink.S2mSupport.none()

    val mappings = spinal.lib.system.tag.MemoryConnection.getMemoryTransfers(node)
    for(mapping <- mappings){
      println(s"- ${mapping.where} -> ${mapping.transfers}")
    }

    bus.map(node.bus <> _)
  }
}

case class APlicMSITestFiber(hartIds: Seq[Int], sourceIds: Seq[Int], guestIds: Seq[Int]) extends Component {
  val masterBus = TilelinkAPlicReadPortFiber()

  val crossBar = tilelink.fabric.Node()
  crossBar << masterBus.node

  val files = hartIds.map(hartId => guestIds.map { guestId =>
    val file = new ImsicFile(hartId, guestId, sourceIds.size + 1)

    file.interrupts.foreach(i => {
      i.ie.simPublic()
      i.ip.simPublic()
    })
    file.identity.simPublic()
    file.result.simPublic()
    file.trigger.simPublic()
    file.threshold.simPublic().allowUnsetRegToAvoidLatch()

    file
  })

  val modes = ArrayBuffer[SpinalEnumElement[APlicSourceMode.type]]()

  val peripherals = new Area {
    val access = tilelink.fabric.Node()
    access << crossBar

    val slave0 = TilelinkAPlicFiber(APlicDomainParam.S(APlicGenParam.test))
    slave0.node at 0x10000000 of access

    val slave0Sender = TilelinkAPlicMsiSenderFiber()
    crossBar << slave0Sender.node
    slave0Sender.createMsiStreamConsumer() << slave0.createMsiStreamProducer()

    val slave1 = TilelinkAPlicFiber(APlicDomainParam.S(APlicGenParam.test))
    slave1.node at 0x20000000 of access

    val slave1Sender = TilelinkAPlicMsiSenderFiber()
    crossBar << slave1Sender.node
    slave1Sender.createMsiStreamConsumer() << slave1.createMsiStreamProducer()

    val master = TilelinkAPlicFiber(APlicDomainParam.M(APlicGenParam.test))
    master.node at 0x30000000 of access

    val mSender = TilelinkAPlicMsiSenderFiber()
    crossBar << mSender.node
    mSender.createMsiStreamConsumer() << master.createMsiStreamProducer()

    val root = TilelinkAPlicFiber(APlicDomainParam.root(APlicGenParam.direct.withMsiAddrcfg()))
    root.node at 0x40000000 of access

    val imsic = TilelinkImsicTriggerFiber()
    imsic.node at 0x50000000 of access

    for (hartFile <- files) {
      for (file <- hartFile) {
        val trigger = imsic.addImsicFileinfo(file.asImsicFileInfo())
        file.trigger << trigger
      }
    }

    val slave0Target = hartIds.map(hartId => {
      val node = InterruptNode.slave()
      slave0.mapDownInterrupt(hartId, node)
      node
    })

    val slave1Target = hartIds.map(hartId => {
      val node = InterruptNode.slave()
      slave1.mapDownInterrupt(hartId, node)
      node
    })

    val mTarget = hartIds.map(hartId => {
      val node = InterruptNode.slave()
      master.mapDownInterrupt(hartId, node)
      node
    })

    val rootTargetNode = InterruptNode.slave()
    root.mapDownInterrupt(0, rootTargetNode)

    root.addChildCtrl(master)
    master.addChildCtrl(slave0)
    master.addChildCtrl(slave1)

	  // Init trigger method level high only.
    val rootSource = sourceIds.map(sourceId => {
      val node = InterruptNode.master()
      root.mapUpInterrupt(sourceId, node)
      node
    })

    master.mmsiaddrcfg := root.mmsiaddrcfg
    master.smsiaddrcfg := root.smsiaddrcfg
    slave0.mmsiaddrcfg := root.mmsiaddrcfg
    slave0.smsiaddrcfg := root.smsiaddrcfg
    slave1.mmsiaddrcfg := root.mmsiaddrcfg
    slave1.smsiaddrcfg := root.smsiaddrcfg

  }

  val io = new Bundle {
    val bus = slave(tilelink.Bus(masterBus.m2sParams.toNodeParameters().toBusParameter()))
    val sources = in Bits(sourceIds.size bits)
    val ie = in Vec(files.map(hartFiles => Vec(hartFiles.map(file => Bits(file.interrupts.size bits)))))
    val ip = out Vec(files.map(hartFiles => Vec(hartFiles.map(file => Bits(file.interrupts.size bits)))))
    val identity = out Vec(files.map(hartFiles => Vec(hartFiles.map(file => UInt(file.idWidth bits)))))
    val rootTarget = out Bits(1 bits)
    val masterTarget = out Bits(hartIds.size bits)
    val slave0Target = out Bits(hartIds.size bits)
    val slave1Target = out Bits(hartIds.size bits)
  }

  masterBus.bus = Some(io.bus)

  for ((bundle, source) <- peripherals.rootSource.zip(io.sources.asBools)) {
    bundle.flag := source
  }

  io.rootTarget := peripherals.rootTargetNode.flag.asBits
  io.masterTarget := peripherals.mTarget.map(_.flag).asBits()
  io.slave0Target := peripherals.slave0Target.map(_.flag).asBits()
  io.slave1Target := peripherals.slave1Target.map(_.flag).asBits()

  io.ip := Vec(files.map(hartFiles => Vec(hartFiles.map(file => file.interrupts.map(_.ip).asBits()))))
  io.identity := Vec(files.map(hartFiles => Vec(hartFiles.map(file => file.identity))))

  files.zip(io.ie).foreach{case (hartFile, hartIe) => {
    hartFile.zip(hartIe).foreach{case (file, ies) => {
      file.interrupts.zipWithIndex.foreach{case (i, idx) => i.ie := ies(idx)}
    }}
  }}
}

case class APlicDirectTestFiber(hartIds: Seq[Int], sourceIds: Seq[Int], mode: InterruptMode = LEVEL_HIGH) extends Component {
  val masterBus = TilelinkAPlicReadPortFiber()

  val crossBar = tilelink.fabric.Node()
  crossBar << masterBus.node

  val modes = ArrayBuffer[SpinalEnumElement[APlicSourceMode.type]]()

  val peripherals = new Area {
    val access = tilelink.fabric.Node()
    access << crossBar

    val slave0 = TilelinkAPlicFiber(APlicDomainParam.S(APlicGenParam.direct))
    slave0.node at 0x10000000 of access

    val slave1 = TilelinkAPlicFiber(APlicDomainParam.S(APlicGenParam.direct))
    slave1.node at 0x20000000 of access

    val master = TilelinkAPlicFiber(APlicDomainParam.M(APlicGenParam.direct))
    master.node at 0x30000000 of access

    val root = TilelinkAPlicFiber(APlicDomainParam.root(APlicGenParam.direct))
    root.node at 0x40000000 of access

    val slave0Target = hartIds.map(hartId => {
      val node = InterruptNode.slave()
      slave0.mapDownInterrupt(hartId, node)
      node
    })

    val slave1Target = hartIds.map(hartId => {
      val node = InterruptNode.slave()
      slave1.mapDownInterrupt(hartId, node)
      node
    })

    val masterTarget = hartIds.map(hartId => {
      val node = InterruptNode.slave()
      master.mapDownInterrupt(hartId, node)
      node
    })

    val rootTargetNode = InterruptNode.slave()
    root.mapDownInterrupt(0, rootTargetNode)

    root.addChildCtrl(master)
    master.addChildCtrl(slave0)
    master.addChildCtrl(slave1)

    val rootSource = sourceIds.map(sourceId => {
      val node = InterruptNode.master()
      root.mapUpInterrupt(sourceId, node, mode)
      node
    })
  }

  val io = new Bundle {
    val bus = slave(tilelink.Bus(masterBus.m2sParams.toNodeParameters().toBusParameter()))
    val sources = in Bits(sourceIds.size bits)
    val rootTarget = out Bits(1 bits)
    val masterTarget = out Bits(hartIds.size bits)
    val slave0Target = out Bits(hartIds.size bits)
    val slave1Target = out Bits(hartIds.size bits)
  }

  masterBus.bus = Some(io.bus)

  for ((bundle, source) <- peripherals.rootSource.zip(io.sources.asBools)) {
    bundle.flag := source
  }

  io.rootTarget := peripherals.rootTargetNode.flag.asBits
  io.masterTarget := peripherals.masterTarget.map(_.flag).asBits()
  io.slave0Target := peripherals.slave0Target.map(_.flag).asBits()
  io.slave1Target := peripherals.slave1Target.map(_.flag).asBits()
}

class SpinalSimAPlicTester extends SpinalSimFunSuite {
  onlyVerilator()

  import APlicTestHelper._

  val sourcenum = 64
  val hartnum = 8
  val guestNum = 2

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val hartIds = for (i <- 0 until hartnum) yield i
  val guestIds = for (i <- 0 to guestNum) yield i

  val slave0Addr = 0x10000000
  val slave1Addr = 0x20000000
  val masterAddr = 0x30000000
  val rootAddr   = 0x40000000
  val imsicAddr  = 0x50000000

  val simConfig = SpinalConfig(
                    mode = SystemVerilog,
                    defaultConfigForClockDomains = ClockDomainConfig(
                      resetActiveLevel = HIGH
                    ),
                    oneFilePerComponent = true,
                    removePruned = true,
                    verbose = true
                  )

  test("default data") {
    SimConfig.withConfig(simConfig).withFstWave.compile(
      new APlicDirectTestFiber(hartIds, sourceIds)
    ).doSim("default data"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0

      implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
      val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

      for (addr <- Seq(slave0Addr, slave1Addr, masterAddr, rootAddr)) {
        // domaincfg
        assertReg(agent.get(0, addr + aplicmap.domaincfgOffset, 4), 0x80000000, "Default data of <domaincfg>")

        // mmsiaddrcfg/h
        assertReg(agent.get(0, addr + aplicmap.mmsiaddrcfgOffset, 4), 0x0, "Default data of <mmsiaddrcfg>")
        assertReg(agent.get(0, addr + aplicmap.mmsiaddrcfghOffset, 4), 0x0, "Default data of <mmsiaddrcfgh>")

        // smsiaddrcfg/h
        assertReg(agent.get(0, addr + aplicmap.smsiaddrcfgOffset, 4), 0x0, "Default data of <smsiaddrcfg>")
        assertReg(agent.get(0, addr + aplicmap.smsiaddrcfghOffset, 4), 0x0, "Default data of <smsiaddrcfgh>")

        // setiep
        for (i <- 0 until (sourcenum - 1)/32) {
          assertReg(agent.get(0, addr + aplicmap.setipOffset + i * 4, 4), 0x0, s"Default data of <setipcfg_$i>")
          assertReg(agent.get(0, addr + aplicmap.setieOffset + i * 4, 4), 0x0, s"Default data of <setiecfg_$i>")
        }

        assertReg(agent.get(0, addr + aplicmap.setipnumOffset, 4), 0x0, "Default data of <setipnumcfg>")
        assertReg(agent.get(0, addr + aplicmap.setienumOffset, 4), 0x0, "Default data of <setienumcfg>")
        // should return rectified value, re-test below. ch:4.5.7
        assertReg(agent.get(0, addr + aplicmap.in_clripOffset, 4), 0x0, "Default data of <in_clripcfg1>")
        assertReg(agent.get(0, addr + aplicmap.clripnumOffset, 4), 0x0, "Default data of <clripnumcfg>")
        assertReg(agent.get(0, addr + aplicmap.clrieOffset, 4), 0x0, "Default data of <clrieOffsetcfg>")
        assertReg(agent.get(0, addr + aplicmap.clrienumOffset, 4), 0x0, "Default data of <clrienumOffsetcfg>")
        // setipnum_le/be not implementation
        assertReg(agent.get(0, addr + aplicmap.genmsiOffset, 4), 0x0, "Default data of <genmsicfg>")

        for (i <- 0 until (sourcenum - 1)) {
          assertReg(agent.get(0, addr + aplicmap.sourcecfgOffset + i * 4, 4), 0x0, s"Default data of <sourcecfg_$i>")
          assertReg(agent.get(0, addr + aplicmap.targetOffset + i * 4, 4), 0x1, s"Default data of <targetcfg_$i>")
        }

        for (i <- 0 until hartnum) {
          assertReg(agent.get(0, addr + aplicmap.idcOffset + aplicmap.ideliveryOffset + i * aplicmap.idcGroupSize, 4), 0x0, s"Default data of <ideliverycfg_$i>")
          assertReg(agent.get(0, addr + aplicmap.idcOffset + aplicmap.iforceOffset + i * aplicmap.idcGroupSize, 4), 0x0, s"Default data of <iforcecfg_$i>")
          assertReg(agent.get(0, addr + aplicmap.idcOffset + aplicmap.ithresholdOffset + i * aplicmap.idcGroupSize, 4), 0x0, s"Default data of <ithresholdcfg_$i>")
          assertReg(agent.get(0, addr + aplicmap.idcOffset + aplicmap.topiOffset + i * aplicmap.idcGroupSize, 4), 0x0, s"Default data of <topicfg_$i>")
          assertReg(agent.get(0, addr + aplicmap.idcOffset + aplicmap.claimiOffset + i * aplicmap.idcGroupSize, 4), 0x0, s"Default data of <claimicfg_$i>")
        }
      }
      dut.clockDomain.waitRisingEdge(100)
    }
  }

  test("ie test") {
    SimConfig.withConfig(simConfig).withFstWave.compile(
      new APlicDirectTestFiber(hartIds, sourceIds)
    ).doSim("ie test"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0

      val agent = initAPlicMasterOnlyDirect(dut)
      agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))

      dut.clockDomain.waitRisingEdge(10)

      for (sourceId <- sourceIds) {
        val offset = sourceId / 32 * 4
        // setienum
        assertBit(agent.get(0, masterAddr + aplicmap.setieOffset + offset, 4), sourceId, true, s"use <setienum> to make setie[${sourceId / 32}][${sourceId % 32}] <true>")
        // clrienum
        if (sourceId < 31) {
          agent.putFullData(0, masterAddr + aplicmap.clrienumOffset, SimUInt32(sourceId))
          assertBit(agent.get(0, masterAddr + aplicmap.setieOffset + offset, 4), sourceId, false, s"use <clrienum> to make setie[${sourceId / 32}][${sourceId % 32}] <false>")
        } else {
          agent.putFullData(0, masterAddr + aplicmap.clrieOffset + offset, SimUInt32(BigInt(1) << (sourceId % 32)))
          assertBit(agent.get(0, masterAddr + aplicmap.setieOffset + offset, 4), sourceId, false, s"use <clrieOffset> to make setie[${sourceId / 32}][${sourceId % 32}] <false>")
        }
      }
      dut.clockDomain.waitRisingEdge(100)
    }
  }

  test("level high gateway test") {
    SimConfig.withConfig(simConfig).withFstWave.compile(
      new APlicDirectTestFiber(hartIds, sourceIds)
    ).doSim("level high gateway test"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0

      val agent = initAPlicMasterOnlyDirect(dut)
      agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))

      dut.clockDomain.waitRisingEdge(10)

      for (sourceId <- sourceIds) {
        val offset = sourceId / 32 * 4
        dut.io.sources #= BigInt("0", 16).setBit(sourceId - 1)
        dut.clockDomain.waitRisingEdge(3)
        assertBit(dut.io.masterTarget.toBigInt, (sourceId - 1) % hartnum, true, "trigger ip in level high")

        // clripnum
        agent.putFullData(0, masterAddr + aplicmap.clripnumOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, true, s"use <clripnum> to make setip[${sourceId / 32}][${sourceId % 32}] <false> in level high mode")

        // claimi
        agent.get(0, masterAddr + aplicmap.idcOffset + (((sourceId - 1) % hartnum) * aplicmap.idcGroupSize) + aplicmap.claimiOffset, 4)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, true, s"read reg <claimi> to claim ip in level high mode")

        dut.io.sources #= 0x0
        dut.clockDomain.waitRisingEdge(3)
        assertBit(dut.io.masterTarget.toBigInt, (sourceId - 1) % hartnum, false, "un-trigger ip in level high, sourceId = $sourceId")

        // setipnum, but the option will fail in level high mode.
        agent.putFullData(0, masterAddr + aplicmap.setipnumOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, false, s"use <setipnum> to make setip[${sourceId / 32}][${sourceId % 32}] <true> in level high mode")

        // setipnum_le, but the option will fail in level high mode.
        agent.putFullData(0, masterAddr + aplicmap.setipnum_leOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, false, s"use <setipnum> to make setip[${sourceId / 32}][${sourceId % 32}] <true> in level high mode")

        // setipnum_be reg is not support.
      }

      dut.clockDomain.waitRisingEdge(100)
    }
  }

  test("level low gateway test") {
    SimConfig.withConfig(simConfig).withFstWave.compile(
      new APlicDirectTestFiber(hartIds, sourceIds, LEVEL_LOW)
    ).doSim("level low gateway test"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= BigInt("7fffffffffffffff", 16)

      val agent = initAPlicMasterOnlyDirect(dut, 7)
      agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))

      dut.clockDomain.waitRisingEdge(10)

      for (sourceId <- sourceIds) {
        val offset = sourceId / 32 * 4
        dut.io.sources #= BigInt("7fffffffffffffff", 16).clearBit(sourceId - 1)
        dut.clockDomain.waitRisingEdge(3)
        assertBit(dut.io.masterTarget.toBigInt, (sourceId - 1) % hartnum, true, "trigger ip in level low")

        // clripnum
        agent.putFullData(0, masterAddr + aplicmap.clripnumOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, true, s"use <clripnum> to make setip[${sourceId / 32}][${sourceId % 32}] <false> in level low mode")

        // claimi
        agent.get(0, masterAddr + aplicmap.idcOffset + (((sourceId - 1) % hartnum) * aplicmap.idcGroupSize) + aplicmap.claimiOffset, 4)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, true, s"read reg <claimi> to claim ip in level low mode")

        dut.io.sources #= BigInt("7fffffffffffffff", 16)
        dut.clockDomain.waitRisingEdge(3)
        assertBit(dut.io.masterTarget.toBigInt, (sourceId - 1) % hartnum, false, "un-trigger ip in level low, sourceId = $sourceId")

        // setipnum, but the option will fail in level low mode.
        agent.putFullData(0, masterAddr + aplicmap.setipnumOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, false, s"use <setipnum> to make setip[${sourceId / 32}][${sourceId % 32}] <true> in level low mode")

        // setipnum_le, but the option will fail in level low mode.
        agent.putFullData(0, masterAddr + aplicmap.setipnum_leOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, false, s"use <setipnum> to make setip[${sourceId / 32}][${sourceId % 32}] <true> in level low mode")

        // setipnum_be reg is not support.
      }

      dut.clockDomain.waitRisingEdge(100)
    }
  }

  test("edge falling gateway test") {
    SimConfig.withConfig(simConfig).withFstWave.compile(
      new APlicDirectTestFiber(hartIds, sourceIds, EDGE_FALLING)
    ).doSim("edge falling gateway test"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= BigInt("7fffffffffffffff", 16)

      val agent = initAPlicMasterOnlyDirect(dut, 5)
      agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))

      dut.clockDomain.waitRisingEdge(10)

      for (sourceId <- sourceIds) {
        val offset = sourceId / 32 * 4
        dut.io.sources #= BigInt("7fffffffffffffff", 16).clearBit(sourceId - 1)
        dut.clockDomain.waitRisingEdge(3)
        dut.io.sources #= BigInt("7fffffffffffffff", 16)
        assertBit(dut.io.masterTarget.toBigInt, (sourceId - 1) % hartnum, true, "trigger ip in edge falling")

        // clripnum
        agent.putFullData(0, masterAddr + aplicmap.clripnumOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, false, s"use <clripnum> to make setip[${sourceId / 32}][${sourceId % 32}] <false> in edge falling mode")

        dut.clockDomain.waitRisingEdge(3)
        assertBit(dut.io.masterTarget.toBigInt, (sourceId - 1) % hartnum, false, s"test IO when clear setip, sourceId = $sourceId")

        // setipnum
        agent.putFullData(0, masterAddr + aplicmap.setipnumOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, true, s"use <setipnum> to make setip[${sourceId / 32}][${sourceId % 32}] <true> in edge falling mode")

        // claimi
        agent.get(0, masterAddr + aplicmap.idcOffset + (((sourceId - 1) % hartnum) * aplicmap.idcGroupSize) + aplicmap.claimiOffset, 4)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, false, s"read reg <claimi> to claim ip in edge falling mode")

        agent.putFullData(0, masterAddr + aplicmap.clripnumOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)

        // setipnum_le
        agent.putFullData(0, masterAddr + aplicmap.setipnum_leOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, true, s"use <setipnum> to make setip[${sourceId / 32}][${sourceId % 32}] <true> in edge falling mode")

        dut.clockDomain.waitRisingEdge(3)
        assertBit(dut.io.masterTarget.toBigInt, (sourceId - 1) % hartnum, true, s"test IO when clear setip, sourceId = $sourceId")

        agent.putFullData(0, masterAddr + aplicmap.clripnumOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)

        // setipnum_be reg is not support.
      }

      dut.clockDomain.waitRisingEdge(100)
    }
  }

  test("edge rising gateway test") {
    SimConfig.withConfig(simConfig).withFstWave.compile(
      new APlicDirectTestFiber(hartIds, sourceIds, EDGE_RISING)
    ).doSim("edge rising gateway test"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= BigInt("0", 16)

      val agent = initAPlicMasterOnlyDirect(dut, 4)
      agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))

      dut.clockDomain.waitRisingEdge(10)

      for (sourceId <- sourceIds) {
        val offset = sourceId / 32 * 4
        dut.io.sources #= BigInt("0", 16).setBit(sourceId - 1)
        dut.clockDomain.waitRisingEdge(3)
        dut.io.sources #= BigInt("0", 16)
        assertBit(dut.io.masterTarget.toBigInt, (sourceId - 1) % hartnum, true, "trigger ip in edge rising")

        // clripnum
        agent.putFullData(0, masterAddr + aplicmap.clripnumOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, false, s"use <clripnum> to make setip[${sourceId / 32}][${sourceId % 32}] <false> in edge rising mode")

        dut.clockDomain.waitRisingEdge(3)
        assertBit(dut.io.masterTarget.toBigInt, (sourceId - 1) % hartnum, false, s"test IO when clear setip, sourceId = $sourceId")

        // setipnum
        agent.putFullData(0, masterAddr + aplicmap.setipnumOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, true, s"use <setipnum> to make setip[${sourceId / 32}][${sourceId % 32}] <true> in edge rising mode")

        dut.clockDomain.waitRisingEdge(3)
        assertBit(dut.io.masterTarget.toBigInt, (sourceId - 1) % hartnum, true, s"test IO when clear setip, sourceId = $sourceId")

        // claimi
        agent.get(0, masterAddr + aplicmap.idcOffset + (((sourceId - 1) % hartnum) * aplicmap.idcGroupSize) + aplicmap.claimiOffset, 4)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, false, s"read reg <claimi> to claim ip in edge rising mode")

        // setipnum_le
        agent.putFullData(0, masterAddr + aplicmap.setipnum_leOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, true, s"use <setipnum> to make setip[${sourceId / 32}][${sourceId % 32}] <true> in edge rising mode")

        dut.clockDomain.waitRisingEdge(3)
        assertBit(dut.io.masterTarget.toBigInt, (sourceId - 1) % hartnum, true, s"test IO when clear setip, sourceId = $sourceId")

        agent.putFullData(0, masterAddr + aplicmap.clripnumOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)

        // setipnum_be reg is not support.
      }

      dut.clockDomain.waitRisingEdge(100)
    }
  }

  test("detached gateway test") {
    SimConfig.withConfig(simConfig).withFstWave.compile(
      new APlicDirectTestFiber(hartIds, sourceIds, SPURIOUS)
    ).doSim("detached gateway test"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0

      val agent = initAPlicMasterOnlyDirect(dut, 1)
      agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))

      dut.clockDomain.waitRisingEdge(10)

      for (sourceId <- sourceIds) {
        val offset = sourceId / 32 * 4
        dut.io.sources #= BigInt("0", 16).setBit(sourceId - 1)
        dut.clockDomain.waitRisingEdge(3)
        assertBit(dut.io.masterTarget.toBigInt, (sourceId - 1) % hartnum, false, "trigger ip in detached mode")

        dut.io.sources #= 0x0
        dut.clockDomain.waitRisingEdge(3)

        // setipnum
        agent.putFullData(0, masterAddr + aplicmap.setipnumOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, true, s"use <setipnum> to make setip[${sourceId / 32}][${sourceId % 32}] <true> in level high mode")

        agent.putFullData(0, masterAddr + aplicmap.clripnumOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, false, s"use <clripnum> to make setip[${sourceId / 32}][${sourceId % 32}] <false> in level high mode")

        // setipnum_le, but the option will fail in level high mode.
        agent.putFullData(0, masterAddr + aplicmap.setipnum_leOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, true, s"use <setipnum> to make setip[${sourceId / 32}][${sourceId % 32}] <true> in level high mode")

        // claimi
        agent.get(0, masterAddr + aplicmap.idcOffset + (((sourceId - 1) % hartnum) * aplicmap.idcGroupSize) + aplicmap.claimiOffset, 4)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, false, s"read reg <claimi> to claim ip in detached mode")

        // setipnum_be reg is not support.
      }

      dut.clockDomain.waitRisingEdge(100)
    }
  }

  test("inactive gateway test") {
    SimConfig.withConfig(simConfig).withFstWave.compile(
      new APlicDirectTestFiber(hartIds, sourceIds, SPURIOUS)
    ).doSim("inactive gateway test"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0

      val agent = initAPlicMasterOnlyDirect(dut, 0)
      agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))

      dut.clockDomain.waitRisingEdge(10)

      for (sourceId <- sourceIds) {
        val offset = sourceId / 32 * 4
        dut.io.sources #= BigInt("0", 16).setBit(sourceId - 1)
        dut.clockDomain.waitRisingEdge(3)
        assertBit(dut.io.masterTarget.toBigInt, (sourceId - 1) % hartnum, false, "trigger ip in inactive")

        dut.io.sources #= 0x0

        // setipnum
        agent.putFullData(0, masterAddr + aplicmap.setipnumOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, false, s"use <setipnum> to make setip[${sourceId / 32}][${sourceId % 32}] <true> in level high mode")

        // setipnum_le
        agent.putFullData(0, masterAddr + aplicmap.setipnum_leOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(2)
        assertBit(agent.get(0, masterAddr + aplicmap.setipOffset + offset, 4), sourceId, false, s"use <setipnum> to make setip[${sourceId / 32}][${sourceId % 32}] <true> in level high mode")

        // setipnum_be reg is not support.
      }

      dut.clockDomain.waitRisingEdge(100)
    }
  }

  test("priority test") {
    SimConfig.withConfig(simConfig).withFstWave.compile(
      new APlicDirectTestFiber(hartIds, sourceIds)
    ).doSim("priority test"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0

      val agent = initAPlicMasterOnlyDirect(dut)
      for (sourceId <- sourceIds) {
        val offset = (sourceId - 1) * 4
        agent.putFullData(0, masterAddr + aplicmap.targetOffset + offset, SimUInt32((1 | (0 << 18)) & 0xFFFFFFFF))
      }
      agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))

      dut.clockDomain.waitRisingEdge(10)

      dut.io.sources #= BigInt("4", 16)
      dut.clockDomain.waitRisingEdge(3)
      assertReg(agent.get(0, masterAddr + aplicmap.idcOffset + aplicmap.topiOffset, 4), 0x30001, "id: assert topi when bestrequest id = 4")

      dut.io.sources #= BigInt("6", 16)
      dut.clockDomain.waitRisingEdge(3)
      assertReg(agent.get(0, masterAddr + aplicmap.idcOffset + aplicmap.topiOffset, 4), 0x20001, "id: assert topi when bestrequest id = 3")

      dut.io.sources #= BigInt("7", 16)
      dut.clockDomain.waitRisingEdge(3)
      assertReg(agent.get(0, masterAddr + aplicmap.idcOffset + aplicmap.topiOffset, 4), 0x10001, "id: assert topi when bestrequest id = 2")

      dut.io.sources #= BigInt("0", 16)

      agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000000))

      for (sourceId <- sourceIds) {
        val offset = (sourceId - 1) * 4
        agent.putFullData(0, masterAddr + aplicmap.targetOffset + offset, SimUInt32(((sourcenum - sourceId) | (0 << 18)) & 0xFFFFFFFF))
      }
      agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))

      dut.io.sources #= BigInt("2", 16)
      dut.clockDomain.waitRisingEdge(3)
      assertReg(agent.get(0, masterAddr + aplicmap.idcOffset + aplicmap.topiOffset, 4), 0x2003e, "ipro: assert topi when bestrequest id = 2")

      dut.io.sources #= BigInt("3", 16)
      dut.clockDomain.waitRisingEdge(3)
      assertReg(agent.get(0, masterAddr + aplicmap.idcOffset + aplicmap.topiOffset, 4), 0x2003e, "ipro: assert topi when bestrequest id = 2")

      dut.io.sources #= BigInt("7", 16)
      dut.clockDomain.waitRisingEdge(3)
      assertReg(agent.get(0, masterAddr + aplicmap.idcOffset + aplicmap.topiOffset, 4), 0x3003d, "ipro: assert topi when bestrequest id = 3")

      dut.clockDomain.waitRisingEdge(100)
    }
  }

  test("threshold test") {
    SimConfig.withConfig(simConfig).withFstWave.compile(
      new APlicDirectTestFiber(hartIds, sourceIds)
    ).doSim("threshold test"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0

      val agent = initAPlicMasterOnlyDirect(dut)
      for (sourceId <- sourceIds) {
        val offset = (sourceId - 1) * 4
        agent.putFullData(0, masterAddr + aplicmap.targetOffset + offset, SimUInt32((sourceId | (0 << 18)) & 0xFFFFFFFF))
      }
      for (hartId <- hartIds) {
        agent.putFullData(0, masterAddr + aplicmap.idcOffset + aplicmap.ithresholdOffset + hartId * aplicmap.idcGroupSize, SimUInt32(0x5))
      }
      agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))

      dut.clockDomain.waitRisingEdge(10)

      dut.io.sources #= BigInt("10", 16)
      dut.clockDomain.waitRisingEdge(3)
      assertBit(dut.io.masterTarget.toBigInt, 0, false, "trigger ip but iprio > threshold")

      dut.io.sources #= BigInt("2", 16)
      dut.clockDomain.waitRisingEdge(3)
      assertBit(dut.io.masterTarget.toBigInt, 0, true, "trigger ip but iprio < threshold")

      dut.clockDomain.waitRisingEdge(100)
    }
  }

  test("delegate test") {
    SimConfig.withConfig(simConfig).withFstWave.compile(
      new APlicDirectTestFiber(hartIds, sourceIds)
    ).doSim("delegate test"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0

      val agent = initAPlicAllDirect(dut)
      agent.putFullData(0, slave0Addr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))
      agent.putFullData(0, slave1Addr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))

      dut.clockDomain.waitRisingEdge(10)

      for (sourceId <- sourceIds) {
        val offset = sourceId / 32 * 4
        dut.io.sources #= BigInt("0", 16).setBit(sourceId - 1)
        dut.clockDomain.waitRisingEdge(3)
        if (sourceId < 32)
          assertBit(dut.io.slave0Target.toBigInt, (sourceId - 1) % hartnum, true, "delegate source to slave0 in level high mode")
        else
          assertBit(dut.io.slave1Target.toBigInt, (sourceId - 1) % hartnum, true, "delegate source to slave1 in level high mode")
      }

      dut.clockDomain.waitRisingEdge(100)
    }
  }

  test("msi sender test") {
    SimConfig.withConfig(simConfig).withFstWave.compile(
      new APlicMSITestFiber(hartIds, sourceIds, guestIds)
    ).doSim("msi sender test"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0

      val agent = initAPlicMasterOnlyMsi(dut)

      dut.clockDomain.waitRisingEdge(10)

      for (sourceId <- sourceIds) {
        val offset = sourceId / 32 * 4
        val thisHartId = (sourceId - 1) % hartnum
        val thisGuestId = (sourceId - 1) % (guestNum+1)

        dut.io.sources #= BigInt("0", 16).setBit(sourceId - 1)
        dut.clockDomain.waitRisingEdge(4)
        assertBit(dut.io.ip(thisHartId)(thisGuestId).toBigInt, sourceId - 1, true, s"trigger target at ip($thisHartId)($thisGuestId)")
        assert(dut.io.identity(thisHartId)(thisGuestId).toBigInt == sourceId, s"check trigger target identity at ip($thisHartId)($thisGuestId)")
        dut.files(thisHartId)(thisGuestId).interrupts(sourceId - 1).ip #= false
      }

      dut.clockDomain.waitRisingEdge(100)
    }
  }

  test("re-trigger msi test") {
    SimConfig.withConfig(simConfig).withFstWave.compile(
      new APlicMSITestFiber(hartIds, sourceIds, guestIds)
    ).doSim("re-trigger msi test"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0

      val agent = initAPlicMasterOnlyMsi(dut)

      dut.clockDomain.waitRisingEdge(10)

      for (sourceId <- sourceIds) {
        val offset = sourceId / 32 * 4
        val thisHartId = (sourceId - 1) % hartnum
        val thisGuestId = (sourceId - 1) % (guestNum+1)

        // rectified_source = 1 but use setipnum trigger eip twice
        dut.io.sources #= BigInt("0", 16).setBit(sourceId - 1)
        dut.clockDomain.waitRisingEdge(4)
        assertBit(dut.io.ip(thisHartId)(thisGuestId).toBigInt, sourceId - 1, true, s"trigger ip($thisHartId)($thisGuestId)(${sourceId - 1}) by source(${sourceId-1}) when rectified_source = 1")

        dut.files(thisHartId)(thisGuestId).interrupts(sourceId - 1).ip #= false

        // setipnum
        agent.putFullData(0, masterAddr + aplicmap.setipnumOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(4)
        assertBit(dut.io.ip(thisHartId)(thisGuestId).toBigInt, sourceId - 1, true, s"trigger ip($thisHartId)($thisGuestId)(${sourceId - 1}) by setipnum when rectified_source = 1")

        dut.files(thisHartId)(thisGuestId).interrupts(sourceId - 1).ip #= false

        // setipnum_le
        agent.putFullData(0, masterAddr + aplicmap.setipnum_leOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(4)
        assertBit(dut.io.ip(thisHartId)(thisGuestId).toBigInt, sourceId - 1, true, s"trigger ip($thisHartId)($thisGuestId)(${sourceId - 1}) by setipnum_le when rectified_source = 1")

        dut.files(thisHartId)(thisGuestId).interrupts(sourceId - 1).ip #= false

        // rectified_source = 0, can not trigger eip through setipnum
        dut.io.sources #= BigInt("0", 16)
        dut.clockDomain.waitRisingEdge(3)
        assertBit(dut.io.ip(thisHartId)(thisGuestId).toBigInt, sourceId - 1, false, s"check ip($thisHartId)($thisGuestId)(${sourceId - 1}) when source(${sourceId-1}) = 0")

        // setipnum
        agent.putFullData(0, masterAddr + aplicmap.setipnumOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(4)
        assertBit(dut.io.ip(thisHartId)(thisGuestId).toBigInt, sourceId - 1, false, s"trigger ip($thisHartId)($thisGuestId)(${sourceId - 1}) by setipnum when rectified_source = 0")

        // setipnum_le
        agent.putFullData(0, masterAddr + aplicmap.setipnum_leOffset, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(4)
        assertBit(dut.io.ip(thisHartId)(thisGuestId).toBigInt, sourceId - 1, false, s"trigger ip($thisHartId)($thisGuestId)(${sourceId - 1}) by setipnum_le when rectified_source = 0")
      }

      dut.clockDomain.waitRisingEdge(100)
    }
  }

  test("genmsi test") {
    SimConfig.withConfig(simConfig).withFstWave.compile(
      new APlicMSITestFiber(hartIds, sourceIds, guestIds)
    ).doSim("genmsi test"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0

      val agent = initAPlicMasterOnlyMsi(dut)

      dut.clockDomain.waitRisingEdge(10)

      for (sourceId <- sourceIds) {
        val offset = sourceId / 32 * 4
        val thisHartId = (sourceId - 1) % hartnum
        val thisGuestId = (sourceId - 1) % (guestNum+1)

        val randomHartid = Random.nextInt(hartnum)

        agent.putFullData(0, masterAddr + aplicmap.genmsiOffset, SimUInt32(randomHartid << 18 | sourceId))
        dut.clockDomain.waitRisingEdge(4)
        assertBit(dut.io.ip(randomHartid)(0).toBigInt, sourceId - 1, true, s"trigger ip($randomHartid)(0)(${sourceId - 1}) by genmsi")
        dut.files(randomHartid)(0).interrupts(sourceId - 1).ip #= false
      }

      dut.clockDomain.waitRisingEdge(100)
    }
  }

  test("iforce test") {
    SimConfig.withConfig(simConfig).withFstWave.compile(
      new APlicMSITestFiber(hartIds, sourceIds, guestIds)
    ).doSim("iforce test"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0

      implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
      val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

      for (sourceId <- sourceIds) {
        val offset = (sourceId - 1) * 4
        agent.putFullData(0, rootAddr + aplicmap.sourcecfgOffset + offset, SimUInt32(0x400))
        agent.putFullData(0, masterAddr + aplicmap.sourcecfgOffset + offset, SimUInt32(6))
        agent.putFullData(0, masterAddr + aplicmap.setienumOffset, SimUInt32(sourceId))
        agent.putFullData(0, masterAddr + aplicmap.targetOffset + offset, SimUInt32((1 | ((sourceId - 1) % hartnum << 18)) & 0xFFFFFFFF))
      }
      for (hartId <- hartIds) {
        agent.putFullData(0, masterAddr + aplicmap.idcOffset + aplicmap.ideliveryOffset + hartId * aplicmap.idcGroupSize, SimUInt32(0x1))
      }

      agent.putFullData(0, rootAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))
      agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))

      dut.clockDomain.waitRisingEdge(10)

      for (hartId <- hartIds) {
        agent.putFullData(0, masterAddr + aplicmap.idcOffset + aplicmap.iforceOffset + hartId * aplicmap.idcGroupSize, SimUInt32(0x1))
        dut.clockDomain.waitRisingEdge(4)
        assertBit(dut.io.masterTarget.toBigInt, hartId, true, s"trigger masterTraget($hartId) by iforce")
      }

      dut.clockDomain.waitRisingEdge(100)
    }
  }

  def initAPlicMasterOnlyDirect(dut: APlicDirectTestFiber, mode: BigInt = 6): tilelink.sim.MasterAgent = {
    implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
    val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

    for (sourceId <- sourceIds) {
      val offset = (sourceId - 1) * 4
      agent.putFullData(0, rootAddr + aplicmap.sourcecfgOffset + offset, SimUInt32(0x400))
      agent.putFullData(0, masterAddr + aplicmap.sourcecfgOffset + offset, SimUInt32(mode))
      agent.putFullData(0, masterAddr + aplicmap.setienumOffset, SimUInt32(sourceId))
      agent.putFullData(0, masterAddr + aplicmap.targetOffset + offset, SimUInt32((1 | ((sourceId - 1) % hartnum << 18)) & 0xFFFFFFFF))
    }
    for (hartId <- hartIds) {
      agent.putFullData(0, masterAddr + aplicmap.idcOffset + aplicmap.ideliveryOffset + hartId * aplicmap.idcGroupSize, SimUInt32(0x1))
    }

    agent.putFullData(0, rootAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))
    agent
  }

  def initAPlicAllDirect(dut: APlicDirectTestFiber, mode: BigInt = 6): tilelink.sim.MasterAgent = {
    implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
    val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

    for (sourceId <- sourceIds) {
      val offset = (sourceId - 1) * 4
      agent.putFullData(0, rootAddr + aplicmap.sourcecfgOffset + offset, SimUInt32(0x400))
      if (sourceId < 32)
        agent.putFullData(0, masterAddr + aplicmap.sourcecfgOffset + offset, SimUInt32(0x400))
      else
        agent.putFullData(0, masterAddr + aplicmap.sourcecfgOffset + offset, SimUInt32(0x401))
      agent.putFullData(0, slave0Addr + aplicmap.sourcecfgOffset + offset, SimUInt32(mode))
      agent.putFullData(0, slave1Addr + aplicmap.sourcecfgOffset + offset, SimUInt32(mode))
      agent.putFullData(0, slave0Addr + aplicmap.setienumOffset, SimUInt32(sourceId))
      agent.putFullData(0, slave1Addr + aplicmap.setienumOffset, SimUInt32(sourceId))
      agent.putFullData(0, slave0Addr + aplicmap.targetOffset + offset, SimUInt32((1 | ((sourceId - 1) % hartnum << 18)) & 0xFFFFFFFF))
      agent.putFullData(0, slave1Addr + aplicmap.targetOffset + offset, SimUInt32((1 | ((sourceId - 1) % hartnum << 18)) & 0xFFFFFFFF))
    }
    for (hartId <- hartIds) {
      agent.putFullData(0, slave0Addr + aplicmap.idcOffset + aplicmap.ideliveryOffset + hartId * aplicmap.idcGroupSize, SimUInt32(0x1))
      agent.putFullData(0, slave1Addr + aplicmap.idcOffset + aplicmap.ideliveryOffset + hartId * aplicmap.idcGroupSize, SimUInt32(0x1))
    }

    agent.putFullData(0, rootAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))
    agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))
    agent
  }

  def initAPlicMasterOnlyMsi(dut: APlicMSITestFiber, mode: BigInt = 6): tilelink.sim.MasterAgent = {
    implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
    val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

    dut.io.ie.map(_.map(_ #= BigInt("7fffffffffffffff", 16)))

    agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000004))

    agent.putFullData(0, rootAddr + aplicmap.mmsiaddrcfgOffset, SimUInt32(imsicAddr>>12))
    agent.putFullData(0, rootAddr + aplicmap.mmsiaddrcfghOffset, SimUInt32(0x203000))
    agent.putFullData(0, rootAddr + aplicmap.smsiaddrcfgOffset, SimUInt32(imsicAddr>>12))
    agent.putFullData(0, rootAddr + aplicmap.smsiaddrcfghOffset, SimUInt32(0x200000))

    for (sourceId <- sourceIds) {
      val offset = (sourceId - 1) * 4
      agent.putFullData(0, rootAddr + aplicmap.sourcecfgOffset + offset, SimUInt32(0x400))
      agent.putFullData(0, masterAddr + aplicmap.sourcecfgOffset + offset, SimUInt32(6))
      agent.putFullData(0, masterAddr + aplicmap.setienumOffset, SimUInt32(sourceId))
      agent.putFullData(0, masterAddr + aplicmap.targetOffset + offset, SimUInt32((sourceId | ((sourceId - 1) % (guestNum+1) << 12) | ((sourceId - 1) % hartnum << 18)) & 0xFFFFFFFF))
    }

    agent.putFullData(0, rootAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))
    agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000104))
    agent
  }
}

object APlicTestHelper {
  val aplicmap = APlicMapping

  def assertReg(data: tilelink.sim.TransactionD, expect: Int, name: String): Unit = {
    val value = data.data
    val actualHex = value.reverse.map("%02x".format(_)).mkString
    val expectHex = f"$expect%x"
    // 32bit width
    val result = value.sameElements(expect.toBytes.slice(0, 4))
    assert(result,
      s"$name: check data failed:\n  expected = 0x$expectHex\n  actual   = 0x$actualHex"
    )
  }

  def assertBit(data: tilelink.sim.TransactionD, id: Int, expect: Boolean, name: String): Unit = {
    val value = data.data
    val idx = id % 32
    val byteIndex = idx / 8
    val bitIndex = idx % 8

    val result = value(byteIndex).toBigInt.testBit(bitIndex) == expect

    assert(result,
      s"\n  $name: check reg bit failed:\n  source id = $id\n  expected = $expect"
    )
  }

  def assertBit(data: BigInt, id: Int, expect: Boolean, name: String): Unit = {
    assert(data.testBit(id) == expect,
    s"\n  $name: check IO bit failed:\n  data = 0x${data.toString(16)}\n  bit index = $id\n  expected = $expect\n"
    )
  }
}

class SimUIntFix(width: Int)(number: BigInt, endian: Endianness = LITTLE) {
  val bits = width
  val value = number
  val endianness = endian
}

object SimUIntFix {
  implicit def simUintFix2SeqByte(intx: SimUIntFix): Seq[Byte] = {
    intx.value.toBytes(intx.bits, intx.endianness).toIndexedSeq
  }
}

object SimUInt32 {
  def apply(number: BigInt, endian: Endianness = LITTLE): SimUIntFix = new SimUIntFix(32)(number, endian)
}

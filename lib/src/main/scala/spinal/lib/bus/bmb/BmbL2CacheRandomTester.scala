
import spinal.core.sim._
import spinal.lib.bus.bmb.BmbL2Cache._
import spinal.lib.bus.bmb.sim.{BmbMasterAgent, BmbMemoryAgent, BmbMonitor, BmbRegionAllocator}
import spinal.lib.bus.bmb.{Bmb, BmbAccessParameter, BmbExclusiveMonitor, BmbInvalidationParameter, BmbL2Cache, BmbL2CacheParameter, BmbParameter, BmbSourceParameter}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.sim.{Phase, SparseMemory}

import scala.util.Random
object BmbL2CacheRandomTester extends App{
  val addrBusSize = 24
  val addrRandomSize = 14
  assert(addrRandomSize <= addrBusSize)

  val p = BmbL2CacheParameter(
    lineLength = 64,
    waySize = 32768,
    wayCount = 1

  )
  val ip = BmbParameter(
    access =  BmbAccessParameter(
      addressWidth = 31,
      dataWidth = 32
    ).addSources(
      count = 8,
      p = BmbSourceParameter(
        contextWidth               = 9,
        lengthWidth                = 6,
        alignment                  = BmbParameter.BurstAlignement.WORD,
        alignmentMin               = 0,
        accessLatencyMin           = 1,
        canRead                    = true,
        canWrite                   = true,
        canExclusive               = true,
        withCachedRead             = true
      )
    ),
    invalidation = BmbInvalidationParameter(
      canInvalidate = false,
      canSync = false,
      invalidateLength = 6,
      invalidateAlignment = BmbParameter.BurstAlignement.BYTE
    )
  )
  val op = BmbParameter(
    access =  BmbAccessParameter(
      addressWidth = 31,
      dataWidth = 32
    ).addSources(
      count = 1,
      p = BmbSourceParameter(
        contextWidth               = 18,
        lengthWidth                = 6,
        alignment                  = BmbParameter.BurstAlignement.LENGTH,
        alignmentMin               = 0,
        accessLatencyMin           = 1,
        canRead                    = true,
        canWrite                   = true,
        canExclusive               = false,
        withCachedRead             = true
      )
    ),
    invalidation = BmbInvalidationParameter(
      canInvalidate = false,
      canSync = false,
      invalidateLength = 6,
      invalidateAlignment = BmbParameter.BurstAlignement.BYTE
    )
  )

  val compiled = SimConfig.withFstWave.compile {
  //val compiled = SimConfig.compile {
    val dut = BmbL2Cache(p, ip, op)
    dut.s2.input.simPublic()
    dut.s2.S2out.simPublic()
    dut
  }
  compiled.doSim(seed = 42){dut =>
    dut.clockDomain.forkStimulus(10000)

    dut.io.input.cmd.valid #= false
    dut.io.input.rsp.ready #= false
    //dut.io.input.inv.valid #= false
    //dut.io.input.ack.ready #= false
    //dut.io.input.sync.valid #= false

    val lineShift = 6
    val lineMask = (1 << 9) - 1
    def getLine(addr : Int) = (addr >> lineShift) & lineMask

    val memory = new BmbMemoryAgent()

    memory.addPort(
      bus = dut.io.output,
      busAddress = 0,
      clockDomain = dut.clockDomain,
      withDriver = true
    )

    val cacheState = SparseMemory()

    for(i <- 0 until (1 << addrBusSize)) {
      val randomVal = Random.nextInt() & 0xFF
      memory.memory.write(i, randomVal)
      cacheState.write(i, randomVal)
      memory.memory.write(i, i)
      cacheState.write(i, i)
    }

    val regions = BmbRegionAllocator()
    val agent = new BmbMasterAgent(dut.io.input, dut.clockDomain) {
      override def getCmd(): () => Unit = super.getCmd()

      override def onRspRead(address: BigInt, data: Byte): Unit = {
        //print (f"addr 0x$address%X\n")
        val refData = cacheState.read(address.longValue())
        val lineId = getLine(address.toInt)
        if(refData != data){
          simFailure(f"Invalid data output from cache got 0x$data%02X expected 0x$refData%02X at 0x$address%X (line 0x$lineId)")
        }
      }

      override def onCmdWrite(address: BigInt, data: Byte): Unit = {
        cacheState.write(address.longValue(), data)
      }

      override def regionAllocate(sizeMax: Int): SizeMapping = regions.allocate(Random.nextInt(1 << addrRandomSize), sizeMax, dut.io.input.p, boundarySize=64)
      override def regionFree(region: SizeMapping): Unit = regions.free(region)
      override def regionIsMapped(region: SizeMapping, opcode: Int): Boolean = true
    }

    var opCount : BigInt = 0
    var missCount : BigInt = 0

    val opMonitor = fork {
      while(true) {
        dut.clockDomain.waitSamplingWhere(dut.s2.input.valid.toBoolean && dut.s2.input.ready.toBoolean)
        val addr = dut.s2.input.cmd.fragment.address.toInt
        val lineId = getLine(addr)
        val opc = dut.s2.input.cmd.fragment.opcode.toInt
        val hitId = dut.s2.input.hitId.toInt
        val replaceId = dut.s2.S2out.wayId.toInt
        val hit = if(hitId == 0) "MISS" else "HIT "
        val len = dut.s2.input.cmd.length.toInt >> 2
        val simtime = simTime()
        opCount += 1
        if (hitId == 0) {
          missCount += 1
        }
        val rd = if(opc == 0) "RD" else "WR"
        //val cond = (lineId == 0x282)
        val cond = true
        if (cond) {
          print(f"$simtime $rd $hit 0x$addr%06x $replaceId%x 0x$lineId%x $len\n")
        }
      }
    }

    val bmbMon = new BmbMonitor(dut.io.output, dut.clockDomain) {
      var prevReadLine = 0l
      override def getByte(address: Long, value: Byte): Unit = {
        val simtime = simTime()
        val line = getLine(address.toInt)
        if (prevReadLine != line) {
          print(f"$simtime* READ 0x$line%x\n")
        }
        prevReadLine = line
      }
      override def setByte(address: Long, value: Byte): Unit = {
        val simtime = simTime()
        print(f"$simtime* WRITE 0x$address%08x 0x$value%x\n")
      }
    }

    //dut.clockDomain.waitSampling(50000000)
    dut.clockDomain.waitSampling(1000000)
    print (s"Operations $opCount misses $missCount\n")
  }
}

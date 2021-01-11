
import spinal.core.sim._
import spinal.lib.bus.bmb.BmbL2Cache._
import spinal.lib.bus.bmb.sim.BmbMemoryAgent
import spinal.lib.bus.bmb.{Bmb, BmbAccessParameter, BmbInvalidationParameter, BmbL2Cache, BmbL2CacheParameter, BmbParameter, BmbSourceParameter}
object BmbL2CacheDirectedTester extends App{
  val p = BmbL2CacheParameter(
    lineLength = 64,
    waySize = 32768,
    wayCount = 8
  )
  val ip = BmbParameter(
    access =  BmbAccessParameter(
      addressWidth = 32,
      dataWidth = 32
    ).addSources(
      count = 1,
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
      addressWidth = 32,
      dataWidth = 32
    ).addSources(
      count = 2,
      p = BmbSourceParameter(
        contextWidth               = 21,
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

  val compiled = SimConfig.withFstWave.compile(BmbL2Cache(p, ip, op))
  compiled.doSim(seed = 42){dut =>
    dut.clockDomain.forkStimulus(10)

    dut.io.input.cmd.valid #= false
    dut.io.input.rsp.ready #= false
    //dut.io.input.inv.valid #= false
    //dut.io.input.ack.ready #= false
    //dut.io.input.sync.valid #= false

    val memory = new BmbMemoryAgent()

    memory.addPort(
      bus = dut.io.output,
      busAddress = 0,
      clockDomain = dut.clockDomain,
      withDriver = true
    )

    //for(i <- 0 until 0x100000) memory.memory.write(i, 0xAA)
    for(i <- 0 until 0x100000) memory.memory.write(i, i)

    dut.clockDomain.waitSampling()

    def inputCmdRead(address : BigInt, length : Int, blocking : Boolean = true): Seq[Byte] ={
      dut.io.input.cmd.valid #= true
      dut.io.input.cmd.opcode #= Bmb.Cmd.Opcode.READ
      dut.io.input.cmd.address #= address
      dut.io.input.cmd.length #= length
      //dut.io.input.cmd.exclusive #= false
      dut.io.input.cmd.fragment.source #= 0
      //dut.clockDomain.waitSampling
      dut.clockDomain.waitSamplingWhere(dut.io.input.cmd.ready.toBoolean)
      dut.io.input.cmd.valid #= false

      dut.io.input.rsp.ready #= true
      if (blocking) {
        do {
          dut.clockDomain.waitSamplingWhere(dut.io.input.rsp.valid.toBoolean)
          println("rd beat")
        } while (!dut.io.input.rsp.last.toBoolean)
      }

      println("last " + simTime())
      List(0x42)
    }

    def inputCmdWrite(address : BigInt, length : Int, data : Seq[Byte], blocking : Boolean = true) : Unit ={
      dut.io.input.cmd.valid #= true
      dut.io.input.cmd.opcode #= Bmb.Cmd.Opcode.WRITE
      dut.io.input.cmd.address #= address
      dut.io.input.cmd.length #= 3
      //dut.io.input.cmd.exclusive #= false
      dut.io.input.cmd.fragment.source #= 0
      dut.io.input.cmd.fragment.data #= 0x12345678
      dut.io.input.cmd.fragment.mask #= 0xF
      dut.clockDomain.waitSamplingWhere(dut.io.input.cmd.ready.toBoolean)
      dut.io.input.cmd.valid #= false

      dut.io.input.rsp.ready #= true
      if (blocking) {
        do {
          dut.clockDomain.waitSamplingWhere(dut.io.input.rsp.valid.toBoolean)
          println("wr beat")
        } while (!dut.io.input.rsp.last.toBoolean)
      }
      println("last " + simTime())
    }

    val inputStimulus = fork{
      dut.clockDomain.waitSampling(600)

      inputCmdRead(0x0, 31, blocking = false)
      //dut.clockDomain.waitSampling(2000)
      inputCmdRead(0x8, 8, blocking = false)
      inputCmdRead(0xC, 4, blocking = false)
      inputCmdRead(0x10, 8, blocking = false)
      inputCmdRead(0x14, 12, blocking = false)
      inputCmdRead(0x18, 16, blocking = false)
      //      inputCmdRead(0x3C, 3, blocking = false)

      /*      inputCmdRead(0x44, 3, blocking = false)
            inputCmdRead(0x104, 3, blocking = false)
            inputCmdRead(0x0, 3, blocking = false)
            inputCmdRead(0x40, 3, blocking = false)
            inputCmdRead(0x184, 3, blocking = false)
            inputCmdRead(0x204, 3, blocking = false)
            inputCmdRead(0x208, 3, blocking = false)
            inputCmdRead(0x20C, 3, blocking = false)
            inputCmdRead(0x0, 3, blocking = false)
            inputCmdRead(0x0, 3, blocking = false)

            inputCmdWrite(0x8, 3, Seq(1,2,3,4), blocking = false)
            inputCmdWrite(0x108, 3, Seq(1,2,3,4), blocking = false)

            inputCmdRead(0x0, 3, blocking = false)
            inputCmdRead(0x8, 3, blocking = false)
      inputCmdWrite(0x8, 3, Seq(1,2,3,4), blocking = false)
      //dut.clockDomain.waitSampling(300)
      inputCmdRead(0x800000, 3, blocking = false)
      inputCmdRead(0x800004, 3, blocking = false)
      inputCmdRead(0x10000000, 3, blocking = false)

      inputCmdRead(0x0, 3, blocking = false)
      inputCmdRead(0x8, 3, blocking = false)
      inputCmdRead(0xC, 3, blocking = false)
      inputCmdRead(0x10, 3, blocking = false)
      inputCmdRead(0x14, 3, blocking = false)
      inputCmdRead(0x18, 3, blocking = false)
      inputCmdRead(0x4000000, 3, blocking = false)
      inputCmdRead(0x4000004, 3, blocking = false)
      inputCmdRead(0x4000008, 3, blocking = false)
      inputCmdRead(0x2000000, 3, blocking = false)
      inputCmdRead(0x2000004, 3, blocking = false)


      inputCmdRead(0x4, 3, blocking = false)
      inputCmdRead(0x14, 3, blocking = false)
      inputCmdRead(0xC, 3, blocking = false)
      inputCmdRead(0x10, 3, blocking = false)
      inputCmdRead(0x40, 3, blocking = false)
      inputCmdRead(0x1C, 3, blocking = false)
      inputCmdRead(0x18, 3, blocking = false)

      inputCmdRead(0x44, 3, blocking = false)
      inputCmdRead(0x48, 3, blocking = false)
      inputCmdRead(0x4C, 3, blocking = false)
      inputCmdRead(0x50, 3, blocking = false)
      inputCmdRead(0x54, 3, blocking = false)
      inputCmdRead(0x58, 3, blocking = false)
      inputCmdRead(0x5C, 3, blocking = false)

      inputCmdRead(0x10001000, 3)
      inputCmdRead(0x10001004, 3)
      inputCmdRead(0x102000, 3)
      inputCmdRead(0x1003000, 3)
*/
      dut.clockDomain.waitSampling(1000)
    }

    inputStimulus.join()
  }
}

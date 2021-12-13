package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

object Axi4SharedOnChipRam{
  def getAxiConfig(dataWidth : Int,byteCount : BigInt,idWidth : Int) = Axi4Config(
    addressWidth = log2Up(byteCount),
    dataWidth = dataWidth,
    idWidth = idWidth,
    useLock = false,
    useRegion = false,
    useCache = false,
    useProt = false,
    useQos = false
  )

  def main(args: Array[String]) {
    SpinalVhdl(new Axi4SharedOnChipRam(32,1024,4).setDefinitionName("TopLevel"))
  }
}

case class Axi4SharedOnChipRam(dataWidth : Int, byteCount : BigInt, idWidth : Int, arwStage : Boolean = false) extends Component{
  val axiConfig = Axi4SharedOnChipRam.getAxiConfig(dataWidth,byteCount,idWidth)

  val io = new Bundle {
    val axi = slave(Axi4Shared(axiConfig))
  }

  val wordCount = byteCount / axiConfig.bytePerWord
  val ram = Mem(axiConfig.dataType,wordCount.toInt)
  val wordRange = log2Up(wordCount) + log2Up(axiConfig.bytePerWord)-1 downto log2Up(axiConfig.bytePerWord)

  val arw = if(arwStage) io.axi.arw.s2mPipe().unburstify.m2sPipe() else io.axi.arw.unburstify
  val stage0 = arw.haltWhen(arw.write && !io.axi.writeData.valid)
  io.axi.readRsp.data := ram.readWriteSync(
    address = stage0.addr(axiConfig.wordRange).resized,
    data = io.axi.writeData.data,
    enable = stage0.fire,
    write = stage0.write,
    mask = io.axi.writeData.strb
  )
  io.axi.writeData.ready :=  arw.valid && arw.write  && stage0.ready

  val stage1 = stage0.stage
  stage1.ready := (io.axi.readRsp.ready && !stage1.write) || ((io.axi.writeRsp.ready || ! stage1.last) && stage1.write)

  io.axi.readRsp.valid  := stage1.valid && !stage1.write
  io.axi.readRsp.id  := stage1.id
  io.axi.readRsp.last := stage1.last
  io.axi.readRsp.setOKAY()
  if(axiConfig.useRUser) io.axi.readRsp.user  := stage1.user

  io.axi.writeRsp.valid := stage1.valid &&  stage1.write && stage1.last
  io.axi.writeRsp.setOKAY()
  io.axi.writeRsp.id := stage1.id
  if(axiConfig.useBUser) io.axi.writeRsp.user := stage1.user

  io.axi.arw.ready.noBackendCombMerge //Verilator perf
}


case class Axi4SharedOnChipRamPort(config: Axi4Config) extends Bundle {
    def attach(ram: Mem[Bits]): Axi4Shared = {
        val wordRange       = config.wordRange
        val axi             = Axi4Shared(config)
        val arw             = axi.arw.unburstify
        val writeDataStream = axi.writeData

        val readStream      = Stream(Axi4R(axi.config))
        val readAddrStream  = cloneOf(arw)
        val writeStream     = Stream(Axi4B(axi.config))
        val writeAddrStream = cloneOf(arw)

        Vec(readAddrStream, writeAddrStream) <> StreamDemux(
            arw,
            U(arw.write),
            2
        )

        val writeCombined = StreamJoin(writeAddrStream, writeDataStream)
        val writeAddr     = writeCombined._1.addr(wordRange)
        ram.write(
            address = writeAddr.resized,
            data = writeCombined._2.data,
            enable = writeCombined.fire,
            mask = writeCombined._2.strb
        )

        writeStream.translateFrom(writeCombined) { (to, from) =>
            if (axi.config.useId) to.id := from._1.id
            if (axi.config.useWUser) to.user := from._1.user
            to.setOKAY()
        }
        val writeReady = RegInit(False).riseWhen(True)
        writeStream.ready := writeReady

        val bStream      = cloneOf(writeStream)
        val writeLast    = writeCombined.fire && writeCombined._1.last
        val writePayload = RegNextWhen(writeStream.payload, writeLast)
        val writeDone    = RegInit(False)
        when(writeLast) {
            writeDone := True
        } elsewhen (bStream.fire) {
            writeDone := False
        }
        bStream.valid := writeDone
        bStream.payload := writePayload
        axi.writeRsp << bStream

        val readAddr = readAddrStream.addr(wordRange)
        val readData = ram
            .readSync(
                address = readAddr.resized
            )
            .resized
        val savedReadData = Reg(cloneOf(readData))
        val dataValid     = RegNext(readAddrStream.fire).init(False)
        when(dataValid) {
            savedReadData := readData
        }
        val outData = cloneOf(readData)
        when(dataValid) {
            outData := readData
        } otherwise {
            outData := savedReadData
        }

        readStream.translateFrom(readAddrStream) { (to, from) =>
            if (axi.config.useId) to.id := from.id
            to.last := from.last
            to.data := 0
            to.setOKAY()
            if (axi.config.useRUser) to.user := from.user
        }
        val readOutStream = cloneOf(readStream)
        readOutStream.translateFrom(readStream.stage) { (to, from) =>
            to := from
            to.data.removeAssignments()
            to.data := outData
        }
        axi.readRsp << readOutStream

        axi.arw.ready.noBackendCombMerge //Verilator perf
        axi
    }

    def withSoftResetOf(ram: Mem[Bits], softReset: Bool): Axi4Shared = {
        val clock = ClockDomain.current.copy(softReset = softReset)
        val area = new ClockingArea(clock) {

            val axi = attach(ram)
        }
        area.axi
    }
}

object Axi4SharedOnChipRamMultiPort {
    def apply(portCount: Int, dataWidth: Int, byteCount: BigInt, idWidth: Int) = {
        val axiConfig = Axi4SharedOnChipRam.getAxiConfig(dataWidth, byteCount, idWidth)
        val wordCount = byteCount / axiConfig.bytePerWord
        new Axi4SharedOnChipRamMultiPort(axiConfig, wordCount, portCount)
    }
    def apply(config: Axi4Config, wordCount: BigInt, portCount: Int) =
        new Axi4SharedOnChipRamMultiPort(config, wordCount, portCount)
}

class Axi4SharedOnChipRamMultiPort(config: Axi4Config, wordCount: BigInt, portCount: Int) extends Component {
    val io = new Bundle {
        val axis = Vec(slave(Axi4Shared(config)), portCount)
    }

    if (config.useLock || config.useRegion || config.useCache || config.useProt || config.useQos)
        SpinalWarning("Axi4SharedOnChipRamMultiPort might not support Axi4 Lock, Region, Cahce, Prot and Qos featrues!")

    val ram   = Mem(config.dataType, wordCount.toInt)
    val ports = Vec(Axi4SharedOnChipRamPort(config), portCount)
    for (i <- 0 until portCount) {
        io.axis(i) >> ports(i).attach(ram)
    }
}


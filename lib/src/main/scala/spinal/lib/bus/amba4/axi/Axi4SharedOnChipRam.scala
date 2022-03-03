package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.core.fiber._
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

  val stage1 = stage0.stage()
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


object Axi4SharedOnChipRamPort {
    def apply(config: Axi4Config, ram: Mem[Bits]) = {
        val port = new Axi4SharedOnChipRamPort(config)
        port.attach(ram)
        port
    }

    def apply(config: Axi4Config, ram: Mem[Bits], softReset: Bool) = {
        val clock = ClockDomain.current.copy(softReset = softReset)
        val domain = new ClockingArea(clock) {
            val port = new Axi4SharedOnChipRamPort(config)
            port.attach(ram)
        }
        domain.port
    }
}

case class Axi4SharedOnChipRamPort(config: Axi4Config) extends ImplicitArea[Axi4Shared] {
    val axi                                = Axi4Shared(config)
    override def implicitValue: Axi4Shared = this.axi

    val ram = Handle[Mem[Bits]]

    val logic = Handle {
        new Area {
            val wordRange       = config.wordRange
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
            val address       = arw.addr(wordRange).resize(ram.addressWidth bits)
            ram.write(
                address = address,
                data = writeCombined._2.data,
                enable = writeCombined.fire,
                mask = writeCombined._2.strb
            )

            writeStream.translateFrom(writeCombined) { (to, from) =>
                if (axi.config.useId) to.id := from._1.id
                if (axi.config.useWUser) to.user := from._1.user
                to.setOKAY()
            }
            val writeLast = writeCombined.valid && writeCombined._1.last
            val bStream   = writeStream.throwWhen(!writeLast).queue(1)
            axi.writeRsp << bStream

            val readData = ram
                .readSync(
                    address = address
                )
                .resized
            val dataValid     = RegNext(readAddrStream.fire).init(False)
            val savedReadData = RegNextWhen(readData, dataValid)
            val outData       = CombInit(savedReadData)
            when(dataValid) {
                outData := readData
            }

            readStream.translateFrom(readAddrStream) { (to, from) =>
                if (axi.config.useId) to.id := from.id
                if (axi.config.useLast) to.last := from.last
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
        }
    }

    def attach(ram: Mem[Bits]) = {
        this.ram.load(ram)
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
    val ports = Array.fill(portCount)(Axi4SharedOnChipRamPort(config, ram))
    (io.axis, ports).zipped foreach ((m, s) => m >> s)
}


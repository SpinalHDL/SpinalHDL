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


//WARNING, do not support backpresure
case class Axi4SharedOnChipRamMultiPort(portCount : Int, dataWidth : Int,byteCount : BigInt,idWidth : Int) extends Component{
  val axiConfig = Axi4SharedOnChipRam.getAxiConfig(dataWidth,byteCount,idWidth)

  val io = new Bundle {
    val axis = Vec(slave(Axi4Shared(axiConfig)), portCount)
  }

  val wordCount = byteCount / axiConfig.bytePerWord
  val ram = Mem(axiConfig.dataType,wordCount.toInt)
  val wordRange = log2Up(wordCount) + log2Up(axiConfig.bytePerWord)-1 downto log2Up(axiConfig.bytePerWord)

  
  for(axi <- io.axis) {
    val arw = axi.arw.unburstify
    val stage0 = arw.haltWhen(arw.write && !axi.writeData.valid)
//    axi.readRsp.data := ram.readWriteSync(
//      address = stage0.addr(axiConfig.wordRange).resized,
//      data = axi.writeData.data,
//      enable = stage0.fire,
//      write = stage0.write,
//      mask = axi.writeData.strb
//    )
    val addr = stage0.addr(axiConfig.wordRange)
//    val addrLast = RegNext(addr)

    ram.write(
      address = addr,
      data = axi.writeData.data,
      enable = stage0.fire && stage0.write,
      mask = axi.writeData.strb
    )
    axi.readRsp.data := ram.readSync(
      address = addr
    )

    axi.writeData.ready := arw.valid && arw.write && stage0.ready

    val stage1 = stage0.stage
    stage1.ready := (axi.readRsp.ready && !stage1.write) || ((axi.writeRsp.ready || !stage1.last) && stage1.write)

    axi.readRsp.valid := stage1.valid && !stage1.write
    axi.readRsp.id := stage1.id
    axi.readRsp.last := stage1.last
    axi.readRsp.setOKAY()
    if (axiConfig.useRUser) axi.readRsp.user := stage1.user

    axi.writeRsp.valid := stage1.valid && stage1.write && stage1.last
    axi.writeRsp.setOKAY()
    axi.writeRsp.id := stage1.id
    if (axiConfig.useBUser) axi.writeRsp.user := stage1.user

    axi.arw.ready.noBackendCombMerge //Verilator perf
  }
}



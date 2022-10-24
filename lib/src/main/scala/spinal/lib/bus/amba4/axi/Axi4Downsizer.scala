package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

object Axi4DownsizerSubTransactionGenerator {
    def apply[T <: Axi4Ax](
        input: Stream[T],
        output: Stream[T],
        inputConfig: Axi4Config,
        outputConfig: Axi4Config
    ): Axi4DownsizerSubTransactionGenerator[T] = {
        val gen = new Axi4DownsizerSubTransactionGenerator(input.payloadType, inputConfig, outputConfig)
        gen.io.input << input
        gen.io.output >> output
        gen
    }
}

class Axi4DownsizerSubTransactionGenerator[T <: Axi4Ax](
    dataType: HardType[T],
    inputConfig: Axi4Config,
    outputConfig: Axi4Config
) extends Component {
    val sizeWidth  = 3
    val ratioWidth = (1 << sizeWidth) - 1 //size width is 3
    val io = new Bundle {
        val input   = slave(Stream(dataType))
        val output  = master(Stream(dataType))
        val start   = out UInt (inputConfig.addressWidth bits)
        val ratio   = out UInt (ratioWidth bits)
        val size    = out UInt (sizeWidth bits)
        val working = out Bool ()
        val last    = out Bool ()
        val done    = out Bool ()
    }
    val sizeMaxOut = log2Up(outputConfig.bytePerWord)
    val sizeMaxIn  = log2Up(inputConfig.bytePerWord)

    val startAddress = cloneOf(io.input.addr)
    val sizeIn       = if (inputConfig.useSize) io.input.size else U(sizeMaxIn, sizeWidth bits)
    val sizeDiff     = sizeIn - sizeMaxOut
    val sizePerTrans = UInt(sizeWidth bits)
    val ratio        = cloneOf(io.ratio)
    when(sizeIn > sizeMaxOut) {
        startAddress := (io.input.addr |>> sizeIn) |<< sizeIn
        ratio := (U(1, ratioWidth bits) |<< sizeDiff) - 1
        sizePerTrans := sizeMaxOut
    } otherwise {
        startAddress := io.input.addr
        ratio := 0
        sizePerTrans := sizeIn
    }

    val cmdStreamWithSize = cloneOf(io.input)
    cmdStreamWithSize.translateFrom(io.input) { (to, from) =>
        to.addr := startAddress
        if (inputConfig.useSize) to.size := sizePerTrans
        to.assignUnassignedByName(from)
    }
    val size = RegNextWhen(sizePerTrans, io.input.fire) init sizeMaxOut

    val cmdExtendedStream = cloneOf(io.input)
    val cmdExtender       = StreamTransactionExtender(cmdStreamWithSize, cmdExtendedStream, ratio) { (id, p, _) => p }
    val address           = RegInit(U(0, io.input.addr.getWidth bits))
    when(cmdStreamWithSize.fire) {
        address := cmdStreamWithSize.addr
    } elsewhen (cmdExtendedStream.fire) {
        address := address + ((cmdExtendedStream.len +^ 1) << size).resized
    }

    val cmdStream = cloneOf(io.input)
    cmdStream.translateFrom(cmdExtendedStream) { (to, from) =>
        to.addr := address
        to.assignUnassignedByName(from)
    }

    val dataRatio = RegNextWhen(ratio, io.input.fire) init 0
    when(io.input.fire) {
        io.ratio := ratio
        io.size := sizePerTrans
    } otherwise {
        io.ratio := dataRatio
        io.size := size
    }

    io.working := cmdExtender.io.working
    io.last := cmdExtender.io.last
    io.done := cmdExtender.io.done
    io.start := startAddress
    io.output << cmdStream
}

//Curently only INCR burst compatible
case class Axi4WriteOnlyDownsizer(inputConfig: Axi4Config, outputConfig: Axi4Config, rspDepth: Int = 2)
    extends Component {
    val io = new Bundle {
        val input  = slave(Axi4WriteOnly(inputConfig))
        val output = master(Axi4WriteOnly(outputConfig))
    }
    val sizeMaxOut = log2Up(outputConfig.bytePerWord)
    val sizeMaxIn  = log2Up(inputConfig.bytePerWord)

    val writeStream = cloneOf(io.output)
    val dataWorking = Bool()
    val writeCmd    = io.input.writeCmd.haltWhen(dataWorking)
    val cmdStream   = cloneOf(writeCmd)
    val generator   = Axi4DownsizerSubTransactionGenerator(writeCmd, cmdStream, inputConfig, outputConfig)

    val staleInputData   = Bool()
    val writeData        = io.input.writeData.haltWhen(staleInputData)
    val inputDataCounter = StreamTransactionCounter(writeCmd, writeData, writeCmd.len, true)
    staleInputData := !inputDataCounter.io.working

    val (rspCountStream, countCmdStream, outCmdStream) = StreamFork3(cmdStream)
    writeStream.writeCmd << outCmdStream

    val dataStream    = cloneOf(writeData)
    val streamCounter = StreamTransactionCounter(countCmdStream, dataStream, countCmdStream.len, true)
    countCmdStream.ready := streamCounter.io.available

    val beatOffsetReg = RegNextWhen(countCmdStream.addr(inputConfig.symbolRange), countCmdStream.fire)
    val beatOffset    = CombInit(beatOffsetReg)
    when(countCmdStream.fire) {
        beatOffset := countCmdStream.addr(inputConfig.symbolRange)
    }
    when(dataStream.fire) {
        beatOffsetReg := (beatOffset + (1 << generator.io.size)).resized
    }

    val offset = beatOffset & ~U((1 << sizeMaxOut) - 1, sizeMaxIn bits)
    val dataExtender = StreamTransactionExtender(writeData, dataStream, generator.io.ratio) { (id, p, last) =>
        val out = cloneOf(p)
        if (outputConfig.useLast) out.last := last && p.last
        out.assignUnassignedByName(p)
        out
    }
    dataWorking := !inputDataCounter.io.available || !writeData.ready

    val staleData = !streamCounter.io.working
    writeStream.writeData.translateFrom(dataStream.haltWhen(staleData)) { (to, from) =>
        to.data := from.data(offset << 3, outputConfig.dataWidth bits)
        if (outputConfig.useLast) to.last := streamCounter.io.last
        if (outputConfig.useStrb) to.strb := from.strb(offset, outputConfig.bytePerWord bits)
        to.assignUnassignedByName(from)
    }

    val rspCtrlStream = Stream(Bool())
    rspCtrlStream.translateFrom(rspCountStream) { (to, from) =>
        to := generator.io.last
    }
    val rspStream = StreamJoin(rspCtrlStream.queue(rspDepth), writeStream.writeRsp)
    io.input.writeRsp.translateFrom(rspStream.throwWhen(!rspStream._1)) { (to, from) =>
        to := from._2
    }

    io.output << writeStream
}

//Currently only INCR compatible
case class Axi4ReadOnlyDownsizer(inputConfig: Axi4Config, outputConfig: Axi4Config) extends Component {
    val io = new Bundle {
        val input  = slave(Axi4ReadOnly(inputConfig))
        val output = master(Axi4ReadOnly(outputConfig))
    }
    val sizeMaxOut = log2Up(outputConfig.bytePerWord)
    val sizeMaxIn  = log2Up(inputConfig.bytePerWord)

    val readCmd                    = io.input.readCmd
    val (readCmdGen, readCmdCount) = StreamFork2(readCmd, true)

    val cmdStream = cloneOf(readCmd)
    val generator = Axi4DownsizerSubTransactionGenerator(readCmdGen, cmdStream, inputConfig, outputConfig)
    io.output.readCmd << cmdStream

    val cmdState = new Bundle {
        val ratio = cloneOf(generator.io.ratio)
        val size  = cloneOf(generator.io.size)
        val len   = cloneOf(readCmdCount.len)
        val start = cloneOf(generator.io.start)
    }

    val countCmdStream = Stream(cmdState)
    countCmdStream.translateFrom(readCmdCount) { (to, from) =>
        to.ratio := generator.io.ratio
        to.size := generator.io.size
        to.len := readCmdCount.len
        to.start := generator.io.start
    }
    val countOutStream = Stream(cmdState)
    val dataOutCounter = StreamTransactionExtender(countCmdStream, countOutStream, countCmdStream.len) { (_, p, last) =>
        p
    }

    val dataIn      = io.output.readRsp
    val countStream = Stream(cmdState)
    val dataCounter = StreamTransactionExtender(countOutStream, countStream, countOutStream.ratio) { (_, p, _) =>
        p
    }
    countStream.ready := dataIn.fire

    val dataReg    = Reg(Bits(inputConfig.dataWidth bits))
    val beatOffset = RegInit(U(0, sizeMaxIn bits))
    when(countOutStream.fire && dataOutCounter.io.first) {
        beatOffset := countOutStream.start(inputConfig.symbolRange)
    } elsewhen (dataIn.fire) {
        beatOffset := (beatOffset + (1 << countStream.size)).resized
    }

    val offset = beatOffset & ~U((1 << sizeMaxOut) - 1, sizeMaxIn bits)
    when(dataIn.fire) {
        dataReg(offset << 3, outputConfig.dataWidth bits) := dataIn.data
    }
    val dataOut = cloneOf(io.input.readRsp)
    val data    = Bits(inputConfig.dataWidth bits)
    data := dataReg
    when(dataCounter.io.last && dataOut.valid) {
        data(offset << 3, outputConfig.dataWidth bits) := dataIn.data
    }

    val lastLast = RegInit(False)
    when(dataOutCounter.io.working && dataOutCounter.io.last && countOutStream.fire) {
        lastLast := True
    } elsewhen (dataCounter.io.last && dataIn.fire) {
        lastLast := False
    }
    dataOut.translateFrom(dataIn.throwWhen(!dataCounter.io.last)) { (to, from) =>
        to.data := data
        if (inputConfig.useLast) to.last := from.last && lastLast
        to.assignUnassignedByName(from)
    }
    io.input.readRsp << dataOut
}

case class Axi4Downsizer(inputConfig: Axi4Config, outputConfig: Axi4Config) extends Component {
    val io = new Bundle {
        val input  = slave(Axi4(inputConfig))
        val output = master(Axi4(outputConfig))
    }

    val readOnly  = Axi4ReadOnlyDownsizer(inputConfig, outputConfig)
    val writeOnly = Axi4WriteOnlyDownsizer(inputConfig, outputConfig)

    readOnly.io.input.ar <> io.input.ar
    readOnly.io.input.r <> io.input.r
    writeOnly.io.input.aw <> io.input.aw
    writeOnly.io.input.w <> io.input.w
    writeOnly.io.input.b <> io.input.b

    readOnly.io.output.ar <> io.output.ar
    readOnly.io.output.r <> io.output.r
    writeOnly.io.output.aw <> io.output.aw
    writeOnly.io.output.w <> io.output.w
    writeOnly.io.output.b <> io.output.b
}

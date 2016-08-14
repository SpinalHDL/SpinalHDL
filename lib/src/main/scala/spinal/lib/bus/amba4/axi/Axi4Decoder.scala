package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc.SizeMapping

case class Axi4ReadOnlyDecoder(axiConfig: Axi4Config,decodings : Iterable[SizeMapping],pendingMax : Int = 7) extends Component{
  val io = new Bundle{
    val input = slave(Axi4ReadOnly(axiConfig))
    val outputs = Vec(master(Axi4ReadOnly(axiConfig)),decodings.size)
  }

  val pendingCounter = CounterUpDown(
    stateCount = pendingMax+1,
    incWhen = io.input.readCmd.fire,
    decWhen = io.input.readRsp.fire && io.input.readRsp.last
  )

  val decodedSels,appliedSels = Bits(decodings.size bits)
  val lastCmdSel = RegNextWhen(appliedSels,io.input.readCmd.ready)  init(0)
  val allowCmd = pendingCounter =/= pendingMax && (pendingCounter === 0  || (lastCmdSel === decodedSels))
  decodedSels  := decodings.map(_.hit(io.input.readCmd.addr) && io.input.readCmd.valid).asBits
  appliedSels := allowCmd ? decodedSels | 0

  //Wire readCmd
  io.input.readCmd.ready := (appliedSels & io.outputs.map(_.readCmd.ready).asBits).orR
  for((output,sel) <- (io.outputs,appliedSels.asBools).zipped){
    output.readCmd.valid := sel
    output.readCmd.payload := io.input.readCmd.payload
  }

  //Wire ReadRsp
  val readRspIndex = OHToUInt(lastCmdSel)
  io.input.readRsp.valid := io.outputs.map(_.readRsp.valid).asBits.orR
  io.input.readRsp.payload := io.outputs(readRspIndex).readRsp.payload
  io.outputs.foreach(_.readRsp.ready := io.input.readRsp.ready)

  //Decoding error managment
  val decodingErrorPossible = decodings.map(_.size).sum < (BigInt(1) << axiConfig.addressWidth)
  val decodingError = if(decodingErrorPossible) new Area{
    val detected = decodedSels === 0 && io.input.readCmd.valid
    val sendRsp = RegInit(False)
    val id = Reg(UInt(axiConfig.idWidth bits))
    val remaining = Reg(UInt(8 bits))
    val remainingZero = remaining === 0

    //Wait until all pending commands are done
    when(detected && pendingCounter === 0){
      io.input.readCmd.ready := True
      id := io.input.readCmd.id
      remaining := io.input.readCmd.len
      sendRsp := True
    }

    //Send a DECERR readRsp
    when(sendRsp) {
      io.input.readRsp.valid := True
      io.input.readRsp.setDECERR
      io.input.readRsp.id := id
      io.input.readRsp.last := remainingZero
      when(io.input.readRsp.ready) {
        remaining := remaining - 1
        when(remainingZero) {
          sendRsp := False
        }
      }
    }
  }
}


case class Axi4WriteDecoder(axiConfig: Axi4Config,decodings : Iterable[SizeMapping],pendingMax : Int = 7) extends Component{
  val io = new Bundle{
    val input = slave(Axi4WriteOnly(axiConfig))
    val outputs = Vec(master(Axi4WriteOnly(axiConfig)),decodings.size)
  }

  val pendingCmdCounter = CounterUpDown(
    stateCount = pendingMax+1,
    incWhen = io.input.writeCmd.fire,
    decWhen = io.input.writeRsp.fire
  )

  val pendingDataCounter = CounterUpDown(
    stateCount = pendingMax+1,
    incWhen = io.input.writeCmd.fire,
    decWhen = io.input.writeData.fire && io.input.writeData.last
  )

  val decodedCmdSels,appliedCmdSels = Bits(decodings.size bits)
  val lastCmdSel  = RegNextWhen(appliedCmdSels,io.input.writeCmd.ready)  init(0)
  val allowCmd    = pendingCmdCounter =/= pendingMax && (pendingCmdCounter === 0  || (lastCmdSel === decodedCmdSels))
  val allowData   = pendingDataCounter =/= 0
  decodedCmdSels := decodings.map(_.hit(io.input.writeCmd.addr) && io.input.writeCmd.valid).asBits
  appliedCmdSels := allowCmd ? decodedCmdSels | 0
  
  //Wire writeCmd
  io.input.writeCmd.ready := (appliedCmdSels & io.outputs.map(_.writeCmd.ready).asBits).orR
  for((output,sel) <- (io.outputs,appliedCmdSels.asBools).zipped){
    output.writeCmd.valid := sel
    output.writeCmd.payload := io.input.writeCmd.payload
  }

  //Wire writeData
  io.input.writeData.ready := (lastCmdSel & io.outputs.map(_.writeData.ready).asBits).orR && allowData
  for((output,linkEnable) <- (io.outputs,lastCmdSel.asBools).zipped){
    output.writeData.valid   := io.input.writeData.valid && allowData && linkEnable
    output.writeData.payload := io.input.writeData.payload
  }

  //Wire writeRsp
  val writeRspIndex = OHToUInt(lastCmdSel)
  io.input.writeRsp.valid := io.outputs.map(_.writeRsp.valid).asBits.orR
  io.input.writeRsp.payload := io.outputs(writeRspIndex).writeRsp.payload
  io.outputs.foreach(_.writeRsp.ready := io.input.writeRsp.ready)
  
  
  //Decoding error managment
  val decodingErrorPossible = decodings.map(_.size).sum < (BigInt(1) << axiConfig.addressWidth)
  val decodingError = if(decodingErrorPossible) new Area{
    val detected = decodedCmdSels === 0 && io.input.writeCmd.valid
    val waitDataLast = RegInit(False)
    val sendRsp = RegInit(False)
    val id = Reg(UInt(axiConfig.idWidth bits))
  
    //Wait until all pending commands are done
    when(detected && pendingCmdCounter === 0){
      io.input.writeCmd.ready := True
      id := io.input.writeCmd.id
      waitDataLast := True
    }
  
    //Consume all writeData transaction
    when(waitDataLast){
      io.input.writeData.ready := True
      when(io.input.writeData.valid && io.input.writeData.last){
        waitDataLast := False
        sendRsp := True
      }
    }
  
    //Send a DECERR writeRsp
    when(sendRsp) {
      io.input.writeRsp.valid := True
      io.input.writeRsp.setDECERR
      io.input.writeRsp.id := id
      when(io.input.writeRsp.ready) {
        sendRsp := False
      }
    }
  }
}

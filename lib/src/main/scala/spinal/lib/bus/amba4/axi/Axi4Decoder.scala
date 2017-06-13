package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc.SizeMapping

case class Axi4ReadOnlyDecoder(axiConfig: Axi4Config,decodings : Seq[SizeMapping],pendingMax : Int = 7) extends Component{
  val io = new Bundle{
    val input = slave(Axi4ReadOnly(axiConfig))
    val outputs = Vec(master(Axi4ReadOnly(axiConfig)),decodings.size)
  }

  val pendingCmdCounter = CounterUpDown(
    stateCount = pendingMax+1,
    incWhen = io.input.readCmd.fire,
    decWhen = io.input.readRsp.fire && io.input.readRsp.last
  )
  val decodedCmdSels = decodings.map(_.hit(io.input.readCmd.addr) && io.input.readCmd.valid).asBits
  val pendingSels  = RegNextWhen(decodedCmdSels,io.input.readCmd.ready)  init(0)
  val allowCmd    = pendingCmdCounter === 0 || (pendingCmdCounter =/= pendingMax && pendingSels === decodedCmdSels)

  
  //Wire readCmd
  io.input.readCmd.ready := (decodedCmdSels & io.outputs.map(_.readCmd.ready).asBits).orR  && allowCmd
  for((output,sel) <- (io.outputs,decodedCmdSels.asBools).zipped){
    output.readCmd.valid := io.input.readCmd.valid && sel && allowCmd
    output.readCmd.payload := io.input.readCmd.payload
  }

  //Wire ReadRsp
  val readRspIndex = OHToUInt(pendingSels)
  io.input.readRsp.valid := io.outputs.map(_.readRsp.valid).asBits.orR
  io.input.readRsp.payload := io.outputs(readRspIndex).readRsp.payload
  io.outputs.foreach(_.readRsp.ready := io.input.readRsp.ready)

  //Decoding error managment
  val decodingErrorPossible = decodings.map(_.size).sum < (BigInt(1) << axiConfig.addressWidth)
  val decodingError = if(decodingErrorPossible) new Area{
    val detected = io.input.readCmd.valid && decodedCmdSels === 0 && allowCmd
    val sendRsp = RegInit(False)
    val id = if(axiConfig.useId) Reg(UInt(axiConfig.idWidth bits)) else null
    val remaining = Reg(UInt(8 bits))
    val remainingZero = remaining === 0

    //Wait until all pending commands are done
    when(detected){
      io.input.readCmd.ready := True
      if(id != null)id := io.input.readCmd.id
      remaining := (if(axiConfig.useLen) io.input.readCmd.len else U(0))
      sendRsp := True
    }

    //Send a DECERR readRsp
    when(sendRsp) {
      io.input.readRsp.valid := True
      if(axiConfig.useResp) io.input.readRsp.setDECERR
      if(id != null) io.input.readRsp.id := id
      if(io.input.readRsp.last != null) io.input.readRsp.last := remainingZero
      when(io.input.readRsp.ready) {
        remaining := remaining - 1
        when(remainingZero) {
          sendRsp := False
        }
      }
      allowCmd := False
    }
  }
}


case class Axi4WriteOnlyDecoder(axiConfig: Axi4Config,decodings : Seq[SizeMapping],pendingMax : Int = 7) extends Component{
  val io = new Bundle{
    val input = slave(Axi4WriteOnly(axiConfig))
    val outputs = Vec(master(Axi4WriteOnly(axiConfig)),decodings.size)
  }

  val cmdAllowedStart = Bool

  val pendingCmdCounter = CounterUpDown(
    stateCount = pendingMax+1,
    incWhen = io.input.writeCmd.fire,
    decWhen = io.input.writeRsp.fire
  )

  val pendingDataCounter = CounterUpDown(
    stateCount = pendingMax+1,
    incWhen = cmdAllowedStart,
    decWhen = io.input.writeData.fire && io.input.writeData.last
  )

  val decodedCmdSels = decodings.map(_.hit(io.input.writeCmd.addr) && io.input.writeCmd.valid).asBits
  val pendingSels  = RegNextWhen(decodedCmdSels,cmdAllowedStart)  init(0)
  val allowCmd    = pendingCmdCounter === 0 || (pendingCmdCounter =/= pendingMax && pendingSels === decodedCmdSels)
  val allowData   = pendingDataCounter =/= 0

  cmdAllowedStart := io.input.writeCmd.valid && allowCmd && (RegInit(True) clearWhen(cmdAllowedStart) setWhen(io.input.writeCmd.ready))

  //Wire writeCmd
  io.input.writeCmd.ready := (decodedCmdSels & io.outputs.map(_.writeCmd.ready).asBits).orR && allowCmd
  for((output,sel) <- (io.outputs,decodedCmdSels.asBools).zipped){
    output.writeCmd.valid := io.input.writeCmd.valid && sel && allowCmd
    output.writeCmd.payload := io.input.writeCmd.payload
  }

  //Wire writeData
  io.input.writeData.ready := (pendingSels & io.outputs.map(_.writeData.ready).asBits).orR && allowData
  for((output,sel) <- (io.outputs,pendingSels.asBools).zipped){
    output.writeData.valid   := io.input.writeData.valid && sel && allowData
    output.writeData.payload := io.input.writeData.payload
  }

  //Wire writeRsp
  val writeRspIndex = OHToUInt(pendingSels)
  io.input.writeRsp.valid := io.outputs.map(_.writeRsp.valid).asBits.orR
  io.input.writeRsp.payload := io.outputs(writeRspIndex).writeRsp.payload
  io.outputs.foreach(_.writeRsp.ready := io.input.writeRsp.ready)


  //Decoding error managment
  val decodingErrorPossible = decodings.map(_.size).sum < (BigInt(1) << axiConfig.addressWidth)
  val decodingError = if(decodingErrorPossible) new Area{
    val detected = io.input.writeCmd.valid && decodedCmdSels === 0 && allowCmd
    val sendRsp = RegInit(False)
    val id = if(axiConfig.useId) Reg(UInt(axiConfig.idWidth bits)) else null

    //Stall CMD
    when(detected){
      io.input.writeCmd.ready := False
    }

    //Flush all DATA
    when((RegNext(detected) init(False)) && !sendRsp){  // !sendRsp for consecutive access errors
      when(pendingDataCounter =/= 0) {
        io.input.writeData.ready := True
      } otherwise {
        io.input.writeData.ready := False
        io.input.writeCmd.ready := True
        if(id != null) id := io.input.writeCmd.id
        sendRsp := True;
      }
    }

    //Send a DECERR
    when(sendRsp) {
      allowCmd := False  //Allow consecutive decoding error
      io.input.writeRsp.valid := True
      if(axiConfig.useResp) io.input.writeRsp.setDECERR
      if(id != null) io.input.writeRsp.id := id
      when(io.input.writeRsp.ready) {
        sendRsp := False
      }
    }
  }
}





case class Axi4SharedDecoder(axiConfig: Axi4Config,
                             readDecodings : Seq[SizeMapping],
                             writeDecodings : Seq[SizeMapping],
                             sharedDecodings : Seq[SizeMapping],
                             pendingMax : Int = 7) extends Component{
  val io = new Bundle{
    val input = slave(Axi4Shared(axiConfig))
    val readOutputs   = Vec(master(Axi4ReadOnly(axiConfig)),readDecodings.size)
    val writeOutputs  = Vec(master(Axi4WriteOnly(axiConfig)),writeDecodings.size)
    val sharedOutputs = Vec(master(Axi4Shared(axiConfig)),sharedDecodings.size)
  }

  val cmdAllowedStart = Bool

  val pendingCmdCounter = CounterMultiRequest(
    log2Up(pendingMax+1),
    (io.input.sharedCmd.fire -> (_ + 1)),
    (io.input.writeRsp.fire -> (_ - 1)),
    ((io.input.readRsp.fire && io.input.readRsp.last) -> (_ - 1))
  )

  val pendingDataCounter = CounterUpDown(
    stateCount = pendingMax+1,
    incWhen = cmdAllowedStart && io.input.sharedCmd.write,
    decWhen = io.input.writeData.fire && io.input.writeData.last
  )

  val decodings = readDecodings ++ writeDecodings ++ sharedDecodings
  val readRange   = 0 to readDecodings.size -1
  val writeRange  = readRange.high + 1 to readRange.high + writeDecodings.size
  val sharedRange = writeRange.high + 1 to writeRange.high + sharedDecodings.size

  val decodedCmdSels = Cat(
    sharedDecodings.map(_.hit(io.input.sharedCmd.addr)).asBits,
    writeDecodings.map(_.hit(io.input.sharedCmd.addr) &&  io.input.sharedCmd.write).asBits,
    readDecodings.map(_.hit(io.input.sharedCmd.addr)  && !io.input.sharedCmd.write).asBits
  )
  val pendingSels  = RegNextWhen(decodedCmdSels,cmdAllowedStart)  init(0)
  val allowCmd    = pendingCmdCounter === 0 || (pendingCmdCounter =/= pendingMax && pendingSels === decodedCmdSels)
  val allowData   = pendingDataCounter =/= 0
  cmdAllowedStart := io.input.sharedCmd.valid && allowCmd && (RegInit(True) clearWhen(cmdAllowedStart) setWhen(io.input.sharedCmd.ready))


  io.input.sharedCmd.ready := (decodedCmdSels & (io.readOutputs.map(_.readCmd.ready) ++ io.writeOutputs.map(_.writeCmd.ready) ++ io.sharedOutputs.map(_.sharedCmd.ready)).asBits).orR && allowCmd
  //Wire readCmd
  for((output,sel) <- (io.readOutputs,decodedCmdSels(readRange).asBools).zipped){
    output.readCmd.valid := io.input.sharedCmd.valid && sel && allowCmd
    output.readCmd.payload.assignSomeByName(io.input.sharedCmd.payload)
  }
  //Wire writeCmd
  for((output,sel) <- (io.writeOutputs,decodedCmdSels(writeRange).asBools).zipped){
    output.writeCmd.valid := io.input.sharedCmd.valid && sel && allowCmd
    output.writeCmd.payload.assignSomeByName(io.input.sharedCmd.payload)
  }
  //Wire sharedCmd
  for((output,sel) <- (io.sharedOutputs,decodedCmdSels(sharedRange).asBools).zipped){
    output.sharedCmd.valid := io.input.sharedCmd.valid && sel && allowCmd
    output.sharedCmd.payload.assignSomeByName(io.input.sharedCmd.payload)
  }

  io.input.writeData.ready := (pendingSels(sharedRange) ## (pendingSels(writeRange)) & (io.writeOutputs.map(_.writeData.ready) ++ io.sharedOutputs.map(_.writeData.ready)).asBits).orR && allowData
  //Wire writeWriteData
  for((output,sel) <- (io.writeOutputs,pendingSels(writeRange).asBools).zipped){
    output.writeData.valid   := io.input.writeData.valid && sel && allowData
    output.writeData.payload := io.input.writeData.payload
  }
  //Wire sharedWriteData
  for((output,sel) <- (io.sharedOutputs,pendingSels(sharedRange).asBools).zipped){
    output.writeData.valid   := io.input.writeData.valid && sel && allowData
    output.writeData.payload := io.input.writeData.payload
  }

  //Wire writeRsp
  val writeRspIndex = OHToUInt(pendingSels(sharedRange) ## pendingSels(writeRange))
  io.input.writeRsp.valid :=   (io.writeOutputs.map(_.writeRsp.valid)   ++ io.sharedOutputs.map(_.writeRsp.valid)).asBits.orR
  io.input.writeRsp.payload := (io.writeOutputs.map(_.writeRsp.payload) ++ io.sharedOutputs.map(_.writeRsp.payload)).apply(writeRspIndex)
  io.writeOutputs.foreach(_.writeRsp.ready := io.input.writeRsp.ready)
  io.sharedOutputs.foreach(_.writeRsp.ready := io.input.writeRsp.ready)

  //Wire ReadRsp
  val readRspIndex = OHToUInt(pendingSels(sharedRange) ## pendingSels(readRange))
  io.input.readRsp.valid   := (io.readOutputs.map(_.readRsp.valid)   ++ io.sharedOutputs.map(_.readRsp.valid)).asBits.orR
  io.input.readRsp.payload := (io.readOutputs.map(_.readRsp.payload) ++ io.sharedOutputs.map(_.readRsp.payload)).apply(readRspIndex)
  io.readOutputs.foreach(_.readRsp.ready := io.input.readRsp.ready)
  io.sharedOutputs.foreach(_.readRsp.ready := io.input.readRsp.ready)


  //Decoding error managment
  val decodingErrorPossible = (writeDecodings ++ sharedDecodings).map(_.size).sum < (BigInt(1) << axiConfig.addressWidth) || (readDecodings ++ sharedDecodings).map(_.size).sum < (BigInt(1) << axiConfig.addressWidth)
  val decodingError = if(decodingErrorPossible) new Area{
    val detected = io.input.sharedCmd.valid && decodedCmdSels === 0 && allowCmd
    val sendWriteRsp = RegInit(False)
    val id = if(axiConfig.useId) Reg(UInt(axiConfig.idWidth bits)) else null
    val sendReadRsp = RegInit(False)
    val remaining = Reg(UInt(8 bits))
    val remainingZero = remaining === 0

    //Stall CMD
    when(detected){
      io.input.sharedCmd.ready := False
    }


    //Flush all DATA
    when((RegNext(detected) init(False)) && !sendWriteRsp && !sendReadRsp){  // !sendRsp for consecutive access errors
      when(pendingDataCounter =/= 0) {
        io.input.writeData.ready := True
      } otherwise {
        io.input.writeData.ready := False
        io.input.sharedCmd.ready := True
        if(id != null) id := io.input.sharedCmd.id
        remaining := (if(axiConfig.useLen) io.input.sharedCmd.len else U(0))
        sendWriteRsp :=  io.input.sharedCmd.write
        sendReadRsp  := !io.input.sharedCmd.write
      }
    }

    //Send a DECERR writeRsp
    when(sendWriteRsp) {
      io.input.writeRsp.valid := True
      if(axiConfig.useResp) io.input.writeRsp.setDECERR
      if(id != null) io.input.writeRsp.id := id
      when(io.input.writeRsp.ready) {
        sendWriteRsp := False
      }
      allowCmd := False
    }

    //Send a DECERR readRsp
    when(sendReadRsp) {
      io.input.readRsp.valid := True
      if(axiConfig.useResp) io.input.readRsp.setDECERR
      if(id != null) io.input.readRsp.id := id
      if(io.input.readRsp.last != null) io.input.readRsp.last := remainingZero
      when(io.input.readRsp.ready) {
        remaining := remaining - 1
        when(remainingZero) {
          sendReadRsp := False
        }
      }
      allowCmd := False
    }
  }
}

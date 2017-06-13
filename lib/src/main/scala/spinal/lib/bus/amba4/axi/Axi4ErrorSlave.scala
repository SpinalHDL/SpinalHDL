package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib.slave

/**
 * Created by spinalvm on 13.06.17.
 */
case class Axi4WriteOnlyErrorSlave(axiConfig: Axi4Config) extends Component{
  val io = new Bundle{
    val axi = slave(Axi4WriteOnly(axiConfig))
  }
  val consumeData = RegInit(False)
  val sendRsp     = RegInit(False)
  val id          = if(axiConfig.useId) Reg(UInt(axiConfig.idWidth bits)) else null

  io.axi.writeCmd.ready := !(consumeData || sendRsp)
  when(io.axi.writeCmd.fire){
    consumeData := True
    if(id != null) id := io.axi.writeCmd.id
  }

  io.axi.writeData.ready := consumeData
  when(io.axi.writeData.fire && io.axi.writeData.last){
    consumeData := False
    sendRsp := True
  }

  io.axi.writeRsp.valid := sendRsp
  if(axiConfig.useResp) io.axi.writeRsp.setDECERR
  if(axiConfig.useId) io.axi.writeRsp.id := id
  when(io.axi.writeRsp.fire){
    sendRsp := False
  }
}


case class Axi4ReadOnlyErrorSlave(axiConfig: Axi4Config) extends Component{
  val io = new Bundle{
    val axi = slave(Axi4ReadOnly(axiConfig))
  }

  val sendRsp       = RegInit(False)
  val id            = if(axiConfig.useId) Reg(UInt(axiConfig.idWidth bits)) else null
  val remaining     = Reg(UInt(8 bits))
  val remainingZero = remaining === 0

  io.axi.readCmd.ready := !sendRsp
  when(io.axi.readCmd.fire){
    sendRsp := True
    remaining := (if(axiConfig.useLen) io.axi.readCmd.len else U(0))
    if(axiConfig.useId) id := io.axi.readCmd.id
  }

  io.axi.readRsp.valid := sendRsp
  if(axiConfig.useId) io.axi.readRsp.id := id
  if(axiConfig.useResp) io.axi.readRsp.setDECERR
  if(axiConfig.useLast) io.axi.readRsp.last := remainingZero

  when(sendRsp) {
    when(io.axi.readRsp.ready) {
      remaining := remaining - 1
      when(remainingZero) {
        sendRsp := False
      }
    }
  }
}

case class Axi4SharedErrorSlave(axiConfig: Axi4Config) extends Component{
  val io = new Bundle{
    val axi = slave(Axi4Shared(axiConfig))
  }

  val consumeData   = RegInit(False)
  val sendReadRsp   = RegInit(False)
  val sendWriteRsp  = RegInit(False)
  val id            = if(axiConfig.useId) Reg(UInt(axiConfig.idWidth bits)) else null
  val remaining     = Reg(UInt(8 bits))
  val remainingZero = remaining === 0

  io.axi.sharedCmd.ready := !(consumeData || sendWriteRsp || sendReadRsp )
  when(io.axi.sharedCmd.fire){
    consumeData :=  io.axi.sharedCmd.write
    sendReadRsp := !io.axi.sharedCmd.write
    remaining := (if(axiConfig.useLen) io.axi.sharedCmd.len else U(0))
    if(axiConfig.useId) id := io.axi.sharedCmd.id
  }

  //Write data
  io.axi.writeData.ready := consumeData
  when(io.axi.writeData.fire && io.axi.writeData.last){
    consumeData := False
    sendWriteRsp := True
  }

  //Write rsp
  io.axi.writeRsp.valid := sendWriteRsp
  if(axiConfig.useResp) io.axi.writeRsp.setDECERR
  if(axiConfig.useId) io.axi.writeRsp.id := id
  when(io.axi.writeRsp.fire){
    sendWriteRsp := False
  }

  //Read rsp
  io.axi.readRsp.valid := sendReadRsp
  if(axiConfig.useId) io.axi.readRsp.id := id
  if(axiConfig.useResp) io.axi.readRsp.setDECERR
  if(axiConfig.useLast) io.axi.readRsp.last := remainingZero
  when(sendReadRsp) {
    when(io.axi.readRsp.ready) {
      remaining := remaining - 1
      when(remainingZero) {
        sendReadRsp := False
      }
    }
  }
}
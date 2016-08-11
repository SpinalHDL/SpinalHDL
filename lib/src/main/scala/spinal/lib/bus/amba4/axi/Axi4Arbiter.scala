package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc.SizeMapping

object Axi4ReadArbiter{
  def getInputConfig(inputsConfig : Seq[Axi4Config]) = inputsConfig.head.copy(idWidth = inputsConfig.map(_.idWidth).reduce(Math.max(_,_)))
  def getOutputConfig(inputConfig : Axi4Config,inputsCount : Int) = inputConfig.copy(idWidth = inputConfig.idWidth + log2Up(inputsCount))
}

case class Axi4ReadArbiter(inputConfig: Axi4Config,inputsCount : Int,pendingId : Int) extends Component {
  assert(inputConfig.isReadOnly)
  val outputConfig = Axi4ReadArbiter.getOutputConfig(inputConfig,inputsCount)
  val io = new Bundle{
    val inputs = Vec(slave(Axi4(inputConfig)),inputsCount)
    val output = master(Axi4(outputConfig))
  }

  val cmdArbiter = StreamArbiterFactory.roundRobin.build(Axi4Ar(inputConfig),inputsCount)
  (cmdArbiter.io.inputs,io.inputs.map(_.readCmd)).zipped.map(_ <> _)
  cmdArbiter.io.output <> io.output.readCmd

  io.output.readCmd.id.removeAssignements()
  io.output.readCmd.id := (cmdArbiter.io.chosen @@ cmdArbiter.io.output.id)

  val readRspSels = (0 until inputsCount).map(io.output.readRsp.id === _)
  for((input,sel)<- (io.inputs,readRspSels).zipped){
    input.readRsp.valid := io.output.readRsp.valid && sel
    input.readRsp.payload <> io.output.readRsp.payload
    input.readRsp.id.removeAssignements()
    input.readRsp.id := io.output.readRsp.id >> log2Up(inputsCount)
  }
  io.output.readRsp.ready := (readRspSels,io.inputs.map(_.readRsp.ready)).zipped.map(_ & _).reduce(_ | _)
}










//case class Axi4IdContextBufferPushTransaction[T <: Data](dataType : T,idWidth : Int) extends Bundle{
//  val id = Bits(idWidth bits)
//  val context = cloneOf(dataType)
//}
//
//case class Axi4IdContextBufferRead[T <: Data](dataType : T,idWidth : Int) extends Bundle with IMasterSlave{
//  val id      = Bits(idWidth bits)
//  val remove  = Bool
//  val context = cloneOf(dataType)
//
//  override def asMaster(): Axi4IdContextBufferRead.this.type = {
//    out(id,remove)
//    in(context)
//    this
//  }
//
//}
//
//case class Axi4IdContextBuffer[T <: Data](dataType : T,idWidth : Int,capacity : Int) extends Component{
//  val io = new Bundle{
//    val push = slave Stream(Axi4IdContextBufferPushTransaction(dataType , idWidth))
//    val pop  = slave(Axi4IdContextBufferRead(dataType,idWidth))
//  }
//
//  val valids = Vec(Reg(Bool) init(False),capacity)
//  val ids = Vec(Reg(Bits(idWidth bits)),capacity)
//  val contexts = Vec(Reg(dataType),capacity)
//
//  io.push.ready := !valids.reduce(_ && _)
//  val pushMask = io.push.valid ? OHMasking.first(~valids.asBits) | 0
//  for(idx <- 0 until capacity){
//    when(pushMask(idx)) {
//      valids(idx) := True
//      ids(idx) := io.push.id
//      contexts(idx) := io.push.context
//    }
//  }
//
//  val popId = OHToUInt(ids.map(_ === io.pop.id))
//  io.pop.context := contexts(popId)
//  when(io.pop.remove){
//    valids(popId) := False
//  }
//}
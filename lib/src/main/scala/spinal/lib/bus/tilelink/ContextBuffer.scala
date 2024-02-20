package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._

trait ContextAsyncBufferFactory{
  def apply[T <: Data](idWidth : Int, contextType : HardType[T]) : ContextAsyncBufferBase[T]
}

abstract class ContextAsyncBufferBase[T <: Data](idWidth : Int, contextType : HardType[T]) extends Component {
  val io = new Bundle {
    val add    = slave Stream(ContextBufferAdd(idWidth, contextType))
    val remove = slave Flow(ContextBufferRemove(idWidth))
    val query  = slave (ContextBufferQuery(idWidth, contextType))

    def bind(aHalt : Bool, a : Stream[ChannelA], d : Stream[ChannelD]): Unit ={
      add.valid := a.fire && a.isLast()
      add.id := a.source
      aHalt setWhen(!add.ready)
      remove.valid := d.fire && d.isLast() && Opcode.D.fromA(d.opcode)
      remove.id := d.source
    }
  }
}


case class ContextBufferAdd[T <: Data](idWidth : Int, contextType : HardType[T]) extends Bundle {
  val id = UInt(idWidth bits)
  val context = contextType()
}

case class ContextBufferRemove[T <: Data](idWidth : Int) extends Bundle {
  val id = UInt(idWidth bits)
}

case class ContextBufferQuery[T <: Data](idWidth : Int, contextType : HardType[T]) extends Bundle with IMasterSlave {
  val id = UInt(idWidth bits)
  val context = contextType()

  override def asMaster() = {
    out(id)
    in(context)
  }
}

object ContextAsyncBufferFull extends ContextAsyncBufferFactory{
  override def apply[T <: Data](idWidth: Int, contextType: HardType[T]) = new ContextAsyncBufferFull(idWidth, contextType)
}
class ContextAsyncBufferFull[T <: Data](idWidth : Int, contextType : HardType[T]) extends ContextAsyncBufferBase[T](idWidth, contextType){
  val contexts = Mem.fill(1 << idWidth)(contextType)
  val write = contexts.writePort()
  write.valid   := io.add.valid
  write.address := io.add.id
  write.data    := io.add.context
  io.add.ready  := True

  val read = contexts.readAsyncPort()
  read.address := io.query.id
  io.query.context := read.data
}
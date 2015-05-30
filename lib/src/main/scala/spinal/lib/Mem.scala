package spinal.lib

import spinal.core._


class StreamReadRetData[T <: Data, T2 <: Data](readType: T, linkedType: T2) extends Bundle {
  val value = cloneOf(readType)
  val linked = cloneOf(linkedType)

  override def clone(): this.type = new StreamReadRetData(readType, linkedType).asInstanceOf[this.type]
}

class MemPimped[T <: Data](mem: Mem[T]) {
  def streamReadSync[T2 <: Data](cmd: Stream[UInt], linkedData: T2) = {
    val ret = Stream(new StreamReadRetData(mem.wordType, linkedData))

    val retValid = RegInit(False)
    val retData = mem.readSync(cmd.data, cmd.ready)
    val retLinked = RegNextWhen(linkedData, cmd.ready)

    when(ret.ready) {
      retValid := Bool(false)
    }
    when(cmd.ready) {
      retValid := cmd.valid
    }

    cmd.ready := ret.isFree

    ret.valid := retValid
    ret.data.value := retData
    ret.data.linked := retLinked
    ret
  }

  def streamReadSync(cmd: Stream[UInt]) = {
    val ret = Stream(mem.wordType)

    val retValid = RegInit(False)
    val retData = mem.readSync(cmd.data, cmd.ready)

    when(ret.ready) {
      retValid := Bool(false)
    }
    when(cmd.ready) {
      retValid := cmd.valid
    }

    cmd.ready := ret.isFree

    ret.valid := retValid
    ret.data := retData
    ret
  }
}

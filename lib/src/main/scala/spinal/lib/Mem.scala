package spinal.lib

import spinal.core._


case class ReadRetLinked[T <: Data, T2 <: Data](readType: T, linkedType: T2) extends Bundle {
  val value = cloneOf(readType)
  val linked = cloneOf(linkedType)
}

class MemPimped[T <: Data](mem: Mem[T]) {

  //def streamReadSync[T2 <: Data](event : Event,address: UInt, linkedData: T2) : (Event,T,T2) = {

  def streamReadSync[T2 <: Data](cmd: Stream[UInt], linkedData: T2) : Stream[ReadRetLinked[T,T2]] = {
    val ret = Stream(new ReadRetLinked(mem.wordType, linkedData))

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

  def streamReadSync(cmd: Stream[UInt]): Stream[T] = {
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

  def flowReadSync(cmd : Flow[UInt]) : Flow[T] = {
    val ret = Flow(mem.wordType)
    ret.valid := RegNext(cmd.valid)
    ret.data := mem.readSync(cmd.data)
    ret
  }

  def flowReadSync[T2 <: Data](cmd: Flow[UInt], linkedData: T2) : Flow[ReadRetLinked[T,T2]] = {
    val ret = Flow(ReadRetLinked(mem.wordType, linkedData))
    ret.valid := RegNext(cmd.valid)
    ret.data.linked := RegNext(linkedData)
    ret.data.value := mem.readSync(cmd.data)
    ret
  }
}

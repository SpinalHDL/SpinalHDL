package spinal.lib.io

import spinal.core._
import spinal.lib.IMasterSlave

case class TriState[T <: Data](dataType : HardType[T]) extends Bundle with IMasterSlave{
  val read,write : T = dataType()
  val writeEnable = Bool

  override def asMaster(): Unit = {
    out(write,writeEnable)
    in(read)
  }

  def stage() = {
    val ret = TriState(dataType).setCompositeName(this, "stage", true)
    ret.writeEnable := RegNext(this.writeEnable)
    ret.write := RegNext(this.write)
    this.read := RegNext(ret.read)
    ret
  }

  def <<(m : TriState[T]) : Unit = {
    this.writeEnable := m.writeEnable
    this.write := m.write
    m.read := this.read
  }
}


object TriStateArray{
  def apply(width : BitCount) : TriStateArray = TriStateArray(width.value)
}

case class TriStateArray(width : Int) extends Bundle with IMasterSlave{
  val read,write,writeEnable = Bits(width bits)

  override def asMaster(): Unit = {
    out(write,writeEnable)
    in(read)
  }

  def apply(i : Int) : TriState[Bool] = {
    val ret = TriState(Bool)

    //Make ret readable
    ret.read := this.read(i)
    ret.write := this.write(i)
    ret.writeEnable := this.writeEnable(i)

    //Define a fonction which redirect write access of a userSignal to another signal
    def writePatch(userSignal : BaseType, realSignal : BaseType) : Unit = {
      userSignal.compositeAssign = new Assignable {
        override def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef): Unit = that match {
          case that: BaseType => realSignal.compositAssignFrom(that, realSignal, kind)
        }
        override def getRealSourceNoRec: BaseType = userSignal
      }
    }

    //Make ret writable
    writePatch(ret.read, this.read(i))
    writePatch(ret.write, this.write(i))
    writePatch(ret.writeEnable, this.writeEnable(i))

    ret
  }
}


case class TriStateOutput[T <: Data](dataType : HardType[T]) extends Bundle with IMasterSlave{
  val write : T = dataType()
  val writeEnable = Bool

  override def asMaster(): Unit = {
    out(write,writeEnable)
  }
}

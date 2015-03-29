package spinal.lib

import spinal.core._

class FragmentFactory {
  def apply[T <: Data](dataType: T): Fragment[T] = new Fragment(dataType)
}

object Fragment extends FragmentFactory

object FlowFragment extends FlowFragmentFactory

object HandshakeFragment extends HandshakeFragmentFactory


class FlowFragmentPimped[T <: Data](that: Flow[Fragment[T]]) {
  def last = that.data.last
  def fragment = that.data.fragment
}


class HandshakeFragmentPimped[T <: Data](pimped: Handshake[Fragment[T]]) {
  def last = pimped.data.last
  def fragment = pimped.data.fragment



  def insertHeader(header : T) : Handshake[Fragment[T]] = {
    val ret = cloneOf(pimped)
    val waitPacket = RegInit(True)

    ret.valid := False
    ret.data.last := False

    when(pimped.valid){
      ret.valid := True
      when(waitPacket){
        ret.data.fragment := header
      } otherwise{
        ret.data := pimped.data
      }
    }

    when(ret.fire){
      waitPacket := False
      when(ret.data.last){
        waitPacket := True
      }
    }

    ret
  }
}


class DataCarrierFragmentPimped[T <: Data](pimped: DataCarrier[Fragment[T]]) {
  def isNotInTail = RegNextWhen(pimped.data.last, pimped.fire, True)
  def isInTail = !isNotInTail

  def isFirst = pimped.valid && isNotInTail
  def isLast = pimped.valid && pimped.data.last
}


class DataCarrierFragmentBitsPimped(pimped: DataCarrier[Fragment[Bits]]) {

  //Little endian
  def toFlowOf[T <: Data](toDataType: T): Flow[T] = {
    val fromWidth = pimped.data.fragment.getWidth
    val toWidth = toDataType.getBitsWidth
    val ret = Flow(toDataType)

    pimped.freeRun

    if (toWidth <= fromWidth) {
      ret.valid := pimped.fire && pimped.data.last
      ret.data.assignFromBits(pimped.data.fragment)
    } else {
      val missingBitsCount = toWidth - fromWidth

      val buffer = Reg(Bits(((missingBitsCount-1)/fromWidth + 1)*fromWidth bit))
      when(pimped.fire) {
        buffer := pimped.data.fragment ## (buffer >> fromWidth)
      }

      ret.valid := pimped.isLast
      ret.data.assignFromBits(pimped.data.fragment ## buffer)
    }
    ret
  }
}


class FlowFragmentFactory extends MSFactory {
  def apply[T <: Data](dataType: T): Flow[Fragment[T]] = {
    val ret = new Flow(Fragment(dataType))
    postApply(ret)
    ret
  }
}

class HandshakeFragmentFactory extends MSFactory {
  def apply[T <: Data](dataType: T): Handshake[Fragment[T]] = {
    val ret = new Handshake(Fragment(dataType))
    postApply(ret)
    ret
  }
}


class Fragment[T <: Data](dataType: T) extends Bundle {
  val last = Bool
  val fragment: T = dataType.clone

  override def clone: this.type = {
    new Fragment(dataType).asInstanceOf[this.type];
  }
}




object FlowFragmentRouter {
  def apply(input: Flow[Fragment[Bits]], outputSize: Int): Vec[Flow[Fragment[Bits]]] = {
    FlowFragmentRouter(input, (0 until outputSize).map(BigInt(_)))
  }

  def apply(input: Flow[Fragment[Bits]], mapTo: Iterable[BigInt]): Vec[Flow[Fragment[Bits]]] = {
    val router = new FlowFragmentRouter(input, mapTo)
    return router.outputs
  }
}

class FlowFragmentRouter(input: Flow[Fragment[Bits]], mapTo: Iterable[BigInt]) extends Area {
  val outputs = Vec(mapTo.size, input)
  val enables = Vec(mapTo.size, Reg(Bool))

  outputs.foreach(_.data := input.data)
  when(input.isNotInTail) {
    (enables, mapTo).zipped.foreach((en,filter) => en := b(filter) === input.data.fragment)
  } otherwise {
    (outputs, enables).zipped.foreach(_.valid := _)
  }
}


class HandshakeToHandshakeFragmentBits[T <: Data](dataType : T,bitsWidth : Int) extends Component{
  val io = new Bundle{
    val input = slave Handshake(dataType)
    val output = master Handshake Fragment(Bits(bitsWidth bit))
  }
  val counter = Counter((widthOf(dataType)-1)/bitsWidth + 1)
  val inputBits = b(0,bitsWidth bit) ## toBits(io.input.data) //The cat allow to mux inputBits

  io.input.ready := counter.overflow
  io.output.data.last := counter.overflow
  io.output.valid := io.input.valid
  io.output.data.fragment := inputBits(counter * u(bitsWidth),bitsWidth bit)
  when(io.output.fire){
    counter++
  }
}



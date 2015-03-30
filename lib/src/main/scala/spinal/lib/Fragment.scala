package spinal.lib

import spinal.core._

class FragmentFactory {
  def apply[T <: Data](dataType: T): Fragment[T] = new Fragment(dataType)
}

object Fragment extends FragmentFactory

object FlowFragment extends FlowFragmentFactory

object HandshakeFragment extends HandshakeFragmentFactory


class FlowFragmentPimped[T <: Data](that: Flow[Fragment[T]]) {
//  def last : Bool = that.last
//  def fragment : T = that.data.fragment
}


class HandshakeFragmentPimped[T <: Data](pimped: Handshake[Fragment[T]]) {
//  def last : Bool = pimped.last
//  def fragment : T = pimped.data.fragment



  def insertHeader(header : T) : Handshake[Fragment[T]] = {
    val ret = cloneOf(pimped)
    val waitPacket = RegInit(True)

    ret.valid := False
    ret.last := False

    when(pimped.valid){
      ret.valid := True
      when(waitPacket){
        ret.fragment := header
      } otherwise{
        ret.data := pimped.data
      }
    }

    when(ret.fire){
      waitPacket := False
      when(ret.last){
        waitPacket := True
      }
    }

    ret
  }
}


class DataCarrierFragmentPimped[T <: Data](pimped: DataCarrier[Fragment[T]]) {
  def last : Bool = pimped.data.last
  def fragment : T = pimped.data.fragment


  def isNotInTail = RegNextWhen(pimped.last, pimped.fire, True)
  def isInTail = !isNotInTail

  def isFirst = pimped.valid && isNotInTail
  def isLast = pimped.valid && pimped.last
}


class DataCarrierFragmentBitsPimped(pimped: DataCarrier[Fragment[Bits]]) {

  //Little endian
  def toFlowOf[T <: Data](toDataType: T): Flow[T] = {
    val fromWidth = pimped.fragment.getWidth
    val toWidth = toDataType.getBitsWidth
    val ret = Flow(toDataType)

    pimped.freeRun

    if (toWidth <= fromWidth) {
      ret.valid := pimped.fire && pimped.last
      ret.data.assignFromBits(pimped.fragment)
    } else {
      val missingBitsCount = toWidth - fromWidth

      val buffer = Reg(Bits(((missingBitsCount-1)/fromWidth + 1)*fromWidth bit))
      when(pimped.fire) {
        buffer := pimped.fragment ## (buffer >> fromWidth)
      }

      ret.valid := pimped.isLast
      ret.data.assignFromBits(pimped.fragment ## buffer)
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
    (enables, mapTo).zipped.foreach((en,filter) => en := b(filter) === input.fragment)
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
  io.output.last := counter.overflow
  io.output.valid := io.input.valid
  io.output.fragment := inputBits(counter * u(bitsWidth),bitsWidth bit)
  when(io.output.fire){
    counter++
  }
}



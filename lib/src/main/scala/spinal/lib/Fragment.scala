package spinal.lib

import spinal.core._

class FragmentFactory {
  def apply[T <: Data](dataType: T): Fragment[T] = new Fragment(dataType)
}

object Fragment extends FragmentFactory

object FlowFragment extends FlowFragmentFactory

object HandshakeFragment extends HandshakeFragmentFactory


class FlowFragmentPimped[T <: Data](that : Flow[Fragment[T]]){
  def isFirst = isFirstFragment(that)
}


class HandshakeFragmentPimped[T <: Data](that : Handshake[Fragment[T]]){
  def isFirst = isFirstFragment(that)
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
  val last = Bool()
  val fragment : T = dataType.clone

  override def clone: this.type = {
    new Fragment(dataType).asInstanceOf[this.type];
  }
}


object isFirstFragment{
  def apply[T <: Data](flow : Flow[Fragment[T]]) : Bool = RegNextWhen(flow.data.last,flow.fire,Bool(true))
  def apply[T <: Data](flow : Handshake[Fragment[T]]) : Bool = RegNextWhen(flow.data.last,flow.fire,Bool(true))
}

object FlowFragmentRouter{
    def apply(input : Flow[Fragment[Bits]],outputSize : Int) : Vec[Flow[Fragment[Bits]]] = {
      FlowFragmentRouter(input,(0 until outputSize).map(BigInt(_)))
    }

    def apply(input : Flow[Fragment[Bits]],mapTo : Iterable[BigInt]) : Vec[Flow[Fragment[Bits]]] = {
    val router = new FlowFragmentRouter(input,mapTo)
    return router.outputs
  }
}

class FlowFragmentRouter(input : Flow[Fragment[Bits]],mapTo : Iterable[BigInt]) extends Area{
  val outputs = Vec(mapTo.size,input)
  val enables = Vec(mapTo.size,Reg(Bool()))

  outputs.foreach(_.data := input.data)
  when(input.isFirst){
    (enables,mapTo).zipped.foreach(_ := Bits(_) === input.data.fragment)
  } otherwise{
    (outputs,enables).zipped.foreach(_.valid := _)
  }
}
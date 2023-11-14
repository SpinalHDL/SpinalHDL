package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.AddressMapping
import scala.collection.Seq
import spinal.core.sim.simRandom


object BusParameter{
  def simple(addressWidth : Int,
             dataWidth : Int,
             sizeBytes : Int,
             sourceWidth : Int): BusParameter = BusParameter(
    addressWidth = addressWidth,
    dataWidth    = dataWidth,
    sizeBytes    = sizeBytes,
    sourceWidth  = sourceWidth,
    sinkWidth    = 0,
    withBCE      = false,
    withDataA    = true,
    withDataB    = false,
    withDataC    = true,
    withDataD    = true,
    node         = null
  )
}

case class BusParameter(addressWidth : Int,
                        dataWidth    : Int,
                        sizeBytes    : Int,
                        sourceWidth  : Int,
                        sinkWidth    : Int,
                        withBCE      : Boolean,
                        withDataA    : Boolean,
                        withDataB    : Boolean,
                        withDataC    : Boolean,
                        withDataD    : Boolean,
                        node         : NodeParameters){
  val dataBytes   = dataWidth/8
  val sizeMax     = log2Up(sizeBytes)
  val sizeMin     = 0 //pessimistic
  def sizeValues  = sizeMin to sizeMax
  val sizeWidth   = log2Up(sizeMax+1)
  val beatMax     = (sizeBytes+dataBytes-1)/dataBytes
  val beatWidth   = log2Up(beatMax)
  val dataBytesLog2Up = log2Up(dataBytes)

  val address     = HardType(UInt(addressWidth bits))
  val data        = HardType(Bits(dataWidth bits))
  val mask        = HardType(Bits(dataBytes bits))
  val source      = HardType(UInt(sourceWidth bits))
  val sink        = HardType(UInt(sinkWidth bits))
  val size        = HardType(UInt(sizeWidth bits))
  val beat        = HardType(UInt(beatWidth  bits))
}

object SizeRange{
  def none = SizeRange(0, 0)
  def all = SizeRange(1, 4096)
  def upTo(x: Int): SizeRange = SizeRange(1, x)
  def downTo(x: Int): SizeRange = SizeRange(x, 4096)
  def apply(x: Int) : SizeRange = SizeRange(x, x)
}

case class SizeRange(min : Int, max : Int){

  require (min <= max, s"Min transfer $min > max transfer $max")
  require (min >= 0 && max >= 0, s"TransferSupport must be positive, got: ($min, $max)")
  require (max == 0 || isPow2(max), s"TransferSupport must be a power of 2, got: $max")
  require (min == 0 || isPow2(min), s"TransferSupport must be a power of 2, got: $min")
  require (max == 0 || min != 0, s"TransferSize 0 is forbidden unless (0,0), got: ($min, $max)")

  def foreach(body : Int => Unit) = for(i <- log2Up(min) to log2Up(max)) body(1 << i)
  def none = min == 0
  def some = !none
  def contains(x: Int) = isPow2(x) && min <= x && x <= max
  def containsLg(x: Int) = contains(1 << x)

  def contains(x: SizeRange) = x.none || (min <= x.min && x.max <= max)

  def intersect(x: SizeRange) =
    if (x.max < min || max < x.min) SizeRange.none
    else SizeRange(scala.math.max(min, x.min), scala.math.min(max, x.max))

  def mincover(x: SizeRange) = {
    if (none) {
      x
    } else if (x.none) {
      this
    } else {
      SizeRange(scala.math.min(min, x.min), scala.math.max(max, x.max))
    }
  }

  def random(randMax : Int) : Int = {
    val min = log2Up(this.min)
    val max = log2Up(this.max min randMax)
    1 << (min + simRandom.nextInt(max+1-min))
  }

  def getSingleSize(): Option[Int] = {
    if(min == max) Some(min) else None
  }

  override def toString() = "TransferSupport[%d, %d]".format(min, max)
}



case class NodeParameters(m : M2sParameters,
                          s : S2mParameters = S2mParameters.none()){
  val sizeBytes = s.sizeBytes max m.sizeBytes
  val withBCE = s.withBCE || m.withBCE
  def toBusParameter() = BusParameter(
    addressWidth  = m.addressWidth,
    dataWidth     = m.dataWidth,
    sizeBytes     = sizeBytes,
    sourceWidth   = m.sourceWidth,
    sinkWidth     = s.sinkWidth,
    withBCE       = withBCE,
    withDataA     = m.withDataA,
    withDataB     = s.withDataB,
    withDataC     = true,
    withDataD     = m.withDataD,
    node          = this
  )
}

object NodeParameters{
  def mergeMasters(nodes : Seq[NodeParameters]): NodeParameters ={
    NodeParameters(
      m = mergeMasters(nodes.map(_.m)),
      s = nodes.head.s
    )
  }

  def mergeMasters(node : Seq[M2sParameters]): M2sParameters ={
    val sourcePreWidth = node.map(_.sourceWidth).max
    M2sParameters(
      addressWidth = node.map(_.addressWidth) max,
      dataWidth = node.map(_.dataWidth) max,
      masters = node.zipWithIndex.flatMap{
        case (m, i) => m.masters.map(_.withSourceOffset(i << sourcePreWidth))
      }
    )
  }


  def mergeSlaves(node : Seq[S2mParameters]): S2mParameters ={
    if(node.exists(_.withBCE)) {
      val sinkPreWidth = node.map(_.sinkWidth).max
      val ref = S2mParameters(
        slaves = node.zipWithIndex.flatMap {
          case (s, i) => s.slaves.map(_.withSinkOffset(i << sinkPreWidth))
        }
      )

      var sinkId = -1
      val ret = S2mParameters(
        slaves = node.flatMap { s =>
          if(s.emits.withAny) sinkId += 1
          s.slaves.map(e => e.withSinkOffset(if(e.emits.withAny) {sinkId << sinkPreWidth } else 0))
        }
      )
      ret
    } else {
      S2mParameters(
        slaves = node.zipWithIndex.flatMap {
          case (s, i) => s.slaves
        }
      )
    }
  }

  def mergeNodes(nodes : Seq[NodeParameters]): NodeParameters ={
    NodeParameters(
      m = mergeMasters(nodes.map(_.m)),
      s = mergeSlaves(nodes.map(_.s))
    )
  }
}
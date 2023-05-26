package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping}

import scala.collection.mutable.ArrayBuffer

object Decoder{
  def upNodeFrom(downs : Seq[NodeParameters]) : NodeParameters = {
    NodeParameters.mergeNodes(downs)
  }
  def upSlavesFrom(downs : Seq[S2mParameters]) : S2mParameters = {
    NodeParameters.mergeSlaves(downs)
  }
  def downMastersFrom(up : M2sParameters, down : M2sSupport) : M2sParameters = {
    up.copy(
      addressWidth = down.addressWidth,
      masters = up.masters.map(e =>
        e.copy(
          mapping = e.mapping.map( m =>
            m.copy(
              emits = m.emits.intersect(down.transfers)
            )
          )
        )
      )
    )
  }
}

case class Decoder(upNode : NodeParameters, downsSupports : Seq[M2sSupport], downsS2m : Seq[S2mParameters], mapping : Seq[AddressMapping]) extends Component{
  val downsNodes = (downsSupports, downsS2m). zipped.map((support, s2m) => upNode.copy(
    m = Decoder.downMastersFrom(upNode.m, support),
    s = s2m
  ))

  assert(!mapping.contains(DefaultMapping))

  val io = new Bundle{
    val up = slave(Bus(upNode))
    val downs = Vec(downsNodes.map(e => master(Bus(e))))
  }

  val sinkOffsetWidth = if(downsNodes.exists(_.withBCE)) log2Up(downsNodes.size) else 0
  val downs = io.downs.zipWithIndex.map{case (bus, id) => bus.fromSinkOffset(id << sinkOffsetWidth, upNode.s.sinkWidth)}

  val a = new Area{
    val readys = ArrayBuffer[Bool]()
    val logic = for((s, id) <- downs.zipWithIndex) yield new Area {
      val hit = mapping(id).hit(io.up.a.address) && s.p.node.m.emits.contains(io.up.a.opcode)
      s.a.valid := io.up.a.valid && hit
      s.a.payload := io.up.a.payload
      s.a.address.removeAssignments() := io.up.a.address.resized
      readys += s.a.ready && hit
    }
    io.up.a.ready := readys.orR

    val miss = !logic.filter(_ != null).map(_.hit).orR
    assert(!(io.up.a.valid && miss))
  }

  val b = upNode.withBCE generate new Area{
    val arbiter = StreamArbiterFactory().roundRobin.lambdaLock[ChannelB](_.isLast()).build(ChannelB(upNode), downsNodes.filter(_.withBCE).size)
    val iter = arbiter.io.inputs.iterator
    for(i <- 0 until downsSupports.size if downsNodes(i).withBCE){
      val arbiterInput = iter.next()
      arbiterInput << downs(i).b

      val base = mapping(i) match{
        case DefaultMapping => BigInt(0)
        case v => v.lowerBound
      }
      arbiterInput.address.removeAssignments() := downs(i).b.address.resize(upNode.m.addressWidth) | base
    }
    arbiter.io.output >> io.up.b
  }

  val c = upNode.withBCE generate new Area{
    val readys = ArrayBuffer[Bool]()
    val logic = for((s, id) <- downs.zipWithIndex if s.p.withBCE) yield new Area {
      val hit = mapping(id).hit(io.up.c.address)
      s.c.valid := io.up.c.valid && hit
      s.c.payload := io.up.c.payload
      s.c.address.removeAssignments() := io.up.c.address.resized
      readys += s.a.ready && hit
    }
    io.up.c.ready := readys.orR
  }

  val d = new Area{
    val arbiter = StreamArbiterFactory().roundRobin.lambdaLock[ChannelD](_.isLast()).build(ChannelD(upNode), downsNodes.size)
    (arbiter.io.inputs, downs).zipped.foreach(_ connectFromRelaxed _.d)
    arbiter.io.output >> io.up.d
  }

  val e = upNode.withBCE generate new Area{
    val sel = io.up.d.sink.takeHigh(sinkOffsetWidth).asUInt
    io.up.e.ready := downs.map(v => if(v.p.withBCE) v.e.ready else False).read(sel)
    for((s, id) <- downs.zipWithIndex if s.p.withBCE) {
      val hit = sel === id
      s.e.valid := io.up.e.valid && hit
      s.e.payload := io.up.e.payload
    }
  }
}

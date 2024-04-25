package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, AddressTransformer, DefaultMapping, InterleavedMapping, NeverMapping}
import spinal.lib.logic.{DecodingSpec, DecodingSpecExample, Masked, Symplify}
import spinal.lib.system.tag.{MappedNode, MappedTransfers}

import scala.collection.mutable.ArrayBuffer
import scala.collection.{Seq, mutable}

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

case class DecoderDownSpec(mappeds : Seq[MappedTransfers],
                           transformers : Seq[AddressTransformer],
                           nodeParam : NodeParameters)
case class Decoder(upNode : NodeParameters,
                   downsSpec : Seq[DecoderDownSpec]) extends Component{
  //TODO it doesn't check for overlapp (elaboration time)
  val io = new Bundle{
    val up = slave(Bus(upNode))
    val downs = Vec(downsSpec.map(e => master(Bus(e.nodeParam))))
  }

  val sinkOffsetWidth = log2Up(downsSpec.count(_.nodeParam.withBCE))
  val perNodeSinkWidth = downsSpec.map(_.nodeParam.s.sinkWidth).max
  var sinkPtr = -1
  val downs = io.downs.map { bus =>
    if(bus.p.withBCE) sinkPtr += 1
    bus.fromSinkOffset(sinkPtr << perNodeSinkWidth, upNode.s.sinkWidth)
  }

  def getTermsA(m : AddressMapping, s : M2sTransfers) : Seq[Masked] = {
    val mappingTerms = AddressMapping.terms(m, io.up.p.addressWidth)
    val opcodeTerms = ArrayBuffer[Masked]()
    def op(filter : M2sTransfers => Boolean, op : Int) = if(filter(s)) opcodeTerms += Masked(BigInt(op) << io.up.p.addressWidth, BigInt(7) << io.up.p.addressWidth)
    op(_.get.some, 4)
    op(_.putFull.some, 0)
    op(_.putPartial.some, 1)
    op(e => e.acquireB.some || e.acquireT.some, 6)
    op(e => e.acquireB.some || e.acquireT.some, 7)
    val terms = ArrayBuffer[Masked]()
    for(m <- mappingTerms; o <- opcodeTerms){
      terms += m fuse o
    }
    terms
  }

  val allTermsA, allTermsC = mutable.LinkedHashMap[MappedTransfers, Seq[Masked]]()
  for(spec <- downsSpec; mt <- spec.mappeds){
    allTermsA(mt) = getTermsA(mt.mapping, mt.transfers.asInstanceOf[M2sTransfers])
    if(mt.transfers.asInstanceOf[M2sTransfers].withBCE){
      allTermsC(mt) = AddressMapping.terms(mt.mapping, io.up.p.addressWidth)
    }
  }

  def decodeA(mts : Seq[MappedTransfers], key : Bits) = {
    val dc = new DecodingSpec(Bool())
    dc.setDefault(Masked.zero)
    for(mt <- mts) dc.addNeeds(allTermsA(mt), Masked.one)
    dc.build(key, allTermsA.values.flatten)
  }

  def decodeC(mts : Seq[MappedTransfers], key : Bits): Bool = {
    val dc = new DecodingSpec(Bool())
    dc.setDefault(Masked.zero)
    for (mt <- mts if mt.transfers.asInstanceOf[M2sTransfers].withBCE) dc.addNeeds(allTermsC(mt), Masked.one)
    dc.build(key, allTermsC.values.flatten)
  }

  val a = new Area{
    val readys = ArrayBuffer[Bool]()
    val key = io.up.a.opcode ## io.up.a.address
    val logic = for((s, id) <- downs.zipWithIndex) yield new Area {
      val hit = decodeA(downsSpec(id).mappeds, key) //mapping(id).hit(io.up.a.address)// && s.p.node.m.emits.contains(io.up.a.opcode)
      s.a.valid := io.up.a.valid && hit
      s.a.payload := io.up.a.payload
      s.a.address.removeAssignments() := downsSpec(id).transformers(io.up.a.address).resized
      s.a.size.removeAssignments() := io.up.a.size.resized
      readys += s.a.ready && hit
    }
    io.up.a.ready := readys.orR

    val miss = !logic.filter(_ != null).map(_.hit).orR
    assert(!(io.up.a.valid && miss), "Tilelink decoder miss ???")
    assert(!(io.up.a.valid && CountOne(io.downs.map(_.a.valid)) =/= 1), "Tilelink decoder miss ???")
  }

  val b = upNode.withBCE generate new Area{
    val arbiter = StreamArbiterFactory().roundRobin.lambdaLock[ChannelB](_.isLast()).build(ChannelB(upNode), downsSpec.filter(_.nodeParam.withBCE).size)
    val iter = arbiter.io.inputs.iterator
    for(i <- 0 until downsSpec.size if downsSpec(i).nodeParam.withBCE){
      val arbiterInput = iter.next()
      arbiterInput << downs(i).b
      arbiterInput.address.removeAssignments() := downsSpec(i).transformers.invert(downs(i).b.address.resize(upNode.m.addressWidth))
      arbiterInput.size.removeAssignments() := downs(i).b.size.resized
    }
    arbiter.io.output >> io.up.b
  }

  val c = upNode.withBCE generate new Area{
    val readys = ArrayBuffer[Bool]()
    val key = io.up.c.address asBits
    val logic = for((s, id) <- downs.zipWithIndex if s.p.withBCE) yield new Area {
      val hit = decodeC(downsSpec(id).mappeds, key) //mapping(id).hit(io.up.c.address)
      s.c.valid := io.up.c.valid && hit
      s.c.payload := io.up.c.payload
      s.c.address.removeAssignments() := downsSpec(id).transformers(io.up.c.address).resized
      s.c.size.removeAssignments() := io.up.c.size.resized
      readys += s.c.ready && hit
    }

    io.up.c.ready := readys.orR
  }

  val d = new Area{
    val arbiter = StreamArbiterFactory().roundRobin.lambdaLock[ChannelD](_.isLast()).build(ChannelD(upNode), downsSpec.size)
    (arbiter.io.inputs, downs).zipped.foreach{(arb, down) =>
      arb.arbitrationFrom(down.d)
      arb.payload.weakAssignFrom(down.d.payload)
    }
    arbiter.io.output >> io.up.d
  }

  val e = upNode.withBCE generate new Area{
    val sel = io.up.e.sink.takeHigh(sinkOffsetWidth).asUInt
    io.up.e.ready := downs.filter(_.p.withBCE).map(_.e.ready).read(sel)
    for((s, id) <- downs.filter(_.p.withBCE).zipWithIndex) {
      val hit = sel === id
      s.e.valid := io.up.e.valid && hit
      s.e.payload := io.up.e.payload
    }
  }
}

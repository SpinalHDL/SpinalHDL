package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, AddressTransformer, DefaultMapping, InterleavedMapping, NeverMapping}
import spinal.lib.logic.{Masked, Symplify}

import scala.collection.mutable.ArrayBuffer
import scala.collection.Seq

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

case class Decoder(upNode : NodeParameters,
                   downsSupports : Seq[M2sSupport],
                   downsS2m : Seq[S2mParameters],
                   mapping : Seq[AddressMapping],
                   transformers : Seq[Seq[AddressTransformer]]) extends Component{
  val downsNodes = (downsSupports, downsS2m). zipped.map((support, s2m) => upNode.copy(
    m = Decoder.downMastersFrom(upNode.m, support),
    s = s2m
  ))

  assert(mapping.forall(_ != DefaultMapping))
  for(self <- mapping.indices;
      other <- mapping.indices.dropWhile(_ != self).tail){
    if(!mapping(self).isInstanceOf[InterleavedMapping]) { //workaround for now
      if (downsSupports(self).transfers.intersect(downsSupports(other).transfers).nonEmpty) {
        assert(mapping(self).intersect(mapping(other)) == NeverMapping, s"Overlap between $self and $other")
      }
    }
  }

  val io = new Bundle{
    val up = slave(Bus(upNode))
    val downs = Vec(downsNodes.map(e => master(Bus(e))))
  }

  val sinkOffsetWidth = log2Up(downsNodes.count(_.withBCE))
  val perNodeSinkWidth = downsNodes.map(_.s.sinkWidth).max
  var sinkPtr = -1
  val downs = io.downs.map { bus =>
    if(bus.p.withBCE) sinkPtr += 1
    bus.fromSinkOffset(sinkPtr << perNodeSinkWidth, upNode.s.sinkWidth)
  }

  def getTermsA(m : AddressMapping, s : M2sSupport) : Seq[Masked] = {
    val mappingTerms = AddressMapping.terms(m, io.up.p.addressWidth)
    val opcodeTerms = ArrayBuffer[Masked]()
    def op(filter : M2sTransfers => Boolean, op : Int) = if(filter(s.transfers)) opcodeTerms += Masked(op << io.up.p.addressWidth, 7 << io.up.p.addressWidth)
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

  def decodeA(id : Int, key : Bits) = {
    val trueTerms, falseTerms = ArrayBuffer[Masked]()
    for(eid <- 0 until mapping.size){
      val terms = getTermsA(mapping(eid), downsSupports(eid))
      val target = if(eid == id) trueTerms else falseTerms
      target ++= terms
    }
    Symplify(key, trueTerms, falseTerms)
  }

  def decodeC(id : Int, key : Bits): Bool = {
    val trueTerms, falseTerms = ArrayBuffer[Masked]()
    for(eid <- 0 until mapping.size if downsSupports(eid).transfers.withBCE){
      val terms = AddressMapping.terms(mapping(eid), io.up.p.addressWidth)
      val target = if(eid == id) trueTerms else falseTerms
      target ++= terms
    }
    Symplify(key, trueTerms, falseTerms)
  }

  val a = new Area{
    val readys = ArrayBuffer[Bool]()
    val key = io.up.a.opcode ## io.up.a.address
    val logic = for((s, id) <- downs.zipWithIndex) yield new Area {
      val hit = decodeA(id, key) //mapping(id).hit(io.up.a.address)// && s.p.node.m.emits.contains(io.up.a.opcode)
      s.a.valid := io.up.a.valid && hit
      s.a.payload := io.up.a.payload
      s.a.address.removeAssignments() := transformers(id)(io.up.a.address).resized
      s.a.size.removeAssignments() := io.up.a.size.resized
      readys += s.a.ready && hit
    }
    io.up.a.ready := readys.orR

    val miss = !logic.filter(_ != null).map(_.hit).orR
    assert(!(io.up.a.valid && miss), "Tilelink decoder miss ???")
  }

  val b = upNode.withBCE generate new Area{
    val arbiter = StreamArbiterFactory().roundRobin.lambdaLock[ChannelB](_.isLast()).build(ChannelB(upNode), downsNodes.filter(_.withBCE).size)
    val iter = arbiter.io.inputs.iterator
    for(i <- 0 until downsSupports.size if downsNodes(i).withBCE){
      val arbiterInput = iter.next()
      arbiterInput << downs(i).b
      arbiterInput.address.removeAssignments() := transformers(i).invert(downs(i).b.address.resize(upNode.m.addressWidth))
      arbiterInput.size.removeAssignments() := downs(i).b.size.resized
    }
    arbiter.io.output >> io.up.b
  }

  val c = upNode.withBCE generate new Area{
    val readys = ArrayBuffer[Bool]()
    val key = io.up.c.address asBits
    val logic = for((s, id) <- downs.zipWithIndex if s.p.withBCE) yield new Area {
      val hit = decodeC(id, key) //mapping(id).hit(io.up.c.address)
      s.c.valid := io.up.c.valid && hit
      s.c.payload := io.up.c.payload
      s.c.address.removeAssignments() := transformers(id)(io.up.c.address).resized
      s.c.size.removeAssignments() := io.up.c.size.resized
      readys += s.c.ready && hit
    }

    io.up.c.ready := readys.orR
  }

  val d = new Area{
    val arbiter = StreamArbiterFactory().roundRobin.lambdaLock[ChannelD](_.isLast()).build(ChannelD(upNode), downsNodes.size)
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

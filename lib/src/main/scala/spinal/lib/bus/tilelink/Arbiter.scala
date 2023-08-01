package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import scala.collection.Seq

object Arbiter{
  def downMastersFrom(ups : Seq[M2sParameters]) : M2sParameters = {
    NodeParameters.mergeMasters(ups)
  }
  def downNodeFrom(ups : Seq[NodeParameters]) : NodeParameters = {
    NodeParameters.mergeMasters(ups)
  }
  def upSlaveFrom(down : S2mParameters, up : S2mSupport) : S2mParameters = {
    up.transfers.withAny match {
      case true => down.copy(
        slaves = down.slaves.map(e =>
          e.copy(
            emits = e.emits.intersect(up.transfers)
          )
        )
      )
      case false => S2mParameters.none()
    }
  }
}

case class Arbiter(upsNodes : Seq[NodeParameters], downNode : NodeParameters) extends Component{
  val obp = downNode //Arbiter.downNodeFrom(upsNodes)
  val io = new Bundle{
    val ups = Vec(upsNodes.map(e => slave(Bus(e))))
    val down = master(Bus(obp))
  }

  val sourceOffsetWidth = log2Up(upsNodes.size)
  val perNodeSourceWidth = upsNodes.map(_.m.sourceWidth).max
  val ups = io.ups.zipWithIndex.map{case (bus, id) => bus.withSourceOffset(id << perNodeSourceWidth, obp.m.sourceWidth)}

  val a = new Area{
    val arbiter = StreamArbiterFactory().roundRobin.lambdaLock[ChannelA](_.isLast()).build(ChannelA(obp.toBusParameter()), upsNodes.size)
//    (arbiter.io.inputs, ups).zipped.foreach(_ connectFromRelaxed _.a)
    (arbiter.io.inputs, ups).zipped.foreach{(arb, up) =>
      arb.arbitrationFrom(up.a)
      arb.payload.weakAssignFrom(up.a.payload)
    }
    arbiter.io.output >> io.down.a
//    io.down.a.source(obp.m.sourceWidth-sourceOffsetWidth, sourceOffsetWidth bits) := arbiter.io.chosen
  }

  val b = obp.withBCE generate new Area{
    val sel = io.down.b.source.takeHigh(sourceOffsetWidth).asUInt
    io.down.b.ready := ups.map(e => if(e.p.withBCE) e.b.ready else False).read(sel)
    for((s, id) <- ups.zipWithIndex if s.p.withBCE) {
      val hit = sel === id
      s.b.valid := io.down.b.valid && hit
      s.b.payload := io.down.b.payload
    }
  }

  val c = obp.withBCE generate new Area{
    val arbiter = StreamArbiterFactory().roundRobin.lambdaLock[ChannelC](_.isLast()).build(ChannelC(obp.toBusParameter()), upsNodes.filter(_.withBCE).size)
    (arbiter.io.inputs, ups.filter(_.p.withBCE)).zipped.foreach(_ << _.c)
    arbiter.io.output >> io.down.c
//    io.down.c.source(obp.m.sourceWidth-sourceOffsetWidth, sourceOffsetWidth bits) := arbiter.io.chosen
  }

  val d = new Area{
    val sel = io.down.d.source.takeHigh(sourceOffsetWidth).asUInt
    io.down.d.ready := ups.map(_.d.ready).read(sel)
    for((s, id) <- ups.zipWithIndex){
      val hit = sel === id
      s.d.valid := io.down.d.valid && hit
      s.d.payload.weakAssignFrom(io.down.d.payload)
      if(!s.p.withBCE) s.d.sink.removeAssignments() := 0
    }
  }

  val e = obp.withBCE generate new Area{
    val arbiter = StreamArbiterFactory().roundRobin.transactionLock.build(ChannelE(obp.toBusParameter()), upsNodes.filter(_.withBCE).size)
    (arbiter.io.inputs, ups.filter(_.p.withBCE)).zipped.foreach(_ << _.e)
    arbiter.io.output >> io.down.e
  }
}

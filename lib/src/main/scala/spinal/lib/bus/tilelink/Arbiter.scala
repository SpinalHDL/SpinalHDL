package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._

object Arbiter{
  def outputMastersFrom(inputs : Seq[M2sParameters]) : M2sParameters = {
    NodeParameters.mergeMasters(inputs)
  }
  def outputNodeFrom(inputs : Seq[NodeParameters]) : NodeParameters = {
    NodeParameters.mergeMasters(inputs)
  }
}

case class Arbiter(inputsNodes : Seq[NodeParameters]) extends Component{
  val obp = Arbiter.outputNodeFrom(inputsNodes)
  val io = new Bundle{
    val inputs = Vec(inputsNodes.map(e => slave(Bus(e))))
    val output = master(Bus(obp))
  }

  val sourceOffsetWidth = log2Up(inputsNodes.size)
  val inputs = io.inputs.zipWithIndex.map{case (bus, id) => bus.withSourceOffset(id << sourceOffsetWidth, obp.m.sourceWidth)}

  val a = new Area{
    val arbiter = StreamArbiterFactory().roundRobin.lambdaLock[ChannelA](_.isLast()).build(ChannelA(obp.toBusParameter()), inputsNodes.size)
    (arbiter.io.inputs, inputs).zipped.foreach(_ connectFromRelaxed _.a)
    arbiter.io.output >> io.output.a
  }

  val b = obp.withBCE generate new Area{
    val sel = io.output.d.source.takeHigh(sourceOffsetWidth).asUInt
    io.output.d.ready := inputs.map(e => if(e.p.withBCE) e.b.ready else False).read(sel)
    for((s, id) <- inputs.zipWithIndex if s.p.withBCE) {
      val hit = sel === id
      s.b.valid := io.output.b.valid && hit
      s.b.payload := io.output.b.payload
    }
  }

  val c = obp.withBCE generate new Area{
    val arbiter = StreamArbiterFactory().roundRobin.lambdaLock[ChannelC](_.isLast()).build(ChannelC(obp.toBusParameter()), inputsNodes.filter(_.withBCE).size)
    (arbiter.io.inputs, inputs.filter(_.p.withBCE)).zipped.foreach(_ << _.c)
    arbiter.io.output >> io.output.c
  }

  val d = new Area{
    val sel = io.output.d.source.takeHigh(sourceOffsetWidth).asUInt
    io.output.d.ready := inputs.map(_.d.ready).read(sel)
    for((s, id) <- inputs.zipWithIndex){
      val hit = sel === id
      s.d.valid := io.output.d.valid && hit
      s.d.payload := io.output.d.payload
    }
  }

  val e = obp.withBCE generate new Area{
    val arbiter = StreamArbiterFactory().roundRobin.transactionLock.build(ChannelE(obp.toBusParameter()), inputsNodes.filter(_.withBCE).size)
    (arbiter.io.inputs, inputs.filter(_.p.withBCE)).zipped.foreach(_ << _.e)
    arbiter.io.output >> io.output.e
  }
}

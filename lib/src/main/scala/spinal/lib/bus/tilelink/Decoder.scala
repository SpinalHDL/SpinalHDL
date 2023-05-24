package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping}

import scala.collection.mutable.ArrayBuffer

object Decoder{
  def inputNodeFrom(outputs : Seq[NodeParameters]) : NodeParameters = {
    NodeParameters.mergeNodes(outputs)
  }
  def inputSlavesFrom(outputs : Seq[S2mParameters]) : S2mParameters = {
    NodeParameters.mergeSlaves(outputs)
  }
  def outputMastersFrom(input : M2sParameters, output : M2sSupport) : M2sParameters = {
    input.copy(
      addressWidth = output.addressWidth,
      masters = input.masters.map(e =>
        e.copy(
          mapping = e.mapping.map( m =>
            m.copy(
              emits = m.emits.intersect(output.transfers)
            )
          )
        )
      )
    )
  }
}

case class Decoder(inputNode : NodeParameters, outputsSupports : Seq[M2sSupport], outputsS2m : Seq[S2mParameters], mapping : Seq[AddressMapping]) extends Component{
  val outputsNodes = (outputsSupports, outputsS2m). zipped.map((support, s2m) => inputNode.copy(
    m = Decoder.outputMastersFrom(inputNode.m, support),
    s = s2m
  ))

  val io = new Bundle{
    val input = slave(Bus(inputNode))
    val outputs = Vec(outputsNodes.map(e => master(Bus(e))))
  }

  val sinkOffsetWidth = if(outputsNodes.exists(_.withBCE)) log2Up(outputsNodes.size) else 0
  val outputs = io.outputs.zipWithIndex.map{case (bus, id) => bus.fromSinkOffset(id << sinkOffsetWidth, inputNode.s.sinkWidth)}

  val a = new Area{
    val readys = ArrayBuffer[Bool]()
    val logic = for((s, id) <- outputs.zipWithIndex) yield new Area {
      val hit = mapping(id).hit(io.input.a.address) && s.p.node.m.emits.contains(io.input.a.opcode)
      s.a.valid := io.input.a.valid && hit
      s.a.payload := io.input.a.payload
      s.a.address.removeAssignments() := io.input.a.address.resized
      readys += s.a.ready && hit
    }
    io.input.a.ready := readys.orR

    val miss = !logic.filter(_ != null).map(_.hit).orR
    assert(!(io.input.a.valid && miss))
  }

  val b = inputNode.withBCE generate new Area{
    val arbiter = StreamArbiterFactory().roundRobin.lambdaLock[ChannelB](_.isLast()).build(ChannelB(inputNode), outputsNodes.filter(_.withBCE).size)
    for(i <- 0 until outputsSupports.size if outputsNodes(i).withBCE){
      arbiter.io.inputs(i) << outputs(i).b

      val base = mapping(i) match{
        case DefaultMapping => BigInt(0)
        case v => v.lowerBound
      }
      arbiter.io.inputs(i).address.removeAssignments() := outputs(i).b.address.resize(inputNode.m.addressWidth) | base
    }
    arbiter.io.output >> io.input.b
  }

  val c = inputNode.withBCE generate new Area{
    val readys = ArrayBuffer[Bool]()
    val logic = for((s, id) <- outputs.zipWithIndex if s.p.withBCE) yield new Area {
      val hit = mapping(id).hit(io.input.c.address)
      s.c.valid := io.input.c.valid && hit
      s.c.payload := io.input.c.payload
      s.c.address.removeAssignments() := io.input.c.address.resized
      readys += s.a.ready && hit
    }
    io.input.c.ready := readys.orR
  }

  val d = new Area{
    val arbiter = StreamArbiterFactory().roundRobin.lambdaLock[ChannelD](_.isLast()).build(ChannelD(inputNode), outputsNodes.size)
    (arbiter.io.inputs, outputs).zipped.foreach(_ connectFromRelaxed _.d)
    arbiter.io.output >> io.input.d
  }

  val e = inputNode.withBCE generate new Area{
    val sel = io.input.d.sink.takeHigh(sinkOffsetWidth).asUInt
    io.input.e.ready := outputs.map(v => if(v.p.withBCE) v.e.ready else False).read(sel)
    for((s, id) <- outputs.zipWithIndex if s.p.withBCE) {
      val hit = sel === id
      s.e.valid := io.input.e.valid && hit
      s.e.payload := io.input.e.payload
    }
  }
}

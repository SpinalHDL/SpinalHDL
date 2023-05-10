package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.AddressMapping

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

case class Decoder(inputNode : NodeParameters, outputsSupports : Seq[M2sSupport], mapping : Seq[AddressMapping]) extends Component{
  val outputsNodes = outputsSupports.map(support => inputNode.copy(
    m = Decoder.outputMastersFrom(inputNode.m, support)
  ))

  val io = new Bundle{
    val input = slave(Bus(inputNode))
    val outputs = Vec(outputsNodes.map(e => master(Bus(e))))
  }

  val sinkOffsetWidth = if(outputsNodes.exists(_.withBCE)) log2Up(outputsNodes.size) else 0
  val outputs = io.outputs.zipWithIndex.map{case (bus, id) => bus.fromSinkOffset(id << sinkOffsetWidth, inputNode.s.sinkWidth)}

  val withError = true
  val error = withError generate new Area{
    val ctrl = new ErrorSlave(inputNode.toBusParameter().copy(withBCE = false))
  }

  val a = new Area{
    val readys = ArrayBuffer[Bool]()
    val logic = for((s, id) <- outputs.zipWithIndex) yield new Area {
      val hit = mapping(id).hit(io.input.a.address) && s.p.node.m.emits.contains(io.input.a.opcode)
      val instrFetch = (!outputsSupports(id).allowExecute && inputNode.m.masters.exists(_.mapping.exists(_.isExecute))) generate new Area{
        val sources = inputNode.m.masters.flatMap(_.mapping).filter(_.isExecute)
        val error = sources.map(_.id.hit(io.input.a.source)).orR
        hit.clearWhen(error)
      }
      s.a.valid := io.input.a.valid && hit
      s.a.payload := io.input.a.payload
      s.a.address.removeAssignments() := io.input.a.address.resized
      readys += s.a.ready && hit
    }

    val miss = !logic.filter(_ != null).map(_.hit).orR
    error.ctrl.io.bus.a.valid := io.input.a.valid && miss
    error.ctrl.io.bus.a.payload := io.input.a.payload
    readys += error.ctrl.io.bus.a.ready && miss

    io.input.a.ready := readys.orR
  }

  val b = inputNode.withBCE generate new Area{
    val arbiter = StreamArbiterFactory().roundRobin.lambdaLock[ChannelB](_.isLast()).build(ChannelB(inputNode), outputsNodes.filter(_.withBCE).size)
    (arbiter.io.inputs, outputs.filter(_.p.withBCE)).zipped.foreach(_ << _.b)
    arbiter.io.output >> io.input.b
  }

  val c = inputNode.withBCE generate new Area{
    val readys = ArrayBuffer[Bool]()
    val logic = for((s, id) <- outputs.zipWithIndex if s.p.withBCE) yield new Area {
      val hit = mapping(id).hit(io.input.c.address)
      s.c.valid := io.input.c.valid && hit
      s.c.payload := io.input.c.payload
      readys += s.a.ready && hit
    }
    io.input.c.ready := readys.orR
  }

  val d = new Area{
    val arbiter = StreamArbiterFactory().roundRobin.lambdaLock[ChannelD](_.isLast()).build(ChannelD(inputNode), outputsNodes.size + withError.toInt)
    (arbiter.io.inputs, outputs).zipped.foreach(_ connectFromRelaxed _.d)
    if(withError) arbiter.io.inputs.last << error.ctrl.io.bus.d
    arbiter.io.output >> io.input.d
  }

  val e = inputNode.withBCE generate new Area{
    val sel = io.input.d.sink.takeHigh(sinkOffsetWidth).asUInt
    io.input.d.ready := outputs.map(v => if(v.p.withBCE) v.e.ready else False).read(sel)
    for((s, id) <- outputs.zipWithIndex if s.p.withBCE) {
      val hit = sel === id
      s.e.valid := io.input.e.valid && hit
      s.e.payload := io.input.e.payload
    }
  }
}

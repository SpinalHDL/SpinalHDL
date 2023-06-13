package spinal.lib.bus.tilelink.sim

import spinal.core.sim._
import spinal.lib.bus.tilelink._

import scala.util.Random

case class MasterDebugTesterElement(m : MasterSpec, agent : MasterAgent)

class MasterDebugTester(masters : Seq[MasterDebugTesterElement]){
  val flatten = masters.flatMap(e => e.m.bus.p.node.m.masters.flatMap(e2 => e2.mapping.map(e -> _)))
  case class Ctx(val agent : MasterAgent, val source : Int, val address :  Long, val byte : Int)
  def find(sizes : M2sTransfers => SizeRange) : Ctx = {
    val master = flatten.filter(e => sizes(e._2.emits).some).randomPick()
    val mapping = master._1.m.mapping.filter(e => sizes(e.allowed).some).randomPick().mapping.randomPick()
    val source = master._2
    val sizeMax = mapping.size.toInt
    val bytes = sizes(source.emits).random(randMax = sizeMax)
    val addressLocal = bytes * Random.nextInt(sizeMax / bytes)
    val address = mapping.base.toLong + addressLocal
    Ctx(master._1.agent, source.id.randomPick().toInt, address.toLong, bytes)
  }

  def cover(sizes : M2sTransfers => SizeRange)(body : Ctx => Unit): Unit ={
    for((e, source) <- flatten if sizes(source.emits).some){
      for(slave <- e.m.mapping if sizes(slave.allowed).some){
        for(mapping <- slave.mapping) {
          val sizeMax = mapping.size.toInt
          val bytes = sizes(source.emits).random(randMax = sizeMax)
          val addressLocal = bytes * Random.nextInt(sizeMax / bytes)
          val address = mapping.base.toLong + addressLocal
          body(Ctx(e.agent, source.id.randomPick().toInt, address, bytes))
        }
      }
    }
  }

  def sourceOf(m : MasterDebugTesterElement, f : M2sTransfers => SizeRange) = masters.filter(e => f(e.agent.bus.p.node.m.emits).some).randomPick()

  def randomizedData(bytes : Int) = {
    val data = new Array[Byte](bytes)
    Random.nextBytes(data)
    data
  }
  def randomizedMask(bytes : Int) = {
    Array.fill[Boolean](bytes)(Random.nextBoolean())
  }

  def coverGet(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(_.get){ ctx =>
      ctx.agent.get(ctx.source, ctx.address, ctx.byte)
    }
  }

  def coverPutFullData(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(_.putFull){ ctx =>
      ctx.agent.putFullData(ctx.source, ctx.address, randomizedData(ctx.byte))
    }
  }

  def coverPutPartialData(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(_.putFull){ ctx =>
      ctx.agent.putPartialData(ctx.source, ctx.address, randomizedData(ctx.byte), randomizedMask(ctx.byte))
    }
  }
  def coverAcquireB(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(_.acquireB){ ctx =>
      val block = ctx.agent.acquireBlock(ctx.source, Param.Grow.NtoB, ctx.address, ctx.byte)
      ctx.agent.release(ctx.source, Param.Cap.toN, block)
    }
  }
  def coverAcquireT(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(_.acquireT){ ctx =>
      val block = ctx.agent.acquireBlock(ctx.source, Param.Grow.NtoT, ctx.address, ctx.byte)
      if(Random.nextBoolean()) block.makeDataDirty()
      ctx.agent.release(ctx.source, Param.Cap.toN, block)
    }
  }
  def coverAcquireBT(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(_.acquireT){ ctx =>
      var block = ctx.agent.acquireBlock(ctx.source, Param.Grow.NtoB, ctx.address, ctx.byte)
      if(block.cap == Param.Cap.toB) ctx.agent.acquireBlock(ctx.source, Param.Grow.BtoT, ctx.address, ctx.byte)
      if(Random.nextBoolean()) block.makeDataDirty()
      ctx.agent.release(ctx.source, Param.Cap.toN, block)
    }
  }
  def coverAcquireTB(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(_.acquireT){ ctx =>
      var block = ctx.agent.acquireBlock(ctx.source, Param.Grow.NtoT, ctx.address, ctx.byte)
      if(Random.nextBoolean()) block.makeDataDirty()
      ctx.agent.release(ctx.source, Param.Cap.toB, block)
      ctx.agent.release(ctx.source, Param.Cap.toN, block)
    }
  }
}

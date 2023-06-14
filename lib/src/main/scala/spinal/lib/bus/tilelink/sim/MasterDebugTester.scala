package spinal.lib.bus.tilelink.sim

import spinal.core.sim._
import spinal.lib.bus.tilelink.{M2sTransfers, _}

import scala.collection.mutable
import scala.util.Random

case class MasterDebugTesterElement(m : MasterSpec, agent : MasterAgent)

class MasterDebugTester(masters : Seq[MasterDebugTesterElement]){
  val flatten = masters.flatMap(e => e.m.bus.p.node.m.masters.flatMap(e2 => e2.mapping.map((e,_, e2))))
  case class Ctx(val agent : MasterAgent, val source : Int, val address :  Long, val bytes : Int, slave : Mapping, m2sAgent: M2sAgent){
    def check(): Unit ={
      if(slave.allowed.get.contains(bytes)){
        agent.get(source, address, bytes)
      } else if(slave.allowed.acquireB.contains(bytes)){
        val block = agent.acquireBlock(source, Param.Grow.NtoB, address, bytes)
        agent.release(source, Param.Cap.toN, block)
      }
    }
    def anotherMaster(sizes : M2sTransfers => SizeRange) : Ctx = {
      val candidates = flatten.filter(e => e._3 != m2sAgent && e._1.m.mapping.exists(_.model == slave.model) && sizes(e._2.emits).some)
      val other = candidates.randomPick()
      val source = other._2.id.randomPick().toInt
      Ctx(other._1.agent, source, address, bytes, slave, other._3)
    }
  }
//  def find(sizes : M2sTransfers => SizeRange) : Ctx = {
//    val master = flatten.filter(e => sizes(e._2.emits).some).randomPick()
//    val mapping = master._1.m.mapping.filter(e => sizes(e.allowed).some).randomPick().mapping.randomPick()
//    val source = master._2
//    val sizeMax = mapping.size.toInt
//    val bytes = sizes(source.emits).random(randMax = sizeMax)
//    val addressLocal = bytes * Random.nextInt(sizeMax / bytes)
//    val address = mapping.base.toLong + addressLocal
//    Ctx(master._1.agent, source.id.randomPick().toInt, address.toLong, bytes)
//  }

  val checks = mutable.LinkedHashSet[() => Unit]()
  def cover(sizes : M2sTransfers => SizeRange)(body : Ctx => Unit): Unit ={
    for((e, source, m2sAgent) <- flatten if sizes(source.emits).some){
      for(slave <- e.m.mapping if sizes(slave.allowed).some){
        for(mapping <- slave.mapping) {
          val sizeMax = mapping.size.toInt
          val bytes = sizes(source.emits).random(randMax = sizeMax)
          val addressLocal = bytes * Random.nextInt(sizeMax / bytes)
          val address = mapping.base.toLong + addressLocal
          val sourceId = source.id.randomPick().toInt
          body(Ctx(e.agent, sourceId, address, bytes, slave, m2sAgent))
        }
      }
    }
  }

  def sourceOf(m : MasterDebugTesterElement, f : M2sTransfers => SizeRange) = masters.filter(e => f(e.agent.bus.p.node.m.emits).some).randomPick()

  def BT(e : M2sTransfers) = e.acquireT intersect e.acquireB

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
      ctx.agent.get(ctx.source, ctx.address, ctx.bytes)
    }
  }

  def coverPutFullData(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(_.putFull){ ctx =>
      ctx.agent.putFullData(ctx.source, ctx.address, randomizedData(ctx.bytes))
      ctx.check()
    }
  }

  def coverPutPartialData(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(_.putFull){ ctx =>
      ctx.agent.putPartialData(ctx.source, ctx.address, randomizedData(ctx.bytes), randomizedMask(ctx.bytes))
      ctx.check()
    }
  }
  def coverAcquireB(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(_.acquireB){ ctx =>
      val block = ctx.agent.acquireBlock(ctx.source, Param.Grow.NtoB, ctx.address, ctx.bytes)
      ctx.agent.release(ctx.source, Param.Cap.toN, block)
    }
  }
  def coverAcquireT(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(_.acquireT){ ctx =>
      val block = ctx.agent.acquireBlock(ctx.source, Param.Grow.NtoT, ctx.address, ctx.bytes)
      if(Random.nextBoolean()) block.makeDataDirty()
      ctx.agent.releaseAuto(ctx.source, Param.Cap.toN, block)
      ctx.check()
    }
  }
  def coverAcquireBT(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(BT){ ctx =>
      var block = ctx.agent.acquireBlock(ctx.source, Param.Grow.NtoB, ctx.address, ctx.bytes)
      if(block.cap == Param.Cap.toB) ctx.agent.acquireBlock(ctx.source, Param.Grow.BtoT, ctx.address, ctx.bytes)
      if(Random.nextBoolean()) block.makeDataDirty()
      ctx.agent.releaseAuto(ctx.source, Param.Cap.toN, block)
      ctx.check()
    }
  }
  def coverAcquireTB(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(BT){ ctx =>
      var block = ctx.agent.acquireBlock(ctx.source, Param.Grow.NtoT, ctx.address, ctx.bytes)
      if(Random.nextBoolean()) block.makeDataDirty()
      ctx.agent.releaseAuto(ctx.source, Param.Cap.toB, block)
      ctx.agent.release(ctx.source, Param.Cap.toN, block)
      ctx.check()
    }
  }
  def coverAcquirePerm(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(_.acquireT){ ctx =>
      var block = ctx.agent.acquirePerm(ctx.source, Param.Grow.NtoT, ctx.address, ctx.bytes)
      block.dirty = true
      block.data = new Array[Byte](block.bytes)
      Random.nextBytes(block.data)
      ctx.agent.releaseData(ctx.source, Param.Cap.toN, block)
      ctx.check()
    }
  }

  def coverCoherencyBx2(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(_.acquireB){ ctx1 =>
      val ctx2 = ctx1.anotherMaster(_.acquireB)
      var block1 = ctx1.agent.acquireBlock(ctx1.source, Param.Grow.NtoB, ctx1.address, ctx1.bytes)
      var block2 = ctx2.agent.acquireBlock(ctx2.source, Param.Grow.NtoB, ctx2.address, ctx2.bytes)
      ctx1.agent.release(ctx1.source, Param.Cap.toN, block1)
      ctx2.agent.release(ctx2.source, Param.Cap.toN, block2)
    }
  }

  def coverCoherencyTx2(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(_.acquireT){ ctx1 =>
      val ctx2 = ctx1.anotherMaster(_.acquireT)
      var block1 = ctx1.agent.acquireBlock(ctx1.source, Param.Grow.NtoT, ctx1.address, ctx1.bytes)
      block1.makeDataDirty()
      var block2 = ctx2.agent.acquireBlock(ctx2.source, Param.Grow.NtoT, ctx2.address, ctx2.bytes)
      assert(block1.cap == Param.Cap.toN)
      assert(block2.data sameElements block1.data)
      ctx2.agent.release(ctx2.source, Param.Cap.toN, block2)
    }
  }

  def coverCoherencyT_B(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(BT){ ctx1 =>
      val ctx2 = ctx1.anotherMaster(BT)
      var block1 = ctx1.agent.acquireBlock(ctx1.source, Param.Grow.NtoT, ctx1.address, ctx1.bytes)
      block1.makeDataDirty()
      var block2 = ctx2.agent.acquireBlock(ctx2.source, Param.Grow.NtoB, ctx2.address, ctx2.bytes)
      assert(block1.cap == Param.Cap.toB)
      assert(block2.data sameElements block1.data)
      ctx1.agent.release(ctx1.source, Param.Cap.toN, block1)
      ctx2.agent.release(ctx2.source, Param.Cap.toN, block2)
    }
  }

  def coverCoherencyBx2_T_Bx2(repeat : Int): Unit ={
    for(i <- 0 until repeat) cover(BT){ ctx1 =>
      val ctx2 = ctx1.anotherMaster(BT)
      var block1 = ctx1.agent.acquireBlock(ctx1.source, Param.Grow.NtoB, ctx1.address, ctx1.bytes)
      var block2 = ctx2.agent.acquireBlock(ctx2.source, Param.Grow.NtoB, ctx2.address, ctx2.bytes)
      assert(block2.data sameElements block1.data)
      block1 = ctx1.agent.acquireBlock(ctx1.source, Param.Grow.BtoT, ctx1.address, ctx1.bytes)
      block1.makeDataDirty()
      block2 = ctx2.agent.acquireBlock(ctx2.source, Param.Grow.NtoB, ctx2.address, ctx2.bytes)
      assert(block1.cap == Param.Cap.toB)
      assert(block2.cap == Param.Cap.toB)
      assert(block2.data sameElements block1.data)
      ctx1.agent.release(ctx1.source, Param.Cap.toN, block1)
      ctx2.agent.release(ctx2.source, Param.Cap.toN, block2)
    }
  }
}

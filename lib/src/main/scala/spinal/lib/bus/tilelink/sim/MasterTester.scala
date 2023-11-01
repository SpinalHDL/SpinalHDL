package spinal.lib.bus.tilelink.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.misc.{AddressMapping, AddressTransformer, OffsetTransformer, SizeMapping}
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.fabric.TransferFilterTag
import spinal.lib.sim.SparseMemory
import spinal.sim.SimThread

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.Seq

object Chunk{
  def apply(allowed: M2sTransfers,
            mapping: AddressMapping,
            offset : BigInt): Chunk ={
    Chunk(allowed, mapping, List(new OffsetTransformer(offset)))
  }
}
case class Chunk(allowed: M2sTransfers,
                 mapping: AddressMapping,
                 transforms: List[AddressTransformer]) {
  override def toString = f"$allowed $mapping"
  def globalToLocal(address : BigInt) : BigInt = transforms.foldLeft(address)((a, t) => t(a))
}

case class Endpoint(model : Any,
                    chunks : Seq[Chunk])
case class MasterSpec(bus : Bus,
                      cd : ClockDomain,
                      endpoints : Seq[Endpoint])

class MasterTester(val m : MasterSpec, val agent : MasterAgent){
  val threads = ArrayBuffer[SimThread]()
  def join() = {
    threads.foreach(_.join())
    threads.clear()
  }

  val node = m.bus.p.node

  def randomizedData(bytes : Int) = {
    val data = new Array[Byte](bytes)
    simRandom.nextBytes(data)
    data
  }
  def randomizedMask(bytes : Int) = {
    Array.fill[Boolean](bytes)(simRandom.nextBoolean())
  }

  def startPerSource(perSourceBurst : Int, globalLock : Option[SimMutex] = None) {
    for (masterParam <- node.m.masters) {
      val locks = mutable.HashMap[Long, SimMutex]()
      var randomAddress = 0l
      def lock(address : Long) = {
        val l = locks.getOrElseUpdate(address, new SimMutex(randomized = true))
        if(simRandom.nextBoolean()) randomAddress = address
        l.lock()
        globalLock.foreach(_.lock())
      }
      def unlock(address : Long) = {
        val l = locks(address)
        l.unlock()
        globalLock.foreach(_.unlock())
        if(!l.locked) locks.remove(address)
      }

      for (source <- masterParam.mapping) {
        source.id.foreach { sourceIdBI =>
          val sourceId = sourceIdBI.toInt
          threads += fork {
            Thread.currentThread().setName(s"MasterTester_source_$sourceId" )
            val distribution = new WeightedDistribution[Unit]()

            def randomized(mappings : Seq[Chunk], sizes : M2sTransfers => SizeRange): (Long, Int) = {
              val s = mappings.randomPick()
              randomizedImpl(s, sizes)
            }

            def randomizedImpl(chunk : Chunk, sizes : M2sTransfers => SizeRange): (Long, Int) ={
              val sizeMax = chunk.mapping.maxSequentialSize.toInt
              val bytes = sizes(source.emits).intersect(sizes(chunk.allowed)).random(randMax = sizeMax)
              val address = chunk.mapping.randomPick(bytes, true).toLong
              (address, bytes)
            }

            def add(filter : M2sTransfers => SizeRange, weight : Int = 10)(body : (Long, Int) => Unit): Unit ={
              val filtred = m.endpoints.flatMap(_.chunks).filter(e => filter(e.allowed).some)
              if(filtred.nonEmpty) distribution(weight) {
                val (address, bytes) = randomized(filtred, filter)
                lock(address)
                body(address, bytes)
                unlock(address)
              }
            }

            add(_.get){(address, bytes) =>
              agent.get(sourceId, address, bytes)
            }
            add(_.putPartial){(address, bytes) =>
              agent.putPartialData(sourceId, address, randomizedData(bytes), randomizedMask(bytes))
            }
            add(_.putFull){(address, bytes) =>
              agent.putFullData(sourceId, address, randomizedData(bytes))
            }
            // acquireBlock NtoB
            add(_.acquireB){(address, bytes) =>
              val b = agent.block.get(sourceId, address) match {
                case Some(x : Block) => x
                case None => agent.acquireBlock(sourceId, Param.Grow.NtoB, address, bytes)
              }
              assert(b.cap < Param.Cap.toN)
            }
            // acquireBlock NtoT
            add(_.acquireT){(address, bytes) =>
              var b = agent.block.get(sourceId, address) match {
                case Some(x : Block) => x
                case None => agent.acquireBlock(sourceId, Param.Grow.NtoT, address, bytes)
              }
              assert(b.cap < Param.Cap.toN)
              if(b.cap > Param.Cap.toT){
                b = agent.acquireBlock(sourceId, Param.Grow.BtoT, address, bytes)
              }
              assert(b.cap == Param.Cap.toT, f"$source $address%x")
              if(simRandom.nextFloat() < 0.8) {
                b.dirty = true
                for (i <- 0 until bytes if simRandom.nextBoolean()) b.data(i) = simRandom.nextInt().toByte
              }
            }
            // acquirePerm XtoT
            add(_.acquireT, 5){(address, bytes) =>
              var b = agent.block.get(sourceId, address) match {
                case Some(x : Block) => x
                case None => agent.acquirePerm(sourceId, Param.Grow.NtoT, address, bytes)
              }
              assert(b.cap < Param.Cap.toN)
              if(b.cap > Param.Cap.toT){
                b = agent.acquirePerm(sourceId, Param.Grow.BtoT, address, bytes)
              }
              assert(b.cap == Param.Cap.toT, f"$source $address%x")
              b.dirty = true
              b.data = new Array[Byte](bytes)
              simRandom.nextBytes(b.data)
            }
            // release + releaseData
            if(masterParam.emits.withBCE) distribution(3) {
              val block = agent.block.getRandomBlock(masterParam)
              if(block != null){
                lock(block.address)
                block.cap  match {
                  case Param.Cap.toN =>
                  case Param.Cap.toB => agent.release(sourceId, Param.Cap.toN, block)
                  case Param.Cap.toT => {
                    val cap = if(simRandom.nextBoolean()) Param.Cap.toB else Param.Cap.toN
                    block.dirty match {
                      case true  => agent.releaseData(sourceId, cap, block);
                      case false => agent.release(sourceId, cap, block)
                    }
                    block.dirty = false
                  }
                }
                unlock(block.address)
              }
            }

            val deadEnds = m.endpoints.filter(_.model == TransferFilterTag)
            if(deadEnds.nonEmpty) {
              def deniedOn(kind : M2sTransfers => SizeRange)(body : (Long, Int) => Unit): Unit = {
                if(kind(source.emits).some) distribution(1) {
                  val deadEnd = deadEnds.randomPick()
                  val chunk = deadEnd.chunks.randomPick()
                  val sizes = kind(source.emits)
                  val (address, bytes) = randomizedImpl(chunk, _ => sizes)
                  val badAddress = m.endpoints.exists(e => e.model != TransferFilterTag && e.chunks.exists(c => kind(c.allowed).some && c.mapping.hit(address)))
                  if (!badAddress) {
                    body(address, bytes)
                  }
                }
              }

              deniedOn(_.get){(address, bytes) =>
                val d = agent.get(sourceId, address, bytes)
                assert(d.denied)
              }
              deniedOn(_.putPartial){(address, bytes) =>
                val d = agent.putPartialData(sourceId, address, randomizedData(bytes), randomizedMask(bytes))
                assert(d.denied)
              }
              deniedOn(_.putFull){(address, bytes) =>
                val d = agent.putFullData(sourceId, address, randomizedData(bytes))
                assert(d.denied)
              }
              deniedOn(_.acquireB){(address, bytes) =>
                val b = agent.block.get(sourceId, address) match {
                  case Some(x : Block) => x
                  case None => agent.acquireBlock(sourceId, Param.Grow.NtoB, address, bytes)
                }
                assert(b.cap == Param.Cap.toN)
              }
              // acquireBlock NtoT
              deniedOn(_.acquireT) { (address, bytes) =>
                val b = agent.block.get(sourceId, address) match {
                  case Some(x: Block) => x
                  case None => agent.acquireBlock(sourceId, Param.Grow.NtoT, address, bytes)
                }
                assert(b.cap == Param.Cap.toN)
              }
            }


            for (i <- 0 until perSourceBurst) {
              distribution.randomExecute()
            }
            Thread.currentThread().setName(s"???" )
          }
        }
      }
    }
  }
}

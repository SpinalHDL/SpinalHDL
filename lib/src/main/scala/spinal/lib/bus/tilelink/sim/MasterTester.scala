package spinal.lib.bus.tilelink.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink._
import spinal.lib.sim.SparseMemory
import spinal.sim.SimThread

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


case class Chunk(allowed : M2sTransfers,
                 offset : BigInt,
                 mapping : Seq[SizeMapping])
case class Endpoint(model : Any,
                    chunks : Seq[Chunk])
case class MasterSpec(bus : Bus,
                      cd : ClockDomain,
                      endpoints : Seq[Endpoint])

class MasterTester(m : MasterSpec , agent : MasterAgent){
  val threads = ArrayBuffer[SimThread]()
  def join() = {
    threads.foreach(_.join())
    threads.clear()
  }

  val node = m.bus.p.node

  def randomizedData(bytes : Int) = {
    val data = new Array[Byte](bytes)
    Random.nextBytes(data)
    data
  }
  def randomizedMask(bytes : Int) = {
    Array.fill[Boolean](bytes)(Random.nextBoolean())
  }

  def startPerSource(perSourceBurst : Int) {
    for (masterParam <- node.m.masters) {

      val locks = mutable.HashMap[Long, SimMutex]()
      var randomAddress = 0l
      def lock(address : Long) = {
        val l = locks.getOrElseUpdate(address, new SimMutex(randomized = true))
        if(Random.nextBoolean()) randomAddress = address
        l.lock()
      }
      def unlock(address : Long) = {
        val l = locks(address)
        l.unlock()
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
              val mapping = s.mapping.randomPick()
              randomizedImpl(mapping, sizes)
            }

            def randomizedImpl(mapping : SizeMapping, sizes : M2sTransfers => SizeRange): (Long, Int) ={
              val sizeMax = mapping.size.toInt
              val bytes = sizes(source.emits).random(randMax = sizeMax)
              val addressLocal = bytes * Random.nextInt(sizeMax / bytes)
              val address = mapping.base.toLong + addressLocal
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
              if(Random.nextFloat() < 0.8) {
                b.dirty = true
                for (i <- 0 until bytes if Random.nextBoolean()) b.data(i) = Random.nextInt().toByte
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
              Random.nextBytes(b.data)
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
                    val cap = if(Random.nextBoolean()) Param.Cap.toB else Param.Cap.toN
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

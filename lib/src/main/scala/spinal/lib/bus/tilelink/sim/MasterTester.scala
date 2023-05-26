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


case class Mapping(allowed : M2sSupport,
                   mapping : Seq[SizeMapping],
                   model : Any)
case class MasterSpec(bus : Bus,
                      cd : ClockDomain,
                      mapping : Seq[Mapping])

class MasterTester(m : MasterSpec , agent : MasterAgent){
  val threads = ArrayBuffer[SimThread]()
  def join() = {
    threads.foreach(_.join())
    threads.clear()
  }

  val node = m.bus.p.node

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
        val l = locks.getOrElseUpdate(address, new SimMutex(randomized = true))
        l.unlock()
      }

      for (source <- masterParam.mapping) {
        source.id.foreach { sourceIdBI =>
          val sourceId = sourceIdBI.toInt
          threads += fork {
            val distribution = new WeightedDistribution[Unit]()

//            def releaseBlockIfExists(address : Long): Unit ={
//              lock(address)
//              agent.block.get(source, address) match {
//                case Some(block) => {
//                  block.dirty match {
//                    case false => release(agent,source,Param.Cap.toN, block)
//                    case true  => releaseData(agent,source,Param.Cap.toN, block)
//                  }
//                }
//                case None =>
//              }
//              unlock(address)
//            }

            def acquireBlock(param : Int,
                             address : Long,
                             bytes : Int,
                             mem : SparseMemory,
                             memAddress : Long): Block ={
              var ref: Array[Byte] = null
              val block = agent.acquireBlock(sourceId, param, address, bytes){ args =>
                ref = mem.readBytes(args.address.toLong, args.bytes)
              }
              block.ordering(args => mem.write(address-memAddress, block.data))
              //      println(f"* $address%x $source")
              //      println(toHex(block.data))
              //      println(toHex(ref))
              assert((block.data, ref).zipped.forall(_ == _))
              block
            }


            val slavesWithGet = m.mapping.filter(_.allowed.transfers.get.some)
            if(slavesWithGet.nonEmpty) distribution(10) {
              val s = slavesWithGet.randomPick()
              val mapping = s.mapping.randomPick()
              val sizeMax = mapping.size.toInt
              val bytes = source.emits.get.random(randMax = sizeMax)
              val addressLocal = bytes * Random.nextInt(sizeMax / bytes)
              val address = mapping.base.toLong + addressLocal
              val gMem = s.model.asInstanceOf[SparseMemory]
              var ref = new Array[Byte](bytes)

              val orderingCompletion = new OrderingCtrl(bytes)
              val data = agent.get(sourceId, address, bytes) { args =>
                gMem.readBytes(args.address toLong, args.bytes, ref, args.address - addressLocal toInt)
                orderingCompletion -= (args.address - addressLocal toInt, args.bytes)
              }
              assert(orderingCompletion.empty)
              assert((data, ref).zipped.forall(_ == _))
            }

            val slavesWithPutPartial = m.mapping.filter(_.allowed.transfers.putPartial.some)
            if(slavesWithPutPartial.nonEmpty) distribution(10) {
              val s = slavesWithPutPartial.randomPick()
              val mapping = s.mapping.randomPick()
              val sizeMax = mapping.size.toInt
              val bytes = source.emits.putPartial.random(randMax = sizeMax)
              val addressLocal = bytes * Random.nextInt(sizeMax / bytes)
              val address = mapping.base.toLong + addressLocal
              val data = Array.fill[Byte](bytes)(Random.nextInt().toByte)
              val mask = Array.fill[Boolean](bytes)(Random.nextInt(2).toBoolean)
              val gMem = s.model.asInstanceOf[SparseMemory]
              val orderingCompletion = new OrderingCtrl(bytes)
              assert(!agent.putPartialData(sourceId, address, data, mask) { args =>
                gMem.write(args.address toLong, data, mask, args.bytes, args.address - addressLocal toInt)
                orderingCompletion -= (args.address - addressLocal toInt, args.bytes)
              })
              assert(orderingCompletion.empty)
            }


            //Read block
            val slavesWithAcquireB = m.mapping.filter(_.allowed.transfers.acquireB.some)
            if(slavesWithAcquireB.nonEmpty) {
              val s = slavesWithAcquireB.randomPick()
              val mapping = s.mapping.randomPick()
              val sizeMax = mapping.size.toInt
              val bytes = source.emits.acquireB.random(randMax = sizeMax)
              val addressLocal = bytes * Random.nextInt(sizeMax / bytes)
              val address = mapping.base.toLong + addressLocal
              val gMem = s.model.asInstanceOf[SparseMemory]
              lock(address)
              var b : Block = null
              agent.block.get(sourceId, address) match {
                case Some(x) => b = x
                case None => {
                  b = acquireBlock(Param.Grow.NtoB, address, bytes, gMem, mapping.base.toLong)
                }
              }
              assert(b.cap < Param.Cap.toN)
              if(b.dirty == false) assert((b.data, gMem.readBytes(addressLocal, bytes)).zipped.forall(_ == _))
              unlock(address)
            }

            for (i <- 0 until perSourceBurst) {
              distribution.randomExecute()
            }
          }
        }
      }
    }
  }
}

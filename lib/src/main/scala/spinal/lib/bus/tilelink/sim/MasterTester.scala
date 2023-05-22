package spinal.lib.bus.tilelink.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink._
import spinal.lib.sim.SparseMemory
import spinal.sim.SimThread

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


case class Mapping(allowed : M2sSupport,
                   mapping : SizeMapping,
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
      for (source <- masterParam.mapping) {
        source.id.foreach { sourceIdBI =>
          val sourceId = sourceIdBI.toInt
          threads += fork {
            val distribution = new WeightedDistribution[Unit]()
            val slavesWithGet = m.mapping.filter(_.allowed.transfers.get.some)
            distribution(10) {
              val s = slavesWithGet.randomPick()
              val sizeMax = s.mapping.size.toInt
              val bytes = source.emits.get.random(randMax = sizeMax)
              val addressLocal = bytes * Random.nextInt(sizeMax / bytes)
              val address = s.mapping.base.toLong + addressLocal
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
            distribution(10) {
              val s = slavesWithPutPartial.randomPick()
              val sizeMax = s.mapping.size.toInt
              val bytes = source.emits.putPartial.random(randMax = sizeMax)
              val addressLocal = bytes * Random.nextInt(sizeMax / bytes)
              val address = s.mapping.base.toLong + addressLocal
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


            for (i <- 0 until perSourceBurst) {
              distribution.randomExecute()
            }
          }
        }
      }
    }
  }
}

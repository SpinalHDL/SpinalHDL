package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
import spinal.lib.misc.plic.{PlicGatewayActiveHigh, PlicMapper, PlicMapping, PlicTarget}
import spinal.lib.sim._
import spinal.sim._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class SpinalSimPlicTester extends SpinalSimFunSuite {
  test("test1") {
    //Compile the simulator

    val idMapping = ArrayBuffer[Int]() // List(2,4,32,6, 9,63,11,78)
    while (idMapping.size < 35) {
      val rand = Random.nextInt(128) + 1
      if (!idMapping.contains(rand)) idMapping += rand
    }
    val sourceCount = idMapping.length
    val targetCount = 3
    val priorityWidth = 2
    val plicMapping = PlicMapping.sifive
    import plicMapping._

    val compiled = SimConfig.compile(
      rtl = new Component {
        val io = new Bundle {
          val apb = slave(Apb3(24, 32))
          val sources = in Bits (sourceCount bits)
          val targets = out Bits (targetCount bits)
        }

        val gateways = (for ((source, id) <- (io.sources.asBools, idMapping).zipped) yield PlicGatewayActiveHigh(
          source = source,
          id = id,
          priorityWidth = priorityWidth
        )).toSeq

        val targets = for (i <- 0 until targetCount) yield PlicTarget(
          id = i,
          gateways = gateways,
          priorityWidth = priorityWidth
        )

        io.targets := targets.map(_.iep).asBits

        val bus = Apb3SlaveFactory(io.apb)
        val mapping = PlicMapper(bus, plicMapping)(
          gateways = gateways,
          targets = targets
        )
      }.setDefinitionName("Plic")
    )
    println("PLIC Compiled")
    //Run the simulation
    compiled.doSim("test") { dut =>
      println("PLIC SIM start")
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.forkSimSpeedPrinter()

      val apb = Apb3Driver(dut.io.apb, dut.clockDomain)

      dut.io.sources #= 0

      dut.clockDomain.waitSampling(10)
      assert(dut.io.targets.toInt === 0)
      var sourcesPending = Array.fill(sourceCount)(false)
      for (repeat <- 0 until (2000 * durationFactor).toInt) {
        val priorities = Array.fill(sourceCount)(Random.nextInt(4))
        val enables = Array.fill(targetCount)(Array.fill(sourceCount)(Random.nextBoolean()))
        val thresholds = Array.fill(targetCount)(Random.nextInt(priorityWidth))

        (0 until sourceCount).foreach { gatewayIdx =>
          val gatewayId = idMapping(gatewayIdx)
          apb.write(gatewayPriorityOffset + 4 * gatewayId, priorities(gatewayIdx))
        }
        (0 until targetCount).foreach { targetId =>
          apb.write(targetThresholdOffset + (targetId << targetThresholdShift), thresholds(targetId))
          (0 until sourceCount).foreach { gatewayIdx =>
            val gatewayId = idMapping(gatewayIdx)
            val mask = 1l << (gatewayId % 32)
            val address = targetEnableOffset + (targetId << targetEnableShift) + 4 * (gatewayId / 32)
            val old = apb.read(address)
            apb.write(address, (old & ~mask) | (if (enables(targetId)(gatewayIdx)) mask else 0))
          }
        }
        for (repeat2 <- 0 until Random.nextInt(6)) {
          //          for (i <- 0 until Random.nextInt(4) * Random.nextInt(4) / 2) {
          val sourceId = Random.nextInt(sourceCount)
          sourcesPending(sourceId) = true
          dut.io.sources #= (BigInt(1) << sourceId)
          //          }
          dut.clockDomain.waitSampling()
        }
        dut.io.sources #= 0

        dut.clockDomain.waitSampling(10)

        val claims = ArrayBuffer[(Int, Int)]()
        var continue = true
        while (continue) {
          continue = false
          (0 until targetCount).foreach { targetId =>
            val sourceId = (0 to sourceCount, idMapping).zipped.toSeq.sortBy(_._2).map(_._1).reduceLeft { (l, r) =>
              if (!sourcesPending(r) || !enables(targetId)(r) || (sourcesPending(l) && enables(targetId)(l) && priorities(l) >= priorities(r))) {
                l
              } else {
                r
              }
            }
            //            println("*****")
            //            println("pendig=" + sourcesPending.mkString(","))
            //            println("priori=" + priorities.mkString(","))
            //            println("enable=" + enables(targetId).mkString(","))
            //            println("thresh=" + thresholds(targetId))

            val interrupt = sourcesPending(sourceId) && enables(targetId)(sourceId) && priorities(sourceId) > thresholds(targetId)
            continue |= interrupt

            val expectedClaim = if (interrupt) idMapping(sourceId) else 0
            //            println("claim=" + expectedClaim)
            if (interrupt) {
              claims += (targetId -> expectedClaim)
              sourcesPending(sourceId) = false
            }
            val claimResult = apb.read(targetClaimOffset + (targetId << targetClaimShift))
            assert(claimResult == expectedClaim)
            assert(dut.io.targets.toBigInt.testBit(targetId) == interrupt)
            (0 until sourceCount).foreach { sourceId =>
              if (Random.nextFloat() < 0.1) {
                val gatwayId = idMapping(sourceId)
                val isPending = ((apb.read(gatewayPendingOffset + (gatwayId/32*4)) >> (gatwayId % 32)) & 1) != 0
                assert(isPending == sourcesPending(sourceId))
              }
            }
          }
        }
        claims.foreach(claim => apb.write(targetClaimOffset + (claim._1 << targetClaimShift), claim._2))
        dut.clockDomain.waitSampling(10)
      }
      println(simTime())
    }

  }
}

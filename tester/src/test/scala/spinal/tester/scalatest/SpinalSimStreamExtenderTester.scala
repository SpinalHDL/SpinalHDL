package spinal.tester.scalatest

import scala.collection.mutable
import scala.util.Random
import org.scalatest.funsuite.AnyFunSuite

import spinal.core._
import spinal.lib._
import spinal.lib.sim._
import spinal.core.sim._

class SpinalSimStreamExtenderTester extends SpinalSimFunSuite {
    def prepare(
        dut: StreamExtender[UInt, UInt],
        alwaysInput: Boolean = false,
        alwaysOutput: Boolean = false,
        inQueue: mutable.Queue[BigInt],
        outQueue: mutable.Queue[BigInt],
        countQueue: mutable.Queue[BigInt]
    ) {
        dut.clockDomain.forkStimulus(period = 10)
        dut.io.input.valid #= false

        if (alwaysOutput) {
            dut.io.output.ready #= true
        } else {
            val randomReady = StreamReadyRandomizer(dut.io.output, dut.clockDomain)
        }

        val driver = StreamDriver(dut.io.input, dut.clockDomain) { payload =>
            if (inQueue.nonEmpty) {
                payload #= inQueue.dequeue()
                dut.io.count #= countQueue.dequeue()
                true
            } else {
                false
            }
        }
        driver.transactionDelay = () => {
            if (!alwaysInput) {
                val x = Random.nextDouble()
                (x * x * 10).toInt
            } else {
                0
            }
        }

        val monitor = StreamMonitor(dut.io.output, dut.clockDomain) { payload =>
            {
                val address = outQueue.dequeue()
                // println("##pop out queue:" + address.toString(16))
                assert(payload.toBigInt == address)
            }
        }

        for (j <- 0 until 10000) {
            val count   = Random.nextInt(20)
            val address = Random.nextInt(0xffffff)

            inQueue.enqueue(address)
            countQueue.enqueue(count)

            for (i <- 0 to count) {
                outQueue.enqueue(address)
            }
        }
    }

    test("testRandomInOut") {
        val compiled = SimConfig.allOptimisation.compile {
            val dut = new StreamExtender(UInt(32 bits), UInt(32 bits), 12, (id, payload: UInt) => payload)
            dut
        }
        compiled.doSimUntilVoid { dut =>
            val inQueue    = mutable.Queue[BigInt]()
            val outQueue   = mutable.Queue[BigInt]()
            val countQueue = mutable.Queue[BigInt]()
            prepare(dut, false, false, inQueue, outQueue, countQueue)
            dut.clockDomain.waitSampling((1 KiB).toInt)
            simSuccess()
        }
    }

    test("testRandomIn") {
        val compiled = SimConfig.allOptimisation.compile {
            val dut = new StreamExtender(UInt(32 bits), UInt(32 bits), 12, (id, payload: UInt) => payload)
            dut
        }
        compiled.doSimUntilVoid { dut =>
            val inQueue    = mutable.Queue[BigInt]()
            val outQueue   = mutable.Queue[BigInt]()
            val countQueue = mutable.Queue[BigInt]()
            prepare(dut, false, true, inQueue, outQueue, countQueue)
            dut.clockDomain.waitSampling((1 KiB).toInt)
            simSuccess()
        }
    }

    test("testRandomOut") {
        val compiled = SimConfig.allOptimisation.compile {
            val dut = new StreamExtender(UInt(32 bits), UInt(32 bits), 12, (id, payload: UInt) => payload)
            dut
        }
        compiled.doSimUntilVoid { dut =>
            val inQueue    = mutable.Queue[BigInt]()
            val outQueue   = mutable.Queue[BigInt]()
            val countQueue = mutable.Queue[BigInt]()
            prepare(dut, true, false, inQueue, outQueue, countQueue)
            dut.clockDomain.waitSampling((1 KiB).toInt)
            simSuccess()
        }
    }

    test("testFullPipeline") {
        val compiled = SimConfig.allOptimisation.compile {
            val dut = new StreamExtender(UInt(32 bits), UInt(32 bits), 12, (id, payload: UInt) => payload)
            dut
        }
        compiled.doSimUntilVoid { dut =>
            val inQueue    = mutable.Queue[BigInt]()
            val outQueue   = mutable.Queue[BigInt]()
            val countQueue = mutable.Queue[BigInt]()
            prepare(dut, true, true, inQueue, outQueue, countQueue)
            dut.clockDomain.waitSampling((1 KiB).toInt)
            simSuccess()
        }
    }
}

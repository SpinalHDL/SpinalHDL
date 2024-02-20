package spinal.lib

import scala.collection.mutable
import scala.util.Random

import spinal.core._
import spinal.lib.sim._
import spinal.core.sim._
import spinal.tester.SpinalSimFunSuite

class SpinalSimStreamExtenderTester extends SpinalSimFunSuite {
    def prepare(
        dut: StreamTransactionExtender[UInt, UInt],
        alwaysInput: Boolean = false,
        alwaysOutput: Boolean = false,
        inQueue: mutable.Queue[BigInt],
        outQueue: mutable.Queue[BigInt],
        countQueue: mutable.Queue[BigInt],
        lastQueue: mutable.Queue[Boolean]
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
                val last    = lastQueue.dequeue()
                // println("##pop out queue:" + address.toString(16))
                val data = payload.toBigInt
                assert((data >> 1) == address)
                assert(((data & 1) == 1) == last)
            }
        }

        for (j <- 0 until 10000) {
            val count   = Random.nextInt(20)
            val address = Random.nextInt(0xffffff)

            inQueue.enqueue(address)
            countQueue.enqueue(count)

            for (i <- 0 to count) {
                outQueue.enqueue(address)
                lastQueue.enqueue(i == count)
            }
        }
    }

    test("testRandomInOut") {
        val lastQueue = mutable.Queue[Boolean]()
        val compiled = SimConfig.allOptimisation.compile {
            val dut = new StreamTransactionExtender(
                UInt(32 bits),
                UInt(33 bits),
                12,
                false,
                (id, payload: UInt, last) => payload @@ last
            )
            dut
        }
        compiled.doSimUntilVoid { dut =>
            val inQueue    = mutable.Queue[BigInt]()
            val outQueue   = mutable.Queue[BigInt]()
            val countQueue = mutable.Queue[BigInt]()
            prepare(dut, false, false, inQueue, outQueue, countQueue, lastQueue)
            dut.clockDomain.waitSampling((1 KiB).toInt)
            simSuccess()
        }
    }

    test("testRandomIn") {
        val lastQueue = mutable.Queue[Boolean]()
        val compiled = SimConfig.allOptimisation.compile {
            val dut = new StreamTransactionExtender(
                UInt(32 bits),
                UInt(33 bits),
                12,
                false,
                (id, payload: UInt, last) => payload @@ last
            )
            dut
        }
        compiled.doSimUntilVoid { dut =>
            val inQueue    = mutable.Queue[BigInt]()
            val outQueue   = mutable.Queue[BigInt]()
            val countQueue = mutable.Queue[BigInt]()
            prepare(dut, false, true, inQueue, outQueue, countQueue, lastQueue)
            dut.clockDomain.waitSampling((1 KiB).toInt)
            simSuccess()
        }
    }

    test("testRandomOut") {
        val lastQueue = mutable.Queue[Boolean]()
        val compiled = SimConfig.allOptimisation.compile {
            val dut = new StreamTransactionExtender(
                UInt(32 bits),
                UInt(33 bits),
                12,
                false,
                (id, payload: UInt, last) => payload @@ last
            )
            dut
        }
        compiled.doSimUntilVoid { dut =>
            val inQueue    = mutable.Queue[BigInt]()
            val outQueue   = mutable.Queue[BigInt]()
            val countQueue = mutable.Queue[BigInt]()
            prepare(dut, true, false, inQueue, outQueue, countQueue, lastQueue)
            dut.clockDomain.waitSampling((1 KiB).toInt)
            simSuccess()
        }
    }

    test("testFullPipeline") {
        val lastQueue = mutable.Queue[Boolean]()
        val compiled = SimConfig.allOptimisation.compile {
            val dut = new StreamTransactionExtender(
                UInt(32 bits),
                UInt(33 bits),
                12,
                false,
                (id, payload: UInt, last) => payload @@ last
            )
            dut
        }
        compiled.doSimUntilVoid { dut =>
            val inQueue    = mutable.Queue[BigInt]()
            val outQueue   = mutable.Queue[BigInt]()
            val countQueue = mutable.Queue[BigInt]()
            prepare(dut, true, true, inQueue, outQueue, countQueue, lastQueue)
            dut.clockDomain.waitSampling((1 KiB).toInt)
            simSuccess()
        }
    }
}

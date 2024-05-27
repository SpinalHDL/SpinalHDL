package spinal.lib

import scala.collection.mutable
import scala.util.Random

import spinal.core._
import spinal.lib.sim._
import spinal.core.sim._
import spinal.tester.SpinalSimFunSuite

class SpinalSimStreamShiftChainTester extends SpinalSimFunSuite {
    def prepare(
        dut: StreamShiftChain[UInt],
        alwaysInput: Boolean = false,
        alwaysOutput: Boolean = false,
        inQueue: mutable.Queue[BigInt],
        outQueue: mutable.Queue[BigInt],
    ) {
        dut.clockDomain.forkStimulus(period = 10)
        dut.io.push.valid #= false

        if (alwaysOutput) {
            dut.io.pop.ready #= true
        } else {
            val randomReady = StreamReadyRandomizer(dut.io.pop, dut.clockDomain)
        }

        val driver = StreamDriver(dut.io.push, dut.clockDomain) { payload =>
            if (inQueue.nonEmpty) {
                payload #= inQueue.dequeue()
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

        val monitor = StreamMonitor(dut.io.pop, dut.clockDomain) { payload =>
            {
                val expected = outQueue.dequeue()
                val data = payload.toBigInt
                assert(data == expected)
            }
        }

        for (j <- 0 until 10000) {
            val data = Random.nextInt(0xffffff)

            inQueue.enqueue(data)
            outQueue.enqueue(data)
        }
    }

    test("testRandomInOut") {
        val compiled = SimConfig.allOptimisation.compile {
            val dut = new StreamShiftChain(
                UInt(32 bits),
                12,
            )
            dut
        }
        compiled.doSimUntilVoid { dut =>
            val inQueue    = mutable.Queue[BigInt]()
            val outQueue   = mutable.Queue[BigInt]()
            prepare(dut, false, false, inQueue, outQueue)
            dut.clockDomain.waitSampling((1 KiB).toInt)
            simSuccess()
        }
    }

    test("testRandomIn") {
        val compiled = SimConfig.allOptimisation.compile {
            val dut = new StreamShiftChain(
                UInt(32 bits),
                12
            )
            dut
        }
        compiled.doSimUntilVoid { dut =>
            val inQueue    = mutable.Queue[BigInt]()
            val outQueue   = mutable.Queue[BigInt]()
            prepare(dut, false, true, inQueue, outQueue)
            dut.clockDomain.waitSampling((1 KiB).toInt)
            simSuccess()
        }
    }

    test("testRandomOut") {
        val lastQueue = mutable.Queue[Boolean]()
        val compiled = SimConfig.allOptimisation.compile {
            val dut = new StreamShiftChain(
                UInt(32 bits),
                12
            )
            dut
        }
        compiled.doSimUntilVoid { dut =>
            val inQueue    = mutable.Queue[BigInt]()
            val outQueue   = mutable.Queue[BigInt]()
            prepare(dut, true, false, inQueue, outQueue)
            dut.clockDomain.waitSampling((1 KiB).toInt)
            simSuccess()
        }
    }

    test("testFullPipeline") {
        val lastQueue = mutable.Queue[Boolean]()
        val compiled = SimConfig.allOptimisation.compile {
            val dut = new StreamShiftChain(
                UInt(32 bits),
                12
            )
            dut
        }
        compiled.doSimUntilVoid { dut =>
            val inQueue    = mutable.Queue[BigInt]()
            val outQueue   = mutable.Queue[BigInt]()
            prepare(dut, true, true, inQueue, outQueue)
            dut.clockDomain.waitSampling((1 KiB).toInt)
            simSuccess()
        }
    }

    test("testObjectCompile") {
        val lastQueue = mutable.Queue[Boolean]()
        val compiled = SimConfig.allOptimisation.compile {
            val dut = new Component{
                val input = slave Stream(UInt(32 bits))
                val output = master Stream(UInt(32 bits))
                val hist = StreamShiftChain(input, output, 12)
                val test = out Bool()
                test := hist.io.states(0).valid
            }
            dut
        }
    }
}

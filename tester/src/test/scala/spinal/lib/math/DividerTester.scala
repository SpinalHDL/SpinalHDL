// =======================================================================
// License: MIT (part of spinal.lib)
// =======================================================================
//  ____         _                ____
// | __ )  _ __ (_)  __ _  _ __  / ___|  _   _  _ __    ___
// |  _ \ | '__|| | / _` || '_ \ \___ \ | | | || '_ \  / _ \
// | |_) || |   | || (_| || | | | ___) || |_| || | | ||  __/
// |____/ |_|   |_| \__,_||_| |_||____/  \__,_||_| |_| \___|
//
// =======================================================================
// File Revision: 0.9.0
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Trial Version
// Date: 2026/06
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// File: DividerTester.scala
// Designed By: BrianSune
// Contact: briansune@gmail.com
// =======================================================================

package spinal.lib.math

import spinal.core._
import spinal.core.sim._
import spinal.tester.{SpinalAnyFunSuite, SpinalSimTester}
import scala.collection.mutable.Queue
import scala.util.Random


class DividerTester extends SpinalAnyFunSuite {
    def runVideoSim(name: String): Unit = {

        val compiled = SimConfig
            .withWave
            .withConfig(SpinalConfig(verbose = true)
        ).compile(
            new UnsignedDivider(nWidth = 8, dWidth = 8, storeDenominator = true)
        )

        compiled.doSim(name) { dut =>
            dut.clockDomain.forkStimulus(period = 10) // 100MHz clock

            val inputQueue = Queue[(Int, Int)]()
            val random = new Random()

            val scoreboard = fork {
                dut.io.rsp.ready #= true
                while (true) {
                    dut.clockDomain.waitSampling()
                    if (dut.io.rsp.valid.toBoolean) {
                        val result = dut.io.rsp.payload.quotient.toInt
                        val remainder = dut.io.rsp.payload.remainder.toInt
                        val (n, d) = inputQueue.dequeue()
                        println(s"N: $n, D: $d -> Got response: Result=$result, Remainder=$remainder")
                        if(d != 0){
                            assert((n / d).toInt == result, "Quotient ERROR")
                            assert((n % d).toInt == remainder, "Remainder ERROR")
                        }
                    }
                }
            }

            dut.io.cmd.valid #= false
            dut.io.flush #= false
            dut.clockDomain.waitSampling()
            waitUntil(dut.io.cmd.ready.toBoolean)
            dut.clockDomain.waitSampling()

            def check(
                n: Int, d: Int, inputQueue: Queue[(Int, Int)]
            ){
                val earlyValid = random.nextInt(2) == 0
                if(!earlyValid){
                    waitUntil(dut.io.cmd.ready.toBoolean)
                    for(_ <- 0 until random.nextInt(10)){
                        dut.clockDomain.waitSampling()
                    }
                }
                inputQueue.enqueue((n, d))
                dut.io.cmd.payload.numerator #= n
                dut.io.cmd.payload.denominator #= d
                dut.io.cmd.valid #= true
                dut.clockDomain.waitSampling()
                if(earlyValid){
                    waitUntil(dut.io.cmd.ready.toBoolean)
                    dut.clockDomain.waitSampling()
                }
                waitUntil(!dut.io.cmd.ready.toBoolean)
                dut.io.cmd.valid #= false
                dut.clockDomain.waitSampling()
                dut.clockDomain.waitSampling()
            }

            List(
            (255,0),
            (0, 255),
            (255, 128),
            (255, 127),
            (128, 255),
            (127, 255),
            (127, 128),
            (128, 127),
            (255, 255),
            (128, 128),
            (128, 0),
            (127, 0),
            (127, 127),
            (0, 127),
            (127, 1),
            (128, 1),
            (255, 1),
            (0, 1),
            ).foreach{ case (n, d) =>
            check(n, d, inputQueue)
            }

            dut.clockDomain.waitSampling(10)
        }
    }

    val name = s"Unsigned Divider with storeDenominator N8D8"

    test(name) {
      runVideoSim(name)
    }
}



package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._
import spinal.lib.bus.amba3.ahblite.sim._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.sim.ScoreboardInOrder

import scala.util.Random



class AhbLite3InterconnectComponent(config: AhbLite3Config) extends Component{

  val io = new Bundle{
    val busMasters  = Vec(slave(AhbLite3Master(config)), 3)
    val busSlaves   = Vec(master(AhbLite3(config)), 3)
  }

  val crossbar = AhbLite3CrossbarFactory(config)
               .addSlaves(
                 io.busSlaves(0) -> (0x1000, 0x1000),
                 io.busSlaves(1) -> (0x3000, 0x1000),
                 io.busSlaves(2) -> (0x4000, 0x1000)
                )
                .addConnections(
                   io.busMasters(0).toAhbLite3() -> List(io.busSlaves(0), io.busSlaves(1)),
                   io.busMasters(1).toAhbLite3() -> List(io.busSlaves(1), io.busSlaves(2), io.busSlaves(3)),
                   io.busMasters(2).toAhbLite3() -> List(io.busSlaves(0), io.busSlaves(3))
                )
                .build()

}


/**
  * Test the AhbLite3 Interconnect
  */
class SpinalSimAhbLite3Interconnect extends FunSuite {


  def testInterconnect(config: AhbLite3Config, description: String = ""): Unit = {

    val compiledRTL = SimConfig.allOptimisation.withConfig(SpinalConfig(anonymSignalPrefix = "a")).withWave.compile(rtl = new AhbLite3InterconnectComponent(config))

    compiledRTL.doSim(description){ dut =>

      dut.clockDomain.forkStimulus(period = 10)

      /* init slave Value */
      dut.io.busSlaves.foreach{ bus =>
        bus.HREADYOUT #= true
        bus.HRDATA.randomize()
        bus.HRESP     #= false
      }

      /* Init master value */
      dut.io.busMasters.foreach{ bus =>
        bus.HADDR.randomize()
        bus.HWRITE.randomize()
        bus.HSIZE     #= 0
        bus.HBURST    #= 0
        bus.HPROT     #= 0
        bus.HTRANS    #= 0
        bus.HMASTLOCK #= false
        bus.HWDATA.randomize()
      }


      dut.clockDomain.waitActiveEdge(10)

      SimTimeout(1000 * 10000)

      val score = ScoreboardInOrder[AhbLite3Transaction]()

      //val receiver = new AhbLite3Driver(dut.io.busOut, dut.clockDomain)

      //      /** Monitor the master */
      //      AhbLite3Monitor(dut.io.busMaster, dut.clockDomain){ bus =>
      //        score.pushRef(AhbLite3Transaction.sampleAsSlave(bus, dut.clockDomain))
      //      }
      //
      //      /** Monitor all slaves */
      //      dut.io.busSlaves.map{ bus =>
      //        AhbLite3Monitor(bus, dut.clockDomain){ bus =>
      //          score.pushDut(AhbLite3Transaction.sampleAsSlave(bus, dut.clockDomain))
      //        }
      //      }

      /** Do several time the tests */
      for(_ <- 0 until 1){

        val masterPool = scala.collection.mutable.ListBuffer[SimThread]()


        dut.io.busMasters.slice(0, 1).foreach { bus =>

          val busDriver = new AhbLite3Driver(bus.toAhbLite3(), dut.clockDomain)

          masterPool += fork {

            /* Generate random transaction */
            val seq = new AhbLite3Sequencer()

            //seq.addTransaction(AhbLite3Transaction.createSingleTransaction(Random.nextInt(3)))
            seq.addTransaction(AhbLite3Transaction(htrans = 2, haddr = 0x100).randomizeData())
            seq.addTransaction(AhbLite3Transaction(htrans = 2, haddr = 0x500).randomizeData())
            //seq.addTransaction(AhbLite3Transaction.createBurstTransaction())

            // Random sleep before starting transaction on a slave
            dut.clockDomain.waitActiveEdge(Random.nextInt(1))


            /* Execute all transactions */
            while (!seq.isEmpty) {
              busDriver.drive(seq.nextTransaction)
              dut.clockDomain.waitActiveEdge(Random.nextInt(2))
            }

            // Check that all transactions has been received
            assert(score.dut.isEmpty & score.ref.isEmpty, "Transaction errors")
          }
        }


        masterPool.foreach{process => process.join()}
        dut.clockDomain.waitActiveEdge(20)
      }
    }
  }


  test("Interconnect"){
    testInterconnect(AhbLite3Config(32, 32), "BasicInterconnect")
  }
}

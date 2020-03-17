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
    val busMasters  = Vec(slave(AhbLite3(config)), 3)
    val busSlaves   = Vec(master(AhbLite3(config)), 3)
  }

  val busMaster = Vec(AhbLite3(config), io.busMasters.length)

  (busMaster, io.busMasters).zipped.foreach{ (tmpBus, ioBus) =>

    tmpBus.HSEL      := ioBus.HSEL
    tmpBus.HADDR     := ioBus.HADDR
    tmpBus.HTRANS    := ioBus.HTRANS
    tmpBus.HBURST    := ioBus.HBURST
    tmpBus.HSIZE     := ioBus.HSIZE
    tmpBus.HMASTLOCK := ioBus.HMASTLOCK
    tmpBus.HWDATA    := ioBus.HWDATA
    tmpBus.HPROT     := ioBus.HPROT
    tmpBus.HWRITE    := ioBus.HWRITE
    tmpBus.HREADY    := tmpBus.HREADYOUT

    ioBus.HREADYOUT := tmpBus.HREADYOUT
    ioBus.HRDATA    := tmpBus.HRDATA
    ioBus.HRESP     := tmpBus.HRESP
  }

  val crossbar = AhbLite3CrossbarFactory(config)
       .addSlaves(
         io.busSlaves(0) -> (0x1000, 1 KiB),
         io.busSlaves(1) -> (0x3000, 1 KiB),
         io.busSlaves(2) -> (0x4000, 1 KiB)
        )
        .addConnections(
          busMaster(0) -> List(io.busSlaves(0), io.busSlaves(1)),
          busMaster(1) -> List(io.busSlaves(0), io.busSlaves(1), io.busSlaves(2)),
          busMaster(2) -> List(io.busSlaves(0), io.busSlaves(2))
        )

    crossbar.build()

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
        bus.HSEL      #= false
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


      dut.io.busSlaves.foreach{ bus =>
        val receiver = new AhbLite3Driver(bus, dut.clockDomain)
        receiver.slaveSink()
      }

      /** Monitor all slaves */
      dut.io.busSlaves.map{ bus =>
        AhbLite3Monitor(bus, dut.clockDomain){ bus =>
          while(bus.HSEL.toBoolean & bus.HTRANS.toInt != 0 & bus.HREADYOUT.toBoolean) {
            val transaction = AhbLite3Transaction.sampleAsSlave(bus, dut.clockDomain)
           // score.pushDut(transaction)
           // println(f"Slaves ${transaction}")
          }
        }
      }


      /** Monitor all Master */
      dut.io.busMasters.map{ bus =>
        AhbLite3Monitor(bus, dut.clockDomain){ bus =>
          while(bus.HSEL.toBoolean & bus.HTRANS.toInt != 0 & bus.HREADYOUT.toBoolean) {
            val transaction = AhbLite3Transaction.sampleAsSlave(bus, dut.clockDomain)
            //score.pushDut(transaction)
           // println(f"Master ${transaction}")
          }
        }
      }

      /** Do several time the tests */
      for(_ <- 0 until 1){

        val masterPool = scala.collection.mutable.ListBuffer[SimThread]()

        dut.io.busMasters.zipWithIndex.slice(0, 3).foreach { case (busMaster, index) =>

          // get list of slave of the master
          val slaves = dut.crossbar.slavesFromMaster(dut.busMaster(index))
          
          val busDriver = new AhbLite3Driver(busMaster, dut.clockDomain)

          masterPool += fork {

            /* Generate random transaction */
            val seq = new AhbLite3Sequencer()

            //seq.addTransaction(AhbLite3Transaction.createSingleTransaction(3))
            seq.addTransaction(AhbLite3Transaction(htrans = 2, haddr = slaves.head._2.mapping.base).randomizeData())
            seq.addTransaction(AhbLite3Transaction(htrans = 2, haddr = slaves(1)._2.mapping.base).randomizeData())
            //seq.addTransaction(AhbLite3Transaction.createBurstTransaction())

            // Random sleep before starting transaction on a slave
            dut.clockDomain.waitActiveEdge(Random.nextInt(1))


            /* Execute all transactions */
            while (!seq.isEmpty) {
              busDriver.drive(seq.nextTransaction)
              dut.clockDomain.waitActiveEdge(Random.nextInt(2))
            }
          }
        }

        masterPool.foreach{process => process.join()}


        // Check that all transactions has been received
        assert(score.dut.isEmpty & score.ref.isEmpty, "Transaction errors")

        dut.clockDomain.waitActiveEdge(20)
      }
    }
  }


  test("Interconnect"){
    testInterconnect(AhbLite3Config(32, 32), "BasicInterconnect")
  }
}

package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._
import spinal.lib.bus.amba3.ahblite.sim._
import spinal.lib.sim.ScoreboardInOrder

import scala.util.Random



class AhbLite3ArbiterComponent(config: AhbLite3Config, size: Int, roundRobinArbiter: Boolean) extends Component{

  val io = new Bundle{
    val busIn  = Vec(slave(AhbLite3(config)), size)
    val busOut = master(AhbLite3(config))
  }

  val arbiter = AhbLite3Arbiter(config, size, roundRobinArbiter)
  (arbiter.io.inputs, io.busIn).zipped.foreach{ (arbiter, busIn) =>
    arbiter.HSEL   := busIn.HSEL
    arbiter.HADDR  := busIn.HADDR
    arbiter.HTRANS := busIn.HTRANS
    arbiter.HBURST := busIn.HBURST
    arbiter.HSIZE  := busIn.HSIZE
    arbiter.HMASTLOCK := busIn.HMASTLOCK
    arbiter.HWDATA := busIn.HWDATA
    arbiter.HPROT  := busIn.HPROT
    arbiter.HWRITE := busIn.HWRITE
    arbiter.HREADY := arbiter.HREADYOUT

    busIn.HREADYOUT := arbiter.HREADYOUT
    busIn.HRDATA    := arbiter.HRDATA
    busIn.HRESP     := arbiter.HRESP
  }
  io.busOut <> arbiter.io.output

}


/**
  * Test the AhbLite3 Arbiter
  */
class SpinalSimAhbLite3Arbiter extends FunSuite {


  def testArbiter(config: AhbLite3Config, size: Int, roundRobinArbiter: Boolean, description: String = ""): Unit = {

    val compiledRTL = SimConfig.allOptimisation.withConfig(SpinalConfig(anonymSignalPrefix = "a")).withWave.compile(rtl = new AhbLite3ArbiterComponent(config, size, roundRobinArbiter))

    compiledRTL.doSim(description){ dut =>

      dut.clockDomain.forkStimulus(period = 10)

      /* init Value */
      dut.io.busIn.foreach{ bus =>
        bus.HADDR.randomize()
        bus.HSEL      #= false
        bus.HWRITE.randomize()
        bus.HSIZE     #= 0
        bus.HBURST    #= 0
        bus.HPROT     #= 0
        bus.HTRANS    #= 0
        bus.HMASTLOCK #= false
        bus.HWDATA.randomize()
      }

      dut.io.busOut.HRDATA    #= 0
      dut.io.busOut.HREADYOUT #= true
      dut.io.busOut.HRESP     #= false

      dut.clockDomain.waitActiveEdge(10)

      SimTimeout(1000 * 10000)

      val score = ScoreboardInOrder[AhbLite3Transaction]()

      /** Slave driver response */
      val receiver = new AhbLite3Driver(dut.io.busOut, dut.clockDomain)
      receiver.slaveSink()
      //receiver.slaveRandomWait()

      /** Monitor the receiver */
      AhbLite3Monitor(dut.io.busOut, dut.clockDomain){ bus =>
        score.pushRef(AhbLite3Transaction.sampleAsSlave(bus, dut.clockDomain))
      }

      /** Monitor all senders */
      dut.io.busIn.map{ bus =>
        AhbLite3Monitor(bus, dut.clockDomain){ bus =>
          score.pushDut(AhbLite3Transaction.sampleAsSlave(bus, dut.clockDomain))
        }
      }

      /** Do several time the tests */
      for(_ <- 0 until 1){

        val masterPool = scala.collection.mutable.ListBuffer[SimThread]()

        dut.io.busIn.slice(0, 3).foreach{ bus =>
          val busDriver = new AhbLite3Driver(bus, dut.clockDomain)

          masterPool += fork{

            /* Generate random transaction */
            val seq = new AhbLite3Sequencer()

            seq.addTransaction(AhbLite3Transaction.createSingleTransaction(Random.nextInt(3)))
            seq.addTransaction(AhbLite3Transaction(htrans = 2).randomizeAddress().randomizeData())
            seq.addTransaction(AhbLite3Transaction.createBurstTransaction())

            // Random sleep before starting transaction on a slave
            dut.clockDomain.waitActiveEdge(Random.nextInt(4))

            /* Execute all transactions */
            while(!seq.isEmpty){
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


  test("Arbiter Basic"){
    testArbiter(AhbLite3Config(32, 32), 3, true, "BasisArbiter")
  }
}

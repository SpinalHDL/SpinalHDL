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



class AhbLite3DecoderComponent(config: AhbLite3Config, size: Int) extends Component{

  val io = new Bundle{
    val busMaster  = slave(AhbLite3(config))
    val busSlaves  = Vec(master(AhbLite3(config)), size)
  }

  val decoder = AhbLite3Decoder(
    master  = io.busMaster,
    slaves  = for(i <- 0 until size) yield (io.busSlaves(i) -> SizeMapping((i << 10), 1 KiB))
  )
}


/**
  * Test the AhbLite3 Arbiter
  */
class SpinalSimAhbLite3Decoder extends FunSuite {


  def testDecoder(config: AhbLite3Config, size: Int, description: String = ""): Unit = {

    val compiledRTL = SimConfig.allOptimisation.withConfig(SpinalConfig(anonymSignalPrefix = "a")).withWave.compile(rtl = new AhbLite3DecoderComponent(config, size))

    compiledRTL.doSim(description){ dut =>

      dut.clockDomain.forkStimulus(period = 10)

      /* init slave Value */
      dut.io.busSlaves.foreach{ bus =>
        bus.HREADYOUT #= true
        bus.HRDATA.randomize()
        bus.HRESP     #= false
      }

      /* Init master value */
      dut.io.busMaster.HADDR.randomize()
      dut.io.busMaster.HSEL      #= false
      dut.io.busMaster.HWRITE.randomize()
      dut.io.busMaster.HSIZE     #= 0
      dut.io.busMaster.HBURST    #= 0
      dut.io.busMaster.HPROT     #= 0
      dut.io.busMaster.HTRANS    #= 0
      dut.io.busMaster.HMASTLOCK #= false
      dut.io.busMaster.HWDATA.randomize()

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

        //val masterPool = scala.collection.mutable.ListBuffer[SimThread]()

          val busDriver = new AhbLite3Driver(dut.io.busMaster, dut.clockDomain)

          fork{

            /* Generate random transaction */
            val seq = new AhbLite3Sequencer()

            //seq.addTransaction(AhbLite3Transaction.createSingleTransaction(Random.nextInt(3)))
            seq.addTransaction(AhbLite3Transaction(htrans = 2, haddr = 0x100).randomizeData())
            seq.addTransaction(AhbLite3Transaction(htrans = 2, haddr = 0x500).randomizeData())
            //seq.addTransaction(AhbLite3Transaction.createBurstTransaction())

            // Random sleep before starting transaction on a slave
            dut.clockDomain.waitActiveEdge(Random.nextInt(1))


            /* Execute all transactions */
            while(!seq.isEmpty){
              busDriver.drive(seq.nextTransaction)
              dut.clockDomain.waitActiveEdge(Random.nextInt(2))
            }

            // Check that all transactions has been received
            assert(score.dut.isEmpty & score.ref.isEmpty, "Transaction errors")
          }


//        masterPool.foreach{process => process.join()}
        dut.clockDomain.waitActiveEdge(20)
      }
    }
  }


  test("Arbiter"){
    testDecoder(AhbLite3Config(32, 32), 3, "BasisDecoder")
  }
}

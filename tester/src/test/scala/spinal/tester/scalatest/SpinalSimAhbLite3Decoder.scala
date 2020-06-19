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

  val busMaster = AhbLite3(config)

  busMaster.HSEL      := io.busMaster.HSEL
  busMaster.HADDR     := io.busMaster.HADDR
  busMaster.HTRANS    := io.busMaster.HTRANS
  busMaster.HBURST    := io.busMaster.HBURST
  busMaster.HSIZE     := io.busMaster.HSIZE
  busMaster.HMASTLOCK := io.busMaster.HMASTLOCK
  busMaster.HWDATA    := io.busMaster.HWDATA
  busMaster.HPROT     := io.busMaster.HPROT
  busMaster.HWRITE    := io.busMaster.HWRITE
  busMaster.HREADY    := busMaster.HREADYOUT

  io.busMaster.HREADYOUT := busMaster.HREADYOUT
  io.busMaster.HRDATA    := busMaster.HRDATA
  io.busMaster.HRESP     := busMaster.HRESP


  val decoder = AhbLite3Decoder(
    master  = busMaster,
    slaves  = for(i <- 0 until size) yield (io.busSlaves(i) -> SizeMapping((i << 10), 1 KiB))
  )
}


/**
  * Test the AhbLite3 Decoder
  */
class SpinalSimAhbLite3Decoder extends FunSuite {

  trait ArbiterSlaveMode
  object SinkResponse  extends ArbiterSlaveMode
  object DelayResponse extends ArbiterSlaveMode
  object ErrorResponse extends ArbiterSlaveMode


  def testDecoder(config: AhbLite3Config, size: Int, modeSlave: ArbiterSlaveMode, description: String = ""): Unit = {

    val compiledRTL = SimConfig.allOptimisation.withConfig(SpinalConfig(anonymSignalPrefix = "a")).compile(rtl = new AhbLite3DecoderComponent(config, size))

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

      dut.io.busSlaves.foreach{ bus =>
        val receiver = new AhbLite3Driver(bus, dut.clockDomain)
        modeSlave match{
          case SinkResponse  => receiver.slaveSink()
          case DelayResponse => receiver.slaveRandomWait()
          case ErrorResponse => receiver.slaveRandomError()
        }

      }

      /** Monitor the master */
      AhbLite3Monitor(dut.io.busMaster, dut.clockDomain){ bus =>
        while(bus.HSEL.toBoolean & bus.HTRANS.toInt != 0 & bus.HREADYOUT.toBoolean){
          val transaction = AhbLite3Transaction.sampleAsSlave(bus, dut.clockDomain)
          score.pushRef(transaction)
          //println(f"Master ${transaction}")
        }
      }

      /** Monitor all slaves */
      dut.io.busSlaves.map{ bus =>
        AhbLite3Monitor(bus, dut.clockDomain){ bus =>
          while(bus.HSEL.toBoolean & bus.HTRANS.toInt != 0 & bus.HREADYOUT.toBoolean) {
            val transaction = AhbLite3Transaction.sampleAsSlave(bus, dut.clockDomain)
            score.pushDut(transaction)
            //println(f"Slaves ${transaction}")
          }
        }
      }

      /** Do several time the tests */
      for(_ <- 0 until 1){

          val busDriver = new AhbLite3Driver(dut.io.busMaster, dut.clockDomain)

          val masterFork = fork{

            /* Generate random transaction */
            val seq = new AhbLite3Sequencer()

            seq.addTransaction(for(i <- 0 until 4) yield AhbLite3Transaction(htrans = 2, haddr = 0x100 + (i * 0x300)).randomizeData().randomizeRW())
            seq.addTransaction(AhbLite3Transaction(htrans = 2, haddr = 0x100).randomizeData())
            seq.addTransaction(AhbLite3Transaction(htrans = 2, haddr = 0x500).randomizeData())
            seq.addTransaction(AhbLite3Transaction(htrans = 2, haddr = 0xA00).randomizeData())
            seq.addTransaction(AhbLite3Transaction.createBurstTransaction(true))

            // Random sleep before starting transaction on a slave
            dut.clockDomain.waitActiveEdge(Random.nextInt(1))


            /* Execute all transactions */
            while(!seq.isEmpty){
              busDriver.drive(seq.nextTransaction)
              dut.clockDomain.waitActiveEdge(Random.nextInt(2))
            }

          }

        masterFork.join()


        // Check that all transactions has been received
        assert(score.dut.isEmpty & score.ref.isEmpty, "Transaction errors")

        dut.clockDomain.waitActiveEdge(20)
      }
    }
  }


  test("Decoder basic response"){
    testDecoder(AhbLite3Config(32, 32), 3, SinkResponse, "BasisDecoder")
  }

  test("Decoder Delay response"){
    testDecoder(AhbLite3Config(32, 32), 3, DelayResponse, "BasisDecoder")
  }
}

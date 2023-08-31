package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.core.sim._
import spinal.tester.SpinalAnyFunSuite
import spinal.core.sim.SimCompiled
import spinal.lib.bus.amba4.axi.sim.{Axi4ReadOnlyMasterAgent, Axi4ReadOnlyMonitor, Axi4ReadOnlySlaveAgent, Axi4WriteOnlyMasterAgent, Axi4WriteOnlyMonitor, Axi4WriteOnlySlaveAgent}
// import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4ReadOnly, Axi4ReadOnlyDownsizer, Axi4ReadOnlyIdRemover, Axi4ReadOnlyUpsizer, Axi4SharedIdRemover, Axi4WriteOnly, Axi4WriteOnlyDownsizer, Axi4WriteOnlyIdRemover, Axi4WriteOnlyUpsizer}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.{master, slave}

import scala.collection.mutable
import scala.util.Random

class Axi4UpsizerTester extends SpinalAnyFunSuite {

  def writeTester(dut : Axi4WriteOnlyUpsizer): Unit ={
    dut.clockDomain.forkStimulus(10)


    val regions = mutable.Set[SizeMapping]()
    val inputAgent = new Axi4WriteOnlyMasterAgent(dut.io.input, dut.clockDomain) {
      override def genAddress(): BigInt = ((Random.nextInt(1 << 19)))// & 0xFFF00) | 6

      override val pageAlignBits = 16
//      override def lens   =  List(255)
//      override def sizes  = List(5)

      override def bursts: List[Int] = List(1)

      override def mappingAllocate(mapping: SizeMapping): Boolean = {
        if(regions.exists(_.overlap(mapping))) return false
        regions += mapping
        true
      }

      override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
    }
    val outputAgent = new Axi4WriteOnlySlaveAgent(dut.io.output, dut.clockDomain)

    val writes = mutable.HashMap[BigInt, Byte]()
    val inputMonitor = new Axi4WriteOnlyMonitor(dut.io.input, dut.clockDomain) {
      override def onWriteByte(address: BigInt, data: Byte, id: Int): Unit = {
        //          println(s"I $address -> $data")
        assert(!writes.contains(address))
        writes(address) = data
      }
    }

    val outputMonitor = new Axi4WriteOnlyMonitor(dut.io.output, dut.clockDomain) {
      override def onWriteByte(address: BigInt, data: Byte, id: Int): Unit = {
        //          println(s"O $address -> $data")
        assert(writes(address) == data)
        writes.remove(address)
      }
    }


    dut.clockDomain.waitSamplingWhere(inputAgent.rspCounter > 10000)
    inputAgent.allowGen = false
    dut.clockDomain.waitSamplingWhere(!inputAgent.pending)
    dut.clockDomain.waitSampling(100)
    assert(writes.isEmpty)
    println("done")
  }

  test("writeOnly_32_64") {
    SimConfig.compile(Axi4WriteOnlyUpsizer(Axi4Config(20, 32, 4), Axi4Config(20, 64, 4))).doSim("test", 42)(writeTester)
  }
  test("writeOnly_16_64") {
    SimConfig.compile(Axi4WriteOnlyUpsizer(Axi4Config(20, 16, 4), Axi4Config(20, 64, 4))).doSim("test", 42)(writeTester)
  }
  test("writeOnly_32_128") {
    SimConfig.compile(Axi4WriteOnlyUpsizer(Axi4Config(20, 32, 4), Axi4Config(20, 128, 4))).doSim("test", 42)(writeTester)
  }
  test("writeOnly_256_512") {
    SimConfig.compile(Axi4WriteOnlyUpsizer(Axi4Config(20, 256, 4), Axi4Config(20, 512, 4))).doSim("test", 42)(writeTester)
  }



  def readTester(dut : Axi4ReadOnlyUpsizer): Unit ={
    dut.clockDomain.forkStimulus(10)

    val regions = mutable.Set[SizeMapping]()
    val inputAgent = new Axi4ReadOnlyMasterAgent(dut.io.input, dut.clockDomain) {
      override def genAddress(): BigInt = Random.nextInt(1 << 19)
      override def bursts: List[Int] = List(1)
      override val pageAlignBits = 20
//      override def lens   =  (0xf0 to 0xff).toList
      override def mappingAllocate(mapping: SizeMapping): Boolean = {
        if(regions.exists(_.overlap(mapping))) return false
        regions += mapping
        true
      }

      override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
    }

    val outputAgent = new Axi4ReadOnlySlaveAgent(dut.io.output, dut.clockDomain)



    val reads = mutable.HashMap[BigInt, Byte]()
    val inputMonitor = new Axi4ReadOnlyMonitor(dut.io.input, dut.clockDomain) {
      override def onReadByte(address: BigInt, data: Byte, id : Int): Unit = {
        reads(address) = data
      }
    }

    val outputMonitor = new Axi4ReadOnlyMonitor(dut.io.output, dut.clockDomain) {
      override def onReadByte(address: BigInt, data: Byte, id : Int): Unit = {
        if(reads.contains(address)) {
          assert(reads(address) == data)
          reads.remove(address)
        }
      }

      override def onResponse(address: BigInt, id: Int, last: Boolean, resp: Byte): Unit = if (last) assert(reads.isEmpty)
    }



    dut.clockDomain.waitSamplingWhere(inputAgent.rspCounter > 10000)
    inputAgent.allowGen = false
    dut.clockDomain.waitSamplingWhere(!inputAgent.pending)
    dut.clockDomain.waitSampling(100)
    assert(reads.isEmpty)
    println("done")
  }

  test("readOnly_32_64") {
    SimConfig.compile(Axi4ReadOnlyUpsizer(Axi4Config(20, 32, 4), Axi4Config(20, 64, 4),4)).doSim("test", 42)(readTester)
  }
  test("readOnly_16_64") {
    SimConfig.compile(Axi4ReadOnlyUpsizer(Axi4Config(20, 16, 4), Axi4Config(20, 64, 4),4)).doSim("test", 42)(readTester)
  }
  test("readOnly_32_128") {
    SimConfig.compile(Axi4ReadOnlyUpsizer(Axi4Config(20, 32, 4), Axi4Config(20, 128, 4),4)).doSim("test", 42)(readTester)
  }
  test("readOnly_256_512") {
    SimConfig.compile(Axi4ReadOnlyUpsizer(Axi4Config(20, 256, 4), Axi4Config(20, 512, 4),4)).doSim("test", 42)(readTester)
  }
}

class Axi4DownsizerTester extends SpinalAnyFunSuite {

    def writeTester(dut: Axi4WriteOnlyDownsizer, pipelined: Boolean = false): Unit = {
        dut.clockDomain.forkStimulus(10)

        val regions = mutable.Set[SizeMapping]()
        val inputAgent = new Axi4WriteOnlyMasterAgent(dut.io.input, dut.clockDomain) {
            override def genAddress(): BigInt = Random.nextInt(1 << 19) // & 0xFFF00) | 6

            override def bursts: List[Int] = List(1)

            override def mappingAllocate(mapping: SizeMapping): Boolean = {
                if (regions.exists(_.overlap(mapping))) return false
                regions += mapping
                true
            }

            override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
        }
        if (pipelined) {
            inputAgent.awDriver.transactionDelay = () => 0
            inputAgent.wDriver.transactionDelay = () => 0
            inputAgent.bDriver.factor = 1.1f
        } else {
            inputAgent.awDriver.transactionDelay = () => 100 + Random.nextInt(100)
        }

        val outputAgent = new Axi4WriteOnlySlaveAgent(dut.io.output, dut.clockDomain)
        if (pipelined) {
            outputAgent.bDriver.transactionDelay = () => 0
            outputAgent.awDriver.factor = 1.1f
            outputAgent.wDriver.factor = 1.1f
        }

        val writes = mutable.HashMap[BigInt, Byte]()
        val datas  = mutable.Queue[Byte]()
        val inputMonitor = new Axi4WriteOnlyMonitor(dut.io.input, dut.clockDomain) {
            override def onWriteByte(address: BigInt, data: Byte, id: Int): Unit = {
                //          println(s"I $address -> $data")
                writes(address) = data
                datas.enqueue(data)
            }
        }

        val outputMonitor = new Axi4WriteOnlyMonitor(dut.io.output, dut.clockDomain) {
            override def onWriteByte(address: BigInt, data: Byte, id: Int): Unit = {
                //          println(s"O $address -> $data")
                assert(writes(address) == data)
                writes.remove(address)
                assert(data == datas.dequeue())
            }
        }

        dut.clockDomain.waitSamplingWhere(inputAgent.rspCounter > 10000)
        inputAgent.allowGen = false
        dut.clockDomain.waitSamplingWhere(!inputAgent.pending)
        dut.clockDomain.waitSampling(100)
        assert(writes.isEmpty)
        println("done")
    }

    test("writeOnly_32_64") {
        SimConfig
            .compile(
                Axi4WriteOnlyDownsizer(
                    Axi4Config(20, 64, 4, useBurst = false, useId = false),
                    Axi4Config(20, 32, 4, useBurst = false, useId = false)
                )
            )
            .doSim("test", 42)((dut: Axi4WriteOnlyDownsizer) => writeTester(dut))
    }
    test("writeOnly_16_64") {
        SimConfig
            .compile(
                Axi4WriteOnlyDownsizer(
                    Axi4Config(20, 64, 4, useBurst = false, useId = false),
                    Axi4Config(20, 16, 4, useBurst = false, useId = false)
                )
            )
            .doSim("test", 42)((dut: Axi4WriteOnlyDownsizer) => writeTester(dut))
    }
    test("writeOnly_32_128") {
        SimConfig
            .compile(
                Axi4WriteOnlyDownsizer(
                    Axi4Config(20, 128, 4, useBurst = false, useId = false),
                    Axi4Config(20, 32, 4, useBurst = false, useId = false)
                )
            )
            .doSim("test", 42)((dut: Axi4WriteOnlyDownsizer) => writeTester(dut))
    }
    test("writeOnly_32_128_pipelined") {
        SimConfig
            .compile(
                Axi4WriteOnlyDownsizer(
                    Axi4Config(20, 128, 4, useBurst = false, useId = false),
                    Axi4Config(20, 32, 4, useBurst = false, useId = false)
                )
            )
            .doSim("test", 42)((dut: Axi4WriteOnlyDownsizer) => writeTester(dut, true))
    }

    def readTester(dut: Axi4ReadOnlyDownsizer, pipelined: Boolean = false): Unit = {
        dut.clockDomain.forkStimulus(10)

        val regions = mutable.Set[SizeMapping]()
        val inputAgent = new Axi4ReadOnlyMasterAgent(dut.io.input, dut.clockDomain) {
            override def genAddress(): BigInt = Random.nextInt(1 << 19)
            override def bursts: List[Int]    = List(1)
            override def mappingAllocate(mapping: SizeMapping): Boolean = {
                if (regions.exists(_.overlap(mapping))) return false
                regions += mapping
                true
            }

            override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
        }
        if (pipelined) {
            inputAgent.arDriver.transactionDelay = () => 0
            inputAgent.rDriver.factor = 1.1f
        } else {
            inputAgent.arDriver.transactionDelay = () => 100 + Random.nextInt(100)
        }

        val outputAgent = new Axi4ReadOnlySlaveAgent(dut.io.output, dut.clockDomain,withReadInterleaveInBurst = false, withArReordering = false)
        if (pipelined) {
            outputAgent.rDriver.transactionDelay = () => 0
            outputAgent.arDriver.factor = 1.1f
        }

        val reads = mutable.HashMap[BigInt, Byte]()
        val datas = mutable.Queue[Byte]()

        def checkReads(address: BigInt, data: Byte) {
            if (!reads.contains(address)) {
                reads(address) = data
                datas.enqueue(data)
            } else {
                assert(reads(address) == data, "address:" + address.toString(16))
                reads.remove(address)
                assert(data == datas.dequeue(), "address:" + address.toString(16))
            }
        }

        var lastCheck = false
        dut.clockDomain.onSamplings {
            if (lastCheck) {
                assert(reads.isEmpty)
                lastCheck = false
            }
            val last = dut.io.input.readRsp.last.toBoolean
            if (dut.io.input.readRsp.valid.toBoolean && dut.io.input.readRsp.ready.toBoolean && last) {
                lastCheck = true
            }
        }
        val inputMonitor = new Axi4ReadOnlyMonitor(dut.io.input, dut.clockDomain) {
            override def onReadByte(address: BigInt, data: Byte, id: Int): Unit = {
                checkReads(address, data)
            }
        }

        val outputMonitor = new Axi4ReadOnlyMonitor(dut.io.output, dut.clockDomain) {
            override def onReadByte(address: BigInt, data: Byte, id: Int): Unit = {
                checkReads(address, data)
            }
        }

        dut.clockDomain.waitSamplingWhere(inputAgent.rspCounter > 10000)
        inputAgent.allowGen = false
        dut.clockDomain.waitSamplingWhere(!inputAgent.pending)
        dut.clockDomain.waitSampling(100)
        assert(reads.isEmpty)
        println("done")
    }

  test("readOnly_32_64") {
    SimConfig
      .compile(
        Axi4ReadOnlyDownsizer(
          Axi4Config(20, 64, 4, useBurst = false, useId = false),
          Axi4Config(20, 32, 4, useBurst = false, useId = false)
        )
      )
      .doSim("test", 42)((dut: Axi4ReadOnlyDownsizer) => readTester(dut))
  }
  test("readOnly_16_64") {
    SimConfig
      .compile(
        Axi4ReadOnlyDownsizer(
          Axi4Config(20, 64, 4, useBurst = false, useId = false),
          Axi4Config(20, 16, 4, useBurst = false, useId = false)
        )
      )
      .doSim("test", 42)((dut: Axi4ReadOnlyDownsizer) => readTester(dut))
  }
  test("readOnly_32_128") {
    SimConfig
      .compile(
        Axi4ReadOnlyDownsizer(
          Axi4Config(20, 128, 4, useBurst = false, useId = false),
          Axi4Config(20, 32, 4, useBurst = false, useId = false)
        )
      )
      .doSim("test", 42)((dut: Axi4ReadOnlyDownsizer) => readTester(dut))
  }
  test("readOnly_32_128_pipelined") {
    SimConfig
      .compile(
        Axi4ReadOnlyDownsizer(
          Axi4Config(20, 128, 4, useBurst = false, useId = false),
          Axi4Config(20, 32, 4, useBurst = false, useId = false)
        )
      )
      .doSim("test", 42)((dut: Axi4ReadOnlyDownsizer) => readTester(dut, true))
  }
}

class Axi4IdRemoverTester extends SpinalAnyFunSuite {

  def writeTester(dut : Axi4WriteOnlyIdRemover): Unit ={
    dut.clockDomain.forkStimulus(10)


    val regions = mutable.Set[SizeMapping]()
    val inputAgent = new Axi4WriteOnlyMasterAgent(dut.io.input, dut.clockDomain) {
      override def genAddress(): BigInt = ((Random.nextInt(1 << 19)))// & 0xFFF00) | 6

      override def bursts: List[Int] = List(1)

      override def mappingAllocate(mapping: SizeMapping): Boolean = {
        if(regions.exists(_.overlap(mapping))) return false
        regions += mapping
        true
      }

      override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
    }
    val outputAgent = new Axi4WriteOnlySlaveAgent(dut.io.output, dut.clockDomain)

    val writes = mutable.HashMap[BigInt, Byte]()
    val inputMonitor = new Axi4WriteOnlyMonitor(dut.io.input, dut.clockDomain) {
      override def onWriteByte(address: BigInt, data: Byte): Unit = {
        //          println(s"I $address -> $data")
        writes(address) = data
      }
    }

    val outputMonitor = new Axi4WriteOnlyMonitor(dut.io.output, dut.clockDomain) {
      override def onWriteByte(address: BigInt, data: Byte): Unit = {
        //          println(s"O $address -> $data")
        assert(writes(address) == data)
        writes.remove(address)
      }
    }


    dut.clockDomain.waitSamplingWhere(inputAgent.rspCounter > 10000)
    inputAgent.allowGen = false
    dut.clockDomain.waitSamplingWhere(!inputAgent.pending)
    dut.clockDomain.waitSampling(100)
    assert(writes.isEmpty)
    println("done")
  }

  test("writeOnly") {
    SimConfig.compile(new Axi4WriteOnlyIdRemover(Axi4Config(20, 32, 4))).doSim("test", 42)(writeTester)
  }

  def readTester(dut : Axi4ReadOnlyIdRemover): Unit ={
    dut.clockDomain.forkStimulus(10)

    val regions = mutable.Set[SizeMapping]()
    val inputAgent = new Axi4ReadOnlyMasterAgent(dut.io.input, dut.clockDomain) {
      override def genAddress(): BigInt = Random.nextInt(1 << 19)
      override def bursts: List[Int] = List(1)
      override def mappingAllocate(mapping: SizeMapping): Boolean = {
        if(regions.exists(_.overlap(mapping))) return false
        regions += mapping
        true
      }

      override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
    }

    val outputAgent = new Axi4ReadOnlySlaveAgent(dut.io.output, dut.clockDomain)



    val reads = mutable.HashMap[BigInt, Byte]()
    val inputMonitor = new Axi4ReadOnlyMonitor(dut.io.input, dut.clockDomain) {
      override def onReadByte(address: BigInt, data: Byte, id : Int): Unit = {
        reads(address) = data
      }
      override def onLast(id: Int): Unit = {}
    }

    val outputMonitor = new Axi4ReadOnlyMonitor(dut.io.output, dut.clockDomain) {
      override def onReadByte(address: BigInt, data: Byte, id : Int): Unit = {
        if(reads.contains(address)) {
          assert(reads(address) == data)
          reads.remove(address)
        }
      }

      override def onLast(id: Int): Unit = assert(reads.isEmpty)
    }



    dut.clockDomain.waitSamplingWhere(inputAgent.rspCounter > 10000)
    inputAgent.allowGen = false
    dut.clockDomain.waitSamplingWhere(!inputAgent.pending)
    dut.clockDomain.waitSampling(100)
    assert(reads.isEmpty)
    println("done")
  }

  test("readOnly") {
    SimConfig.compile(new Axi4ReadOnlyIdRemover(Axi4Config(20, 32, 4))).doSim("test", 42)(readTester)
  }

  def sharedTester(dut: Axi4SharedIdRemoverDut): Unit = {
    dut.clockDomain.forkStimulus(10)

    val readFork = fork {
      val regions = mutable.Set[SizeMapping]()
      val inputAgent = new Axi4ReadOnlyMasterAgent(dut.io.inputs.read, dut.clockDomain) {
        override def genAddress(): BigInt = Random.nextInt(1 << 19)

        override def bursts: List[Int] = List(1)

        override def mappingAllocate(mapping: SizeMapping): Boolean = {
          if (regions.exists(_.overlap(mapping))) return false
          regions += mapping
          true
        }

        override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
      }
      val outputAgent = new Axi4ReadOnlySlaveAgent(dut.io.outputs.read, dut.clockDomain)


      val reads = mutable.HashMap[BigInt, Byte]()
      val inputMonitor = new Axi4ReadOnlyMonitor(dut.io.inputs.read, dut.clockDomain) {
        override def onReadByte(address: BigInt, data: Byte, id: Int): Unit = {
          reads(address) = data
        }

        override def onLast(id: Int): Unit = {}
      }

      val outputMonitor = new Axi4ReadOnlyMonitor(dut.io.outputs.read, dut.clockDomain) {
        override def onReadByte(address: BigInt, data: Byte, id: Int): Unit = {
          if (reads.contains(address)) {
            assert(reads(address) == data)
            reads.remove(address)
          }
        }

        override def onLast(id: Int): Unit = assert(reads.isEmpty)
      }

      dut.clockDomain.waitSamplingWhere(inputAgent.rspCounter > 10000)
      inputAgent.allowGen = false
      dut.clockDomain.waitSamplingWhere(!inputAgent.pending)
      dut.clockDomain.waitSampling(100)
      assert(reads.isEmpty)
    }

    val writeFork = fork {
      val regions = mutable.Set[SizeMapping]()
      val inputAgent = new Axi4WriteOnlyMasterAgent(dut.io.inputs.write, dut.clockDomain) {
        override def genAddress(): BigInt = ((Random.nextInt(1 << 19))) // & 0xFFF00) | 6

        override def bursts: List[Int] = List(1)

        override def mappingAllocate(mapping: SizeMapping): Boolean = {
          if (regions.exists(_.overlap(mapping))) return false
          regions += mapping
          true
        }

        override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
      }
      val outputAgent = new Axi4WriteOnlySlaveAgent(dut.io.outputs.write, dut.clockDomain)

      val writes = mutable.HashMap[BigInt, Byte]()
      val inputMonitor = new Axi4WriteOnlyMonitor(dut.io.inputs.write, dut.clockDomain) {
        override def onWriteByte(address: BigInt, data: Byte): Unit = {
          //          println(s"I $address -> $data")
          writes(address) = data
        }
      }

      val outputMonitor = new Axi4WriteOnlyMonitor(dut.io.outputs.write, dut.clockDomain) {
        override def onWriteByte(address: BigInt, data: Byte): Unit = {
          //          println(s"O $address -> $data")
          assert(writes(address) == data)
          writes.remove(address)
        }
      }

      dut.clockDomain.waitSamplingWhere(inputAgent.rspCounter > 10000)
      inputAgent.allowGen = false
      dut.clockDomain.waitSamplingWhere(!inputAgent.pending)
      dut.clockDomain.waitSampling(100)
      assert(writes.isEmpty)
    }

    readFork.join()
    writeFork.join()

    println("done")
  }

  class Axi4SharedIdRemoverDut(config: Axi4Config) extends Component {

    val idRemover = new Axi4SharedIdRemover(config)

    val io = new Bundle {
      val inputs = new Bundle {
        val read = slave(Axi4ReadOnly(config))
        val write = slave(Axi4WriteOnly(config))
      }
      val outputs = new Bundle {
        val read = master(Axi4ReadOnly(idRemover.io.output.config))
        val write = master(Axi4WriteOnly(idRemover.io.output.config))
      }
    }

    val input = Axi4(config)
    input << io.inputs.read
    input << io.inputs.write

    idRemover.io.input << input.toShared()

    val output = idRemover.io.output.toAxi4()

    io.outputs.read << output
    io.outputs.write << output
  }

  test("shared") {
    SimConfig.compile(new Axi4SharedIdRemoverDut(Axi4Config(20, 32, 4))).doSim("test", 42)(sharedTester)
  }
}
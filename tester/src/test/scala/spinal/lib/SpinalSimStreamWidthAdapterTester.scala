package spinal.lib

import sim._

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.tester.SpinalSimFunSuite

import scala.collection.mutable
import scala.util.Random

class SpinalSimStreamWidthAdapterTester extends SpinalSimFunSuite {
  test("test2xOut") {
    //Compile the simulator
    val baseWidth = 32
    val compiled = SimConfig.allOptimisation.compile(rtl = new Component{
      val input = slave Stream(UInt(baseWidth bits))
      val output = master Stream(UInt(baseWidth*2 bits))
      val rtl = StreamWidthAdapter(input, output)
    }
    )

    //Run the simulation
    compiled.doSimUntilVoid { dut =>
      val queueModel = mutable.Queue[BigInt]()

      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)

      val driver = StreamDriver(dut.input, dut.clockDomain) { _ =>
        true
      }
      val inMonitor = StreamMonitor(dut.input, dut.clockDomain) { p => 
        queueModel.enqueue(p.toBigInt)
      }

      val ready = StreamReadyRandomizer(dut.output, dut.clockDomain)
      val outMonitor = StreamMonitor(dut.output, dut.clockDomain) { p =>
        assert(queueModel.nonEmpty)
        val low = queueModel.dequeue()
        assert(queueModel.nonEmpty)
        val high = queueModel.dequeue()
        val value = (high << baseWidth) + low
        assert(p.toBigInt == value)
      }
      
      dut.clockDomain.waitSampling(10000)
      simSuccess()
    }
  }

  test("test2xIn") {
    //Compile the simulator
    val baseWidth = 32
    val compiled = SimConfig.allOptimisation.compile(rtl = new Component{
      val input = slave Stream(UInt(baseWidth*2 bits))
      val output = master Stream(UInt(baseWidth bits))
      val rtl = StreamWidthAdapter(input, output)
    }
    )

    //Run the simulation
    compiled.doSimUntilVoid { dut =>
      val inQueue = mutable.Queue[BigInt]()
      val lowQueue = mutable.Queue[BigInt]()
      val highQueue = mutable.Queue[BigInt]()

      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)
      def check(){
        if(inQueue.nonEmpty && lowQueue.nonEmpty && highQueue.nonEmpty){
          val low = lowQueue.dequeue()
          val high = highQueue.dequeue()
          val value = (high << baseWidth) + low
          assert(inQueue.dequeue() == value)
        }
      }

      val driver = StreamDriver(dut.input, dut.clockDomain) { p =>
        true
      }
      val inMonitor = StreamMonitor(dut.input, dut.clockDomain) { p =>
        inQueue.enqueue(p.toBigInt)
        check()
      }

      val ready = StreamReadyRandomizer(dut.output, dut.clockDomain)
      val outMonitor = StreamMonitor(dut.output, dut.clockDomain) { p =>
        if (lowQueue.length <= highQueue.length){
          lowQueue.enqueue(p.toBigInt)
        } else {
          highQueue.enqueue(p.toBigInt)
        }
        check()
      }
      
      dut.clockDomain.waitSampling(10000)
      simSuccess()
    }
  }

  test("test2xOutHighFirst") {
    //Compile the simulator
    val baseWidth = 32
    val compiled = SimConfig.allOptimisation.compile(rtl = new Component{
      val input = slave Stream(UInt(baseWidth bits))
      val output = master Stream(UInt(baseWidth*2 bits))
      val rtl = StreamWidthAdapter(input, output, order = HIGHER_FIRST)
    }
    )

    //Run the simulation
    compiled.doSimUntilVoid { dut =>
      val queueModel = mutable.Queue[BigInt]()

      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)

      val driver = StreamDriver(dut.input, dut.clockDomain) { _ =>
        true
      }
      val inMonitor = StreamMonitor(dut.input, dut.clockDomain) { p => 
        queueModel.enqueue(p.toBigInt)
      }

      val ready = StreamReadyRandomizer(dut.output, dut.clockDomain)
      val outMonitor = StreamMonitor(dut.output, dut.clockDomain) { p =>
        assert(queueModel.nonEmpty)
        val high = queueModel.dequeue()
        assert(queueModel.nonEmpty)
        val low = queueModel.dequeue()
        val value = (high << baseWidth) + low
        assert(p.toBigInt == value)
      }
      
      dut.clockDomain.waitSampling(10000)
      simSuccess()
    }
  }

  test("test2xInHighFirst") {
    //Compile the simulator
    val baseWidth = 32
    val compiled = SimConfig.allOptimisation.compile(rtl = new Component{
      val input = slave Stream(UInt(baseWidth*2 bits))
      val output = master Stream(UInt(baseWidth bits))
      val rtl = StreamWidthAdapter(input, output, order = HIGHER_FIRST)
    }
    )

    //Run the simulation
    compiled.doSimUntilVoid { dut =>
      val inQueue = mutable.Queue[BigInt]()
      val lowQueue = mutable.Queue[BigInt]()
      val highQueue = mutable.Queue[BigInt]()

      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)
      def check(){
        if(inQueue.nonEmpty && lowQueue.nonEmpty && highQueue.nonEmpty){
          val low = lowQueue.dequeue()
          val high = highQueue.dequeue()
          val value = (high << baseWidth) + low
          assert(inQueue.dequeue() == value)
        }
      }

      val driver = StreamDriver(dut.input, dut.clockDomain) { p =>
        true
      }
      val inMonitor = StreamMonitor(dut.input, dut.clockDomain) { p =>
        inQueue.enqueue(p.toBigInt)
        check()
      }

      val ready = StreamReadyRandomizer(dut.output, dut.clockDomain)
      val outMonitor = StreamMonitor(dut.output, dut.clockDomain) { p =>
        if (lowQueue.length > highQueue.length){
          lowQueue.enqueue(p.toBigInt)
        } else {
          highQueue.enqueue(p.toBigInt)
        }
        check()
      }
      
      dut.clockDomain.waitSampling(10000)
      simSuccess()
    }
  }

  test("test2xOutWithNegPadding") {
    //Compile the simulator
    val baseWidth = 16
    val extraWidth = -2
    val compiled = SimConfig.allOptimisation.compile(rtl = new Component{
      val input = slave Stream(UInt(baseWidth bits))
      val output = master Stream(UInt(baseWidth*2+extraWidth bits))
      val rtl = StreamWidthAdapter(input, output, padding=true)
    }
    )

    //Run the simulation
    compiled.doSimUntilVoid { dut =>
      val queueModel = mutable.Queue[BigInt]()

      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)

      val driver = StreamDriver(dut.input, dut.clockDomain) { _ =>
        true
      }
      val inMonitor = StreamMonitor(dut.input, dut.clockDomain) { p => 
        queueModel.enqueue(p.toBigInt)
      }

      val ready = StreamReadyRandomizer(dut.output, dut.clockDomain)
      val outMonitor = StreamMonitor(dut.output, dut.clockDomain) { p =>
        assert(queueModel.nonEmpty)
        val low = queueModel.dequeue()
        assert(queueModel.nonEmpty)
        val high = queueModel.dequeue()
        val mask = ((BigInt(1) << dut.output.payload.getBitsWidth) - 1)
        val value = ((high << baseWidth) + low) & mask
        assert(p.toBigInt == value)
      }
      
      dut.clockDomain.waitSampling(10000)
      simSuccess()
    }
  }

  test("test2xInWithNegPadding") {
    //Compile the simulator
    val baseWidth = 16
    val extraWidth = -2
    val compiled = SimConfig.allOptimisation.compile(rtl = new Component{
      val input = slave Stream(UInt(baseWidth*2+extraWidth bits))
      val output = master Stream(UInt(baseWidth bits))
      val rtl = StreamWidthAdapter(input, output, padding=true)
    }
    )

    //Run the simulation
    compiled.doSimUntilVoid { dut =>
      val inQueue = mutable.Queue[BigInt]()
      val lowQueue = mutable.Queue[BigInt]()
      val highQueue = mutable.Queue[BigInt]()

      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)
      def check(){
        if(inQueue.nonEmpty && lowQueue.nonEmpty && highQueue.nonEmpty){
          val low = lowQueue.dequeue()
          val high = highQueue.dequeue()
          val value = (high << baseWidth) + low
          assert(inQueue.dequeue() == value)
        }
      }

      val driver = StreamDriver(dut.input, dut.clockDomain) { p =>
        true
      }
      val inMonitor = StreamMonitor(dut.input, dut.clockDomain) { p =>
        inQueue.enqueue(p.toBigInt)
        check()
      }

      val ready = StreamReadyRandomizer(dut.output, dut.clockDomain)
      val outMonitor = StreamMonitor(dut.output, dut.clockDomain) { p =>
        if (lowQueue.length <= highQueue.length){
          lowQueue.enqueue(p.toBigInt)
        } else {
          highQueue.enqueue(p.toBigInt)
        }
        check()
      }
      
      dut.clockDomain.waitSampling(10000)
      simSuccess()
    }
  }

  test("test2xInWithPosPadding") {
    //Compile the simulator
    val baseWidth = 16
    val extraWidth = 2
    val compiled = SimConfig.allOptimisation.compile(rtl = new Component{
      val input = slave Stream(UInt(baseWidth*2+extraWidth bits))
      val output = master Stream(UInt(baseWidth bits))
      val rtl = StreamWidthAdapter(input, output, padding=true)
    }
    )

    //Run the simulation
    compiled.doSimUntilVoid { dut =>
      val inQueue = mutable.Queue[BigInt]()
      val lowQueue = mutable.Queue[BigInt]()
      val midQueue = mutable.Queue[BigInt]()
      val highQueue = mutable.Queue[BigInt]()

      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)
      def check(){
        if(inQueue.nonEmpty && lowQueue.nonEmpty && midQueue.nonEmpty && highQueue.nonEmpty){
          val low = lowQueue.dequeue()
          val mid = midQueue.dequeue()
          val high = highQueue.dequeue()
          val value = (high << baseWidth*2) + (mid << baseWidth) + low
          assert(inQueue.dequeue() == value)
        }
      }

      val driver = StreamDriver(dut.input, dut.clockDomain) { p =>
        true
      }
      val inMonitor = StreamMonitor(dut.input, dut.clockDomain) { p =>
        inQueue.enqueue(p.toBigInt)
        check()
      }

      val ready = StreamReadyRandomizer(dut.output, dut.clockDomain)
      val outMonitor = StreamMonitor(dut.output, dut.clockDomain) { p =>
        if (lowQueue.isEmpty || lowQueue.length < midQueue.length){
          lowQueue.enqueue(p.toBigInt)
        } else if (midQueue.isEmpty || midQueue.length < highQueue.length){
          midQueue.enqueue(p.toBigInt)
        } else {
          highQueue.enqueue(p.toBigInt)
        }
        check()
      }
      
      dut.clockDomain.waitSampling(10000)
      simSuccess()
    }
  }

  test("test2xOutWithPosPadding") {
    //Compile the simulator
    val baseWidth = 16
    val extraWidth = 2
    val compiled = SimConfig.allOptimisation.compile(rtl = new Component{
      val input = slave Stream(UInt(baseWidth bits))
      val output = master Stream(UInt(baseWidth*2+extraWidth bits))
      val rtl = StreamWidthAdapter(input, output, padding=true)
    }
    )

    //Run the simulation
    compiled.doSimUntilVoid { dut =>
      val queueModel = mutable.Queue[BigInt]()

      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)

      val driver = StreamDriver(dut.input, dut.clockDomain) { _ =>
        true
      }
      val inMonitor = StreamMonitor(dut.input, dut.clockDomain) { p => 
        queueModel.enqueue(p.toBigInt)
      }

      val ready = StreamReadyRandomizer(dut.output, dut.clockDomain)
      val outMonitor = StreamMonitor(dut.output, dut.clockDomain) { p =>
        assert(queueModel.nonEmpty)
        val low = queueModel.dequeue()
        assert(queueModel.nonEmpty)
        val mid = queueModel.dequeue()
        assert(queueModel.nonEmpty)
        val high = queueModel.dequeue()
        val mask = ((BigInt(1) << dut.output.payload.getBitsWidth) - 1)
        val value = ((high << baseWidth*2) + (mid << baseWidth) + low) & mask
        assert(p.toBigInt == value)
      }
      
      dut.clockDomain.waitSampling(10000)
      simSuccess()
    }
  }

  test("test2xOutHighFirstPadding") {
    //Compile the simulator
    val baseWidth = 32
    val extraWidth = -2
    val compiled = SimConfig.allOptimisation.compile(rtl = new Component{
      val input = slave Stream(UInt(baseWidth bits))
      val output = master Stream(UInt(baseWidth*2+extraWidth bits))
      val rtl = StreamWidthAdapter(input, output, order = HIGHER_FIRST, padding=true)
    }
    )

    //Run the simulation
    compiled.doSimUntilVoid { dut =>
      val queueModel = mutable.Queue[BigInt]()

      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)

      val driver = StreamDriver(dut.input, dut.clockDomain) { _ =>
        true
      }
      val inMonitor = StreamMonitor(dut.input, dut.clockDomain) { p => 
        queueModel.enqueue(p.toBigInt)
      }

      val ready = StreamReadyRandomizer(dut.output, dut.clockDomain)
      val outMonitor = StreamMonitor(dut.output, dut.clockDomain) { p =>
        assert(queueModel.nonEmpty)
        val high = queueModel.dequeue()
        assert(queueModel.nonEmpty)
        val low = queueModel.dequeue()
        val mask = ((BigInt(1) << dut.output.payload.getBitsWidth) - 1)
        val value = ((high << baseWidth) + low) & mask
        assert(p.toBigInt == value)
      }
      
      dut.clockDomain.waitSampling(10000)
      simSuccess()
    }
  }

  test("test2xInHighFirstPadding") {
    //Compile the simulator
    val baseWidth = 32
    val extraWidth = -2
    val compiled = SimConfig.allOptimisation.compile(rtl = new Component{
      val input = slave Stream(UInt(baseWidth*2+extraWidth bits))
      val output = master Stream(UInt(baseWidth bits))
      val rtl = StreamWidthAdapter(input, output, order = HIGHER_FIRST, padding=true)
    }
    )

    //Run the simulation
    compiled.doSimUntilVoid { dut =>
      val inQueue = mutable.Queue[BigInt]()
      val lowQueue = mutable.Queue[BigInt]()
      val highQueue = mutable.Queue[BigInt]()

      SimTimeout(1000000 * 8)
      dut.clockDomain.forkStimulus(2)
      def check(){
        if(inQueue.nonEmpty && lowQueue.nonEmpty && highQueue.nonEmpty){
          val low = lowQueue.dequeue()
          val high = highQueue.dequeue()
          val mask = ((BigInt(1) << dut.output.payload.getBitsWidth) - 1)
          val value = ((high << baseWidth) + low) & mask
          assert(inQueue.dequeue() == value)
        }
      }

      val driver = StreamDriver(dut.input, dut.clockDomain) { p =>
        true
      }
      val inMonitor = StreamMonitor(dut.input, dut.clockDomain) { p =>
        inQueue.enqueue(p.toBigInt)
        check()
      }

      val ready = StreamReadyRandomizer(dut.output, dut.clockDomain)
      val outMonitor = StreamMonitor(dut.output, dut.clockDomain) { p =>
        if (lowQueue.length > highQueue.length){
          lowQueue.enqueue(p.toBigInt)
        } else {
          highQueue.enqueue(p.toBigInt)
        }
        check()
      }
      
      dut.clockDomain.waitSampling(10000)
      simSuccess()
    }
  }
}

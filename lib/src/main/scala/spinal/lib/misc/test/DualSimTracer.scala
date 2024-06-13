package spinal.lib.misc.test

import spinal.core._
import spinal.core.sim._

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext

/**
 * Double simulation, one ahead of the other which will trigger wave capture of the second simulation when it fail
 */
object DualSimTracer {
  def apply[T <: Component](compiled: SimCompiled[T], window: Long, seed: Int)(testbench: T => Unit): Unit = withCb(compiled, window, seed) { (dut, _) => testbench(dut) }
  def withCb[T <: Component](compiled: SimCompiled[T], window: Long, seed: Int, dualSimEnable : Boolean)(testbench: (T, (=> Unit) => Unit) => Unit): Unit = {
    dualSimEnable match {
      case true => DualSimTracer.withCb(compiled, window, seed)(testbench)
      case false => {
        val traceCallbacks = ArrayBuffer[() => Unit]()
        compiled.doSimUntilVoid(seed = seed) { dut => testbench(dut, f => traceCallbacks += (() => f)); traceCallbacks.foreach(_())}
      }
    }
  }

  def withCb[T <: Component](compiled: SimCompiled[T], window: Long, seed: Int)(testbench: (T, (=> Unit) => Unit) => Unit): Unit = {
    var mTime = 0l
    var mEnded = false
    var explorerFailed = false

    implicit val ec = ExecutionContext.global

    val explorer = new AsyncJob(toStdout = true, logsPath = new File(compiled.compiledPath, "explorer")) ({
      try {
        compiled.doSimUntilVoid(name = s"explorer", seed = seed) { dut =>
          disableSimWave()
          periodicaly(window) {
            mTime = simTime()
          }
          onSimEnd {
            mTime = simTime()
            mEnded = true
          }
          testbench(dut, cb => {})
        }
        println("Explorer success")
      } catch {
        case e: Throwable => explorerFailed = true; throw e
      }
    })

    val tracer = new AsyncJob(toStdout = false, logsPath = new File(compiled.compiledPath, "tracer"))({
      val traceCallbacks = ArrayBuffer[() => Unit]()
      compiled.doSimUntilVoid(name = s"tracer", seed = seed) { dut =>
        disableSimWave()
        fork {
          sleep(0)
          while (true) {
            while (simTime + window * 2 >= mTime && !mEnded) {
              Thread.sleep(100, 0)
            }
            if (mEnded && explorerFailed) {
              sleep((mTime - simTime - window) max 0)
              enableSimWave()
              traceCallbacks.foreach(_())
              sleep(window + 1000)
              simFailure("slave thread didn't ended ????")
            }
            sleep(window)
          }
        }
        println("Tracer success")
        testbench(dut, callback => traceCallbacks += (() => callback))
      }
    })

    explorer.join()
    tracer.join()

    assert(explorer.failed == tracer.failed)

    if(tracer.failed){
      throw new Exception(s"Dual sim reached end with failure, see ${tracer.logsPath.getAbsolutePath}")
    } else {
      println("Done")
    }

  }

}

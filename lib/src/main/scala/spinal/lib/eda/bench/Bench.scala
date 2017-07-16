package spinal.lib.eda.bench
import spinal.core._
import java.util.concurrent.ForkJoinPool

import spinal.lib.StreamFifo

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
 * Created by PIC32F_USER on 16/07/2017.
 */

trait Rtl {
  def getName(): String
  def getRtlPath() : String
}



object Bench{
  def apply(rtls : Seq[Rtl], targets : Seq[Target], workspacesRoot : String): Unit ={
    import scala.concurrent.ExecutionContext
    implicit val ec = ExecutionContext.fromExecutorService(
      new ForkJoinPool(Math.max(1,Runtime.getRuntime().availableProcessors()*3/4), ForkJoinPool.defaultForkJoinWorkerThreadFactory, null, true)
    )

    val results = (for (rtl <- rtls) yield {
      (rtl -> (for (target <- targets) yield {
        val ret = (target -> Future{target.synthesise(rtl, workspacesRoot + rtl.getName().replace(" ", "") + "_" + target.getFamilyName().replace(" ", ""))})
//        Thread.sleep(8000)
        ret
      }).toMap)
    }).toMap

    for (rtl <- rtls) {
      for (target <- targets) {
        Await.ready(results(rtl)(target), Duration.Inf)
      }
    }

    for (rtl <- rtls) {
      println(s"${rtl.getName()} ->")
      for (target <- targets) {
        val report = results(rtl)(target).value.get.get
        println(s"${target.getFamilyName()} -> ${(report.getFMax / 1e6).toInt} Mhz ${report.getArea()}")
      }
    }
  }


  //Demo
  def main(args: Array[String]) {
    val fifo128 = new Rtl {
      override def getName(): String = "Fifo 128"
      override def getRtlPath(): String = "fifo128.vhd"

      SpinalVhdl({
        StreamFifo(Bits(32 bits), 128).setDefinitionName(getRtlPath().split("\\.").head)
      })
    }

    val fifo1024 = new Rtl {
      override def getName(): String = "Fifo 1024"
      override def getRtlPath(): String = "fifo1024.vhd"

      SpinalVhdl({
        StreamFifo(Bits(32 bits), 1024).setDefinitionName(getRtlPath().split("\\.").head)
      })
    }

    val rtls = List(fifo128,fifo1024)

    val targets = AlteraStdTargets(
      quartusCycloneIIPath = "D:/altera/13.0sp1/quartus/bin64",
      quartusCycloneIVPath = "D:/altera_lite/15.1/quartus/bin64",
      quartusCycloneVPath  = "D:/altera_lite/15.1/quartus/bin64"
    )

    Bench(rtls, targets, "E:/tmp/")
  }
}
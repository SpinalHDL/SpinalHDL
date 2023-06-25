package spinal.lib.eda.bench
import spinal.core._
import spinal.lib.{KeepAttribute, StreamFifo}
import spinal.sim.SimManager

import java.util.concurrent.ForkJoinPool
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.collection.Seq

/**
 * Created by PIC32F_USER on 16/07/2017.
 */

trait Rtl {
  /** Name */
  def getName(): String
  /** Path of top module RTL */
  def getRtlPath() : String = getRtlPaths().head
  /** A List of RTL paths */
  def getRtlPaths() : Seq[String] = Seq(getRtlPath())
  /** The top module name, defaulting to the file name of top module RTL */
  def getTopModuleName() : String = getRtlPath().split("\\.").head
}

object Rtl {
  /** Create Rtl from SpinalReport */
  def apply[T <: Component](rtl: SpinalReport[T]): Rtl = {
    new Rtl {
      override def getName(): String = rtl.toplevelName
      override def getRtlPaths(): Seq[String] = rtl.rtlSourcesPaths.toSeq
      override def getTopModuleName(): String = rtl.toplevelName
    }
  }

  def ffIo[T <: Component](c : T): T ={
    def buf1[T <: Data](that : T) = KeepAttribute(RegNext(that)).addAttribute("DONT_TOUCH")
    def buf[T <: Data](that : T) = buf1(buf1(buf1(that)))
    c.rework{
      val ios = c.getAllIo.toList
      ios.foreach{io =>
        if(io.getName() == "clk"){

        } else if(io.isInput){
          io.setAsDirectionLess().allowDirectionLessIo
          io := buf(in(cloneOf(io).setName(io.getName() + "_wrap")))
        } else if(io.isOutput){
          io.setAsDirectionLess().allowDirectionLessIo
          out(cloneOf(io).setName(io.getName() + "_wrap")) := buf(io)
        } else ???
      }
    }
    c
  }

  def xorOutputs[T <: Component](c : T): T ={
    def buf1[T <: Data](that : T) = KeepAttribute(RegNext(that)).addAttribute("DONT_TOUCH")
    def buf[T <: Data](that : T) = buf1(buf1(buf1(that)))
    c.rework{
      val outputs = c.getAllIo.toList.filter(_.isOutput)
      val bools = outputs.flatMap(_.asBits.asBools)
      outputs.foreach(_.setAsDirectionLess())

      val agreg = out(B(bools).xorR)
    }
    c
  }
}

object Bench {
  def apply(rtls : Seq[Rtl], targets : Seq[Target], workspacesRoot : String = sys.env.getOrElse("SPINAL_BENCH_WORKSPACE", null)): Unit ={
    import scala.concurrent.ExecutionContext
    implicit val ec = ExecutionContext.fromExecutorService(
      new ForkJoinPool(Math.max(1, SimManager.cpuCount / 2), ForkJoinPool.defaultForkJoinWorkerThreadFactory, null, true)
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
        try{
          val report = results(rtl)(target).value.get.get
          println(s"${target.getFamilyName()} -> ${(report.getFMax / 1e6).toInt} Mhz ${report.getArea()}")
        } catch {
          case t : Throwable =>  println(s"${target.getFamilyName()} -> FAILED")
        }
      }
    }
  }


  //Demo
  def main(args: Array[String]) {
    val fifo128 = new Rtl {
      override def getName(): String = "Fifo 128"
      override def getRtlPath(): String = "fifo128.v"

      SpinalVerilog({
        StreamFifo(Bits(32 bits), 128).setDefinitionName(getRtlPath().split("\\.").head)
      })
    }

    val fifo1024 = new Rtl {
      override def getName(): String = "Fifo 1024"
      override def getRtlPath(): String = "fifo1024.v"

      SpinalVerilog({
        StreamFifo(Bits(32 bits), 1024).setDefinitionName(getRtlPath().split("\\.").head)
      })
    }

    import spinal.lib._
    val rtls = List(128, 1024).map(depth => Rtl(SpinalVerilog(new Component {
      setDefinitionName(s"fifo$depth")
      val push = slave Stream(Bits(32 bits))
      val pop = master Stream(Bits(32 bits))

      val f = new StreamFifo(Bits(32 bits), depth)
      f.logic.ram.addAttribute("ram_style", "block")
      f.io.push << push.halfPipe()
      f.io.pop.halfPipe() >> pop
    })))

    //    val targets = AlteraStdTargets(
    //      quartusCycloneIIPath = "D:/altera/13.0sp1/quartus/bin64",
    //      quartusCycloneIVPath = "D:/altera_lite/15.1/quartus/bin64",
    //      quartusCycloneVPath  = "D:/altera_lite/15.1/quartus/bin64"
    //    ) ++ XilinxStdTargets(
    //      vivadoArtix7Path = "E:\\Xilinx\\Vivado\\2016.3\\bin"
    //    )

    val targets =  XilinxStdTargets() ++ AlteraStdTargets()
    Bench(rtls, targets)

    /*
    fifo128 ->
Artix 7 -> 242 Mhz 23 LUT 50 FF
Artix 7 -> 335 Mhz 32 LUT 50 FF
Cyclone V -> 216 Mhz 23 ALMs
Cyclone IV -> 219 Mhz 30 LUT 50 FF
fifo1024 ->
Artix 7 -> 195 Mhz 36 LUT 56 FF
Artix 7 -> 263 Mhz 45 LUT 56 FF
Cyclone V -> 195 Mhz 26 ALMs
Cyclone IV -> 205 Mhz 40 LUT 56 FF

fifo128 ->
Artix 7 -> 243 Mhz 29 LUT 50 FF
Artix 7 -> 354 Mhz 38 LUT 50 FF
Cyclone V -> 249 Mhz 30 ALMs
Cyclone IV -> 250 Mhz 51 LUT 50 FF
fifo1024 ->
Artix 7 -> 240 Mhz 41 LUT 56 FF
Artix 7 -> 285 Mhz 48 LUT 56 FF
Cyclone V -> 216 Mhz 41 ALMs
Cyclone IV -> 217 Mhz 68 LUT 56 FF

fifo128 ->
Artix 7 -> 248 Mhz 32 LUT 63 FF
Artix 7 -> 357 Mhz 37 LUT 63 FF
Cyclone V -> 275 Mhz 30 ALMs
Cyclone IV -> 250 Mhz 49 LUT 63 FF
fifo1024 ->
Artix 7 -> 251 Mhz 41 LUT 75 FF
Artix 7 -> 296 Mhz 48 LUT 75 FF
Cyclone V -> 230 Mhz 39 ALMs
Cyclone IV -> 227 Mhz 68 LUT 75 FF

fifo128 ->
Artix 7 -> 374 Mhz 23 LUT 51 FF
Artix 7 -> 415 Mhz 29 LUT 51 FF
Cyclone V -> 275 Mhz 23 ALMs
Cyclone IV -> 226 Mhz 72 LUT 195 FF
fifo1024 ->
Artix 7 -> 250 Mhz 29 LUT 57 FF
Artix 7 -> 347 Mhz 33 LUT 57 FF
Cyclone V -> 275 Mhz 26 ALMs
Cyclone IV -> 235 Mhz 82 LUT 207 FF

fifo128 ->
Artix 7 -> 297 Mhz 25 LUT 59 FF
Artix 7 -> 420 Mhz 31 LUT 59 FF
Cyclone V -> 275 Mhz 24 ALMs
Cyclone IV -> 243 Mhz 76 LUT 203 FF
fifo1024 ->
Artix 7 -> 265 Mhz 31 LUT 68 FF
Artix 7 -> 364 Mhz 33 LUT 68 FF
Cyclone V -> 275 Mhz 31 ALMs
Cyclone IV -> 234 Mhz 89 LUT 218 FF


X
fifo128 ->
Artix 7 -> 374 Mhz 83 LUT 228 FF
Artix 7 -> 444 Mhz 119 LUT 228 FF
Cyclone V -> 263 Mhz 87 ALMs
fifo1024 ->
Artix 7 -> 355 Mhz 98 LUT 237 FF
Artix 7 -> 435 Mhz 134 LUT 237 FF
Cyclone V -> 275 Mhz 92 ALMs


fifo128 ->
Artix 7 -> 437 Mhz 52 LUT 65 FF
Artix 7 -> 447 Mhz 59 LUT 65 FF
Cyclone V -> 275 Mhz 50 ALMs

fifo1024 ->
Artix 7 -> 382 Mhz 70 LUT 77 FF
Artix 7 -> 454 Mhz 79 LUT 77 FF
Cyclone V -> 275 Mhz 68 ALMs


     */
  }

  def compressIo[T <: Component](c : T) : T = {
    c.rework{
      var outputXor = False
      for(output <- c.getOrdredNodeIo.filter(_.isOutput)){
        output.setAsDirectionLess().allowDirectionLessIo
        for(outputBit <- output.asBits.asBools){
          outputXor \= outputXor ^ outputBit
        }
      }
      outputXor.asOutput().setName("finalOutput")


    }
    c
  }
}
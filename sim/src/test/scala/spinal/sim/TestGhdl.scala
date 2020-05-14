package spinal.sim

import java.math.BigInteger
import spinal.sim.vpi._
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import sys.process._
import java.lang.RuntimeException
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object TestGhdl1 extends App{
  val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "adder.vhd"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD

  val (ghdlbackend, _) = new GhdlBackend(config).instanciate
  println(ghdlbackend.print_signals())
  val nibble1 = ghdlbackend.get_signal_handle("adder.nibble1")
  val nibble2 = ghdlbackend.get_signal_handle("adder.nibble2")
  val sum = ghdlbackend.get_signal_handle("adder.sum")
  ghdlbackend.write32(nibble1, 0)
  ghdlbackend.eval
  ghdlbackend.write32(nibble1, 3)
  ghdlbackend.write32(nibble2, 5)
  println("? = " + ghdlbackend.read32(nibble1).toString)
  ghdlbackend.eval
  println("3 = " + ghdlbackend.read32(nibble1).toString)
  println("3 + 5 = " + ghdlbackend.read32(sum).toString)
  ghdlbackend.write64(nibble1, 4)
  ghdlbackend.write64(nibble2, 1)
  ghdlbackend.sleep(3)
  println("4 + 1 = " + ghdlbackend.read64(sum).toString)
  ghdlbackend.write(nibble1, new VectorInt8(BigInt(2).toByteArray))
  ghdlbackend.write(nibble2, new VectorInt8(BigInt(3).toByteArray))
  ghdlbackend.eval
  println("2 + 3 = " + BigInt(ghdlbackend.read(sum)
                                         .asScala
                                         .toArray
                                         .map{x => x.toByte}).toString)
  ghdlbackend.close
  println("Finished TestGhdl1")
}

object TestGhdl2 extends App{
    val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "adder.vhd"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD

  val (ghdlbackend, _) = new GhdlBackend(config).instanciate

  for(i <- 0l to 2000000000l){
    ghdlbackend.eval
    ghdlbackend.eval
    ghdlbackend.eval
    ghdlbackend.eval
    ghdlbackend.sleep(10)
    ghdlbackend.sleep(10)
    ghdlbackend.sleep(10)
    ghdlbackend.sleep(10)
    ghdlbackend.sleep(10)
    ghdlbackend.sleep(10)
    ghdlbackend.eval
    ghdlbackend.sleep(10)
    ghdlbackend.eval
    ghdlbackend.sleep(10)
    ghdlbackend.eval
    ghdlbackend.sleep(10)
    ghdlbackend.eval
    ghdlbackend.sleep(10)
    ghdlbackend.eval
    ghdlbackend.sleep(10)
    ghdlbackend.eval
    ghdlbackend.sleep(10)
    ghdlbackend.eval
    ghdlbackend.eval
  }
  ghdlbackend.close
  println("Finished StressGhdl1")
}

object TestGhdl3 extends App{
  val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "adder.vhd"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD

  val r = new Random()
  val (ghdlbackend, _) = new GhdlBackend(config).instanciate
  val nibble1 = ghdlbackend.get_signal_handle("adder.nibble1")
  val nibble2 = ghdlbackend.get_signal_handle("adder.nibble2")
  val sum = ghdlbackend.get_signal_handle("adder.sum")
  for(i <- 0 to 100000){
    var n1 = r.nextInt(16)
    var n2 = r.nextInt(16-n1)
    ghdlbackend.write32(nibble1, n1)
    ghdlbackend.write32(nibble2, n2)
    ghdlbackend.eval
    println(n1.toString + " = " + ghdlbackend.read32(nibble1).toString)
    println(n1.toString + " + " + n2.toString + " = " + ghdlbackend.read32(sum).toString)
    n1 = r.nextInt(16)
    n2 = r.nextInt(16-n1)
    ghdlbackend.write64(nibble1, n1)
    ghdlbackend.write64(nibble2, n2)
    ghdlbackend.sleep(3)
    println(n1.toString + " + " + n2.toString + " = " + ghdlbackend.read64(sum).toString)
    n1 = r.nextInt(16)
    n2 = r.nextInt(16-n1)
    ghdlbackend.write(nibble1, new VectorInt8(BigInt(n1).toByteArray))
    ghdlbackend.write(nibble2, new VectorInt8(BigInt(n2).toByteArray))
    ghdlbackend.eval
    println(n1.toString + " + " + n2.toString + " = " + BigInt(ghdlbackend.read(sum)
                                                                 .asScala
                                                                 .toArray
                                                                 .map{x => x.toByte}).toString)
  }
  ghdlbackend.close
  println("Finished TestGhdl2")
}

object TestGhdl4 extends App{
  val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "adder.vhd"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD

  val r = new Random()
  val (ghdlbackend, _) = new GhdlBackend(config).instanciate
  val nibble1 = ghdlbackend.get_signal_handle("adder.nibble1")
  val nibble2 = ghdlbackend.get_signal_handle("adder.nibble2")
  val sum = ghdlbackend.get_signal_handle("adder.sum")
  ghdlbackend.close
  try {
    println(ghdlbackend.read(sum))
  }
  catch {
    case e: VpiException => {
      println("catched exception")
      println(e.toString)
    }
  }

  println("Finished TestGhdl4")
}

object TestGhdl5 extends App {
  val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "adder.vhd"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD

  val r = new Random()
  val (ghdlbackend, _) = new GhdlBackend(config).instanciate
  try {
    val sum = ghdlbackend.get_signal_handle("adder.yolo")
    println(ghdlbackend.read(sum))
  }
  catch {
    case e: VpiException => {
      println("catched exception")
      println(e.toString)
    }
  }

  println("Finished TestGhdl5")
}

object TestGhdl6 extends App{
  val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "adder.vhd"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD

  val simVpi = new SimVpi(new GhdlBackend(config))
  val nibble1 = new Signal(Seq("adder","nibble1"), new UIntDataType(4))
  val nibble2 = new Signal(Seq("adder","nibble2"), new UIntDataType(4))
  val sum = new Signal(Seq("adder","sum"), new UIntDataType(4))
  simVpi.eval
  simVpi.setInt(nibble1, 3)
  simVpi.setInt(nibble2, 5)
  simVpi.eval
  println("3 + 5 = " + simVpi.getInt(sum).toString)
  simVpi.setLong(nibble1, 4)
  simVpi.setLong(nibble2, 1)
  simVpi.sleep(3)
  println("4 + 1 = " + simVpi.getLong(sum).toString)
  simVpi.setBigInt(nibble1, BigInt(2))
  simVpi.setBigInt(nibble2, BigInt(3))
  simVpi.eval
  println("2 + 3 = " + simVpi.getBigInt(sum).toString)
  simVpi.end
  println("Finished TestGhdl6")
}

object TestGhdl7 extends App{
  val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "adder.vhd"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD

  val simVpi = new SimVpi(new GhdlBackend(config))
  val nibble1 = new Signal(Seq("adder","nibble1"), new UIntDataType(4))
  val nibble2 = new Signal(Seq("adder","nibble2"), new UIntDataType(4))
  val sum = new Signal(Seq("adder","sum"), new UIntDataType(4))
  simVpi.eval
  simVpi.setInt(nibble1, 3)
  simVpi.setInt(nibble2, 5)
  simVpi.eval
  println("3 + 5 = " + simVpi.getInt(sum).toString)
  simVpi.setLong(nibble1, 4)
  simVpi.setLong(nibble2, 1)
  simVpi.sleep(3)
  println("4 + 1 = " + simVpi.getLong(sum).toString)
  simVpi.setBigInt(nibble1, BigInt(2))
  simVpi.setBigInt(nibble2, BigInt(3))
  simVpi.eval
  println("2 + 3 = " + simVpi.getBigInt(sum).toString)
  println("Finished TestGhdl7")
}

object TestGhdl8 extends App{
  val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "toplevel.vhd"
  config.toplevelName = "toplevel"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD

  val (ghdlbackend, _) = new GhdlBackend(config).instanciate
  println(ghdlbackend.print_signals())
  ghdlbackend.eval
  ghdlbackend.close
  println("Finished TestGhdl8")
}

object TestGhdl9 extends App{
    val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "adder.vhd"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD

  val (ghdlbackend, _) = new GhdlBackend(config).instanciate
  val nibble1 = ghdlbackend.get_signal_handle("adder.nibble1")
  val nibble2 = ghdlbackend.get_signal_handle("adder.nibble2")
  val sum = ghdlbackend.get_signal_handle("adder.sum")
  
  for(i <- 0l to 1024l){
    ghdlbackend.randomize(i)
    ghdlbackend.sleep(1)
    println(ghdlbackend.read32(nibble1).toString + " " + 
            ghdlbackend.read32(nibble2).toString + " " + 
            ghdlbackend.read32(sum).toString)
  }
  ghdlbackend.close
  println("Finished StressGhdl9")
}

object TestGhdl10 extends App {
  val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "adder.vhd"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.NONE
  val (ghdlbackend, _) = new GhdlBackend(config).instanciate
  val nibble1 = ghdlbackend.get_signal_handle("adder.nibble1")
  val nibble2 = ghdlbackend.get_signal_handle("adder.nibble2")
  val sum = ghdlbackend.get_signal_handle("adder.sum")

   for(i <- 0l to 1000000l){
    ghdlbackend.randomize(i) 
    ghdlbackend.sleep(1)
    ghdlbackend.write32(nibble1, 1)
    ghdlbackend.write32(nibble1, 2)
    ghdlbackend.read32(sum)
  }
  ghdlbackend.close
  println("Finished TestGhdl10")
}

object TestGhdl11 extends App {
  val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "toplevel.vhd"
  config.toplevelName = "toplevel"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD
  config.useCache = true

  val (ghdlbackend, _) = GhdlBackend.getGCC(config).instanciate
   for(i <- 0l to 1000000l){
    ghdlbackend.randomize(i)
    ghdlbackend.sleep(1)
  }
  ghdlbackend.close
  println("Finished TestGhdl11")
}

object TestGhdl12 extends App {
  val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "toplevel.vhd"
  config.toplevelName = "toplevel"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD
  config.useCache = true

  val (ghdlbackend, _) = GhdlBackend.getLLVM(config).instanciate
   for(i <- 0l to 1000000l){
    ghdlbackend.randomize(i)
    ghdlbackend.sleep(1)
  }
  ghdlbackend.close
  println("Finished TestGhdl12")
}


object TestGhdl13 extends App {
  val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "toplevel.vhd"
  config.toplevelName = "toplevel"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD
  config.useCache = true

  val (ghdlbackend, _) = GhdlBackend.getMCODE(config).instanciate
   for(i <- 0l to 1000000l){
    ghdlbackend.randomize(i)
    ghdlbackend.sleep(1)
  }
  ghdlbackend.close
  println("Finished TestGhdl13")
}

object TestGhdl14 extends App{
  val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "adder.vhd"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD

  val backendFactory = new GhdlBackend(config)
  var (ghdlbackend, _) = backendFactory.instanciate
  var nibble1 = ghdlbackend.get_signal_handle("adder.nibble1")
  var nibble2 = ghdlbackend.get_signal_handle("adder.nibble2")
  var sum = ghdlbackend.get_signal_handle("adder.sum")
  ghdlbackend.write32(nibble1, 3)
  ghdlbackend.write32(nibble2, 5)
  ghdlbackend.eval
  println("3 + 5 = " + ghdlbackend.read32(sum).toString)
  ghdlbackend.close

  config.wavePath = "test2.vcd"
  val (ghdlbackend2, _) = backendFactory.instanciate
  nibble1 = ghdlbackend2.get_signal_handle("adder.nibble1")
  nibble2 = ghdlbackend2.get_signal_handle("adder.nibble2")
  sum = ghdlbackend2.get_signal_handle("adder.sum")
  ghdlbackend2.write32(nibble1, 3)
  ghdlbackend2.write32(nibble2, 5)
  ghdlbackend2.eval
  println("3 + 5 = " + ghdlbackend2.read32(sum).toString)
  ghdlbackend2.close

  config.wavePath = "test3.vcd"
  val (ghdlbackend3, _) = backendFactory.instanciate
  nibble1 = ghdlbackend3.get_signal_handle("adder.nibble1")
  nibble2 = ghdlbackend3.get_signal_handle("adder.nibble2")
  sum = ghdlbackend3.get_signal_handle("adder.sum")
  ghdlbackend3.write32(nibble1, 3)
  ghdlbackend3.write32(nibble2, 5)
  ghdlbackend3.eval
  println("3 + 5 = " + ghdlbackend3.read32(sum).toString)
  ghdlbackend3.close

  println("Finished TestGhdl14")
}

object TestGhdl15 extends App{
  val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "adder.vhd"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD
  config.useCache = true

  val backendFactory = new GhdlBackend(config)
  val runningSims = (0 to 7).map { i =>
    Future {
    val (ghdlbackend, _) = backendFactory.instanciate()
      val nibble1 = ghdlbackend.get_signal_handle("adder.nibble1")
      val nibble2 = ghdlbackend.get_signal_handle("adder.nibble2")
      val sum = ghdlbackend.get_signal_handle("adder.sum")
      ghdlbackend.write32(nibble1, i)
      ghdlbackend.write32(nibble2, i+1)
      ghdlbackend.eval
      val ret = Seq(i.toString, 
                "+", 
                (i+1).toString,
                "=",
                ghdlbackend.read32(sum).toString).mkString(" ")
      ghdlbackend.close
      ret
    }
  }.toArray


  runningSims.foreach { res => println(Await.result(res, Duration.Inf))}
  println("Finished TestGhdl15")
}

object TestGhdl16 extends App{
  val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "adder.vhd"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.waveFormat = WaveFormat.NONE
  config.useCache = false

  val backendFactory = new GhdlBackend(config)
 
  val ghdlbackends = (0 to 7).map { i =>
    val (ghdlbend, _) = backendFactory.instanciate()
    ghdlbend
  }.toArray

  val startAt = System.nanoTime 
  val runningSims = (0 to 7).map { i =>
    Future {
      val ghdlbackend = ghdlbackends(i)
      val nibble1 = ghdlbackend.get_signal_handle("adder.nibble1")
      val nibble2 = ghdlbackend.get_signal_handle("adder.nibble2")
      val sum = ghdlbackend.get_signal_handle("adder.sum")
      
      for(j <- 0 to 1000000) {
        ghdlbackend.write32(nibble1, i)
        ghdlbackend.write32(nibble2, i+1)
        ghdlbackend.eval
      }

      val ret = Seq(i.toString, 
                "+", 
                (i+1).toString,
                "=",
                ghdlbackend.read32(sum).toString).mkString(" ")
      ghdlbackend.close
      ret
    }
  }.toArray


  runningSims.foreach { res => println(Await.result(res, Duration.Inf))}
  val endAt = System.nanoTime
  println(((endAt - startAt) * 1e-6).toString + " ms")
  println("Finished TestGhdl16")
}


object TestGhdl17 extends App{
  val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "adder.vhd"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.waveFormat = WaveFormat.NONE
  config.useCache = false
  config.CFLAGS += " -DNO_SPINLOCK_YIELD_OPTIMIZATION "

  val backendFactory = new GhdlBackend(config)

  val ghdlbackends = (0 to 7).map { i =>
    val (ghdlbend, _) = backendFactory.instanciate()
    ghdlbend
  }.toArray
 
  val startAt = System.nanoTime 
  val runningSims = (0 to 7).map { i =>
    Future {
      val ghdlbackend = ghdlbackends(i)
      val nibble1 = ghdlbackend.get_signal_handle("adder.nibble1")
      val nibble2 = ghdlbackend.get_signal_handle("adder.nibble2")
      val sum = ghdlbackend.get_signal_handle("adder.sum")
      
      for(j <- 0 to 1000000) {
        ghdlbackend.write32(nibble1, i)
        ghdlbackend.write32(nibble2, i+1)
        ghdlbackend.eval
      }

      val ret = Seq(i.toString, 
                "+", 
                (i+1).toString,
                "=",
                ghdlbackend.read32(sum).toString).mkString(" ")
      ghdlbackend.close
      ret
    }
  }.toArray


  runningSims.foreach { res => println(Await.result(res, Duration.Inf))}
  val endAt = System.nanoTime
  println(((endAt - startAt) * 1e-6).toString + " ms")
  println("Finished TestGhdl17")
}

object TestGhdl18 extends App{
  val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "SpinalSimVerilatorIoTestTop.vhd"
  config.toplevelName = "SpinalSimVerilatorIoTestTop"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test3.vcd"
  config.waveFormat = WaveFormat.VCD

  val (ghdlbackend, _) = new GhdlBackend(config).instanciate
  val native_in = ghdlbackend.get_signal_handle("SpinalSimVerilatorIoTestTop.nativeEncoding_stateInput")
  val native_out = ghdlbackend.get_signal_handle("SpinalSimVerilatorIoTestTop.nativeEncoding_stateOutput")
  val native_decoded = ghdlbackend.get_signal_handle("SpinalSimVerilatorIoTestTop.nativeEncoding_stateDecoded")

  println("in -> 0")
  ghdlbackend.write32(native_in, 0)
  ghdlbackend.sleep(1)
  println(ghdlbackend.read32(native_in))
  println(ghdlbackend.read32(native_out))
  println(ghdlbackend.read32(native_decoded))
  println("in -> 1")
  ghdlbackend.write32(native_in, 1)
  ghdlbackend.sleep(1)
  println(ghdlbackend.read32(native_in))
  println(ghdlbackend.read32(native_out))
  println(ghdlbackend.read32(native_decoded))
  println("in -> 2")
  ghdlbackend.write32(native_in, 2)
  ghdlbackend.sleep(1)
  println(ghdlbackend.read32(native_in))
  println(ghdlbackend.read32(native_out))
  println(ghdlbackend.read32(native_decoded))
  println("in -> 3")
  ghdlbackend.write32(native_in, 3)
  ghdlbackend.sleep(1)
  println(ghdlbackend.read32(native_in))
  println(ghdlbackend.read32(native_out))
  println(ghdlbackend.read32(native_decoded))
  println("in -> 4")
  ghdlbackend.write32(native_in, 4)
  ghdlbackend.sleep(1)
  println(ghdlbackend.read32(native_in))
  println(ghdlbackend.read32(native_out))
  println(ghdlbackend.read32(native_decoded))
  ghdlbackend.close

  println("Finished TestGhdl18")
}



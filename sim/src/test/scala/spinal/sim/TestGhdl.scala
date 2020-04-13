package spinal.sim

import java.math.BigInteger
import spinal.sim.vpi._
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import sys.process._
import java.lang.RuntimeException

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
  config.rtlSourcesPaths += "toplevel.vhd"
  config.toplevelName = "toplevel"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD

  val (ghdlbackend, _) = new GhdlBackend(config).instanciate
   for(i <- 0l to 1024l){
    ghdlbackend.randomize(i)
    ghdlbackend.sleep(1)
  }
  ghdlbackend.close
  println("Finished TestGhdl10")
}


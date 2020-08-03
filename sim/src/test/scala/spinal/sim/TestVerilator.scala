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

object TestVerilator1 extends App{
  val config = new VerilatorBackendConfig()
  config.rtlSourcesPaths += "./yolo/rtl/TestMemVerilator.v"
  config.toplevelName = "TestMemVerilator"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.vcdPath = "./test.vcd"
  config.waveFormat = WaveFormat.VCD
  
  val addr    = new Signal(Seq("io_addr"), new BitsDataType(10))
  val wvalue  = new Signal(Seq("io_wvalue"), new BitsDataType(11))
  val wenable = new Signal(Seq("io_wenable"), new BoolDataType)
  val rvalue  = new Signal(Seq("io_rvalue"), new BitsDataType(11))
  val clk     = new Signal(Seq("clk"), new BoolDataType)
  val reset   = new Signal(Seq("reset"), new BoolDataType)
  val mem     = new Signal(Seq("TestMemVerilator", "mem"), new BitsDataType(11))

  addr.id    = 0 
  wvalue.id  = 1
  wenable.id = 2 
  rvalue.id  = 3 
  clk.id     = 4 
  reset.id   = 5 
  mem.id     = 6 

  config.signals += addr   
  config.signals += wvalue 
  config.signals += wenable
  config.signals += rvalue 
  config.signals += clk    
  config.signals += reset  
  config.signals += mem    

  val verilatorBackend = new VerilatorBackend(config)
  val handle = verilatorBackend.instanciate("dummy", 42)
  val simV = new SimVerilator(verilatorBackend, handle)

  simV.setLong(clk, 0)
  simV.setLong(reset, 0)
  simV.setLong(addr, 0)
  simV.setLong(wvalue, 0)
  simV.setLong(wenable, 1)
  simV.sleep(1)
  simV.eval

  for(i <- 0 to 1023){
    simV.setLong(addr, i)
    simV.setLong(wvalue, i)
    simV.sleep(1)
    simV.eval
    simV.setLong(clk, 1)
    simV.sleep(1)
    simV.eval
    simV.setLong(clk, 0)
    simV.sleep(1)
    simV.eval
  }

  simV.setLong(wenable, 0)
  simV.sleep(1)
  simV.eval

  for(i <- 0 to 1023){
    println(i.toString + " -> " + simV.getLongMem(mem, i).toString) // read from index 'i'
  }

  simV.end
  println("Finished TestVerilator1")
}

object TestVerilator2 extends App{
  val config = new VerilatorBackendConfig()
  config.rtlSourcesPaths += "./yolo/rtl/TestMemVerilator.v"
  config.toplevelName = "TestMemVerilator"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.vcdPath = "./test.vcd"
  config.waveFormat = WaveFormat.VCD
  
  val addr    = new Signal(Seq("io_addr"), new BitsDataType(10))
  val wvalue  = new Signal(Seq("io_wvalue"), new BitsDataType(11))
  val wenable = new Signal(Seq("io_wenable"), new BoolDataType)
  val rvalue  = new Signal(Seq("io_rvalue"), new BitsDataType(11))
  val clk     = new Signal(Seq("clk"), new BoolDataType)
  val reset   = new Signal(Seq("reset"), new BoolDataType)
  val mem     = new Signal(Seq("TestMemVerilator", "mem"), new BitsDataType(11))

  addr.id    = 0 
  wvalue.id  = 1
  wenable.id = 2 
  rvalue.id  = 3 
  clk.id     = 4 
  reset.id   = 5 
  mem.id     = 6 

  config.signals += addr   
  config.signals += wvalue 
  config.signals += wenable
  config.signals += rvalue 
  config.signals += clk    
  config.signals += reset  
  config.signals += mem    

  val verilatorBackend = new VerilatorBackend(config)
  val handle = verilatorBackend.instanciate("dummy", 42)
  val simV = new SimVerilator(verilatorBackend, handle)

  simV.setLong(clk, 0)
  simV.setLong(reset, 0)
  simV.setLong(addr, 0)
  simV.setLong(wvalue, 0)
  simV.setLong(wenable, 0)
  simV.sleep(1)
  simV.eval

  for(i <- 0 to 1023){
    simV.setLongMem(mem, i, i)
  }

  simV.sleep(1)
  simV.eval

  for(i <- 0 to 1023){
    simV.setLong(addr, i)
    simV.sleep(1)
    simV.eval
    simV.setLong(clk, 1)
    simV.sleep(1)
    simV.eval
    simV.setLong(clk, 0)
    simV.sleep(1)
    simV.eval
    println(i.toString + " -> " + simV.getLong(rvalue).toString) // read from index 'i'
  }

  simV.end
  println("Finished TestVerilator2")
}

object TestVerilator3 extends App{
  val config = new VerilatorBackendConfig()
  config.rtlSourcesPaths += "./yolo/rtl/TestMemVerilatorW.v"
  config.toplevelName = "TestMemVerilatorW"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.vcdPath = "./test.vcd"
  config.waveFormat = WaveFormat.VCD
  
  val addr    = new Signal(Seq("io_addr"), new BitsDataType(10))
  val wvalue  = new Signal(Seq("io_wvalue"), new BitsDataType(128))
  val wenable = new Signal(Seq("io_wenable"), new BoolDataType)
  val rvalue  = new Signal(Seq("io_rvalue"), new BitsDataType(128))
  val clk     = new Signal(Seq("clk"), new BoolDataType)
  val reset   = new Signal(Seq("reset"), new BoolDataType)
  val mem     = new Signal(Seq("TestMemVerilatorW", "mem"), new BitsDataType(128))

  addr.id    = 0 
  wvalue.id  = 1
  wenable.id = 2 
  rvalue.id  = 3 
  clk.id     = 4 
  reset.id   = 5 
  mem.id     = 6 

  config.signals += addr   
  config.signals += wvalue 
  config.signals += wenable
  config.signals += rvalue 
  config.signals += clk    
  config.signals += reset  
  config.signals += mem    

  val verilatorBackend = new VerilatorBackend(config)
  val handle = verilatorBackend.instanciate("dummy", 42)
  val simV = new SimVerilator(verilatorBackend, handle)

  simV.setLong(clk, 0)
  simV.setLong(reset, 0)
  simV.setLong(addr, 0)
  simV.setBigInt(wvalue, BigInt(0))
  simV.setLong(wenable, 1)
  simV.sleep(1)
  simV.eval

  for(i <- 0 to 1023){
    simV.setLong(addr, i)
    simV.setBigInt(wvalue, BigInt(i))
    simV.sleep(1)
    simV.eval
    simV.setLong(clk, 1)
    simV.sleep(1)
    simV.eval
    simV.setLong(clk, 0)
    simV.sleep(1)
    simV.eval
  }

  simV.setLong(wenable, 0)
  simV.sleep(1)
  simV.eval

  for(i <- 0 to 1023){
    println(i.toString + " -> " + simV.getBigIntMem(mem, i).toString) // read from index 'i'
  }

  simV.end
  println("Finished TestVerilator3")
}

object TestVerilator4 extends App{
  val config = new VerilatorBackendConfig()
  config.rtlSourcesPaths += "./yolo/rtl/TestMemVerilator.v"
  config.toplevelName = "TestMemVerilator"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.vcdPath = "./test.vcd"
  config.waveFormat = WaveFormat.VCD
  
  val addr    = new Signal(Seq("io_addr"), new BitsDataType(10))
  val wvalue  = new Signal(Seq("io_wvalue"), new BitsDataType(11))
  val wenable = new Signal(Seq("io_wenable"), new BoolDataType)
  val rvalue  = new Signal(Seq("io_rvalue"), new BitsDataType(11))
  val clk     = new Signal(Seq("clk"), new BoolDataType)
  val reset   = new Signal(Seq("reset"), new BoolDataType)
  val mem     = new Signal(Seq("TestMemVerilator", "mem"), new BitsDataType(11))

  addr.id    = 0 
  wvalue.id  = 1
  wenable.id = 2 
  rvalue.id  = 3 
  clk.id     = 4 
  reset.id   = 5 
  mem.id     = 6 

  config.signals += addr   
  config.signals += wvalue 
  config.signals += wenable
  config.signals += rvalue 
  config.signals += clk    
  config.signals += reset  
  config.signals += mem    

  val verilatorBackend = new VerilatorBackend(config)
  val handle = verilatorBackend.instanciate("dummy", 42)
  val simV = new SimVerilator(verilatorBackend, handle)

  simV.setLong(clk, 0)
  simV.setLong(reset, 0)
  simV.setLong(addr, 0)
  simV.setLong(wvalue, 0)
  simV.setLong(wenable, 0)
  simV.sleep(1)
  simV.eval

  for(i <- 0 to 1023){
    simV.setBigIntMem(mem, BigInt(i), i)
  }

  simV.sleep(1)
  simV.eval

  for(i <- 0 to 1023){
    simV.setLong(addr, i)
    simV.sleep(1)
    simV.eval
    simV.setLong(clk, 1)
    simV.sleep(1)
    simV.eval
    simV.setLong(clk, 0)
    simV.sleep(1)
    simV.eval
    println(i.toString + " -> " + simV.getBigInt(rvalue).toString) // read from index 'i'
  }

  simV.end
  println("Finished TestVerilator4")
}

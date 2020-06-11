package spinal.sim

import java.io.File

import org.apache.commons.io.FileUtils

object SimError{
  def apply(message : String): Unit ={
    System.out.flush()
    Thread.sleep(20)
    System.err.println("\n\n" + message)
    throw new Exception()
  }
}

object WaveFormat{
  //Common waveformat options
  object VCD extends WaveFormat("vcd")
  object FST extends WaveFormat("fst")
  object DEFAULT extends WaveFormat
  object NONE extends WaveFormat
 
  //GHDL only
  object VCDGZ extends WaveFormat("vcdgz")
  object GHW extends WaveFormat("ghw")

  //Icarus Verilator only
  object FST_SPEED extends WaveFormat("fst-speed")
  object FST_SPACE extends WaveFormat("fst-space")
  object LXT extends WaveFormat("lxt")
  object LXT_SPEED extends WaveFormat("lxt-speed")
  object LXT_SPACE extends WaveFormat("lxt-space")
  object LXT2 extends WaveFormat("lxt2")
  object LXT2_SPEED extends WaveFormat("lxt2-speed")
  object LXT2_SPACE extends WaveFormat("lxt2-space")
}

class WaveFormat(val ext : String = "???")


trait Backend{
  val uniqueId       = Backend.allocateUniqueId()
  def isBufferedWrite : Boolean
}

object Backend{
  private var uniqueId = 0
  def allocateUniqueId(): Int = {
    this.synchronized {
      uniqueId = uniqueId + 1
      uniqueId
    }
  }

  val osName         = System.getProperty("os.name").toLowerCase
  val isWindows      = osName.contains("windows")
  val isMac          = osName.contains("mac") || osName.contains("darwin")
  val isLinux = !isWindows && !isMac

  val jdk = System.getProperty("java.home").replace("/jre","").replace("\\jre","")
}

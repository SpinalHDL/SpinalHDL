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
  case object VCD extends WaveFormat("vcd")
  case object FST extends WaveFormat("fst")
  case object DEFAULT extends WaveFormat
  case object NONE extends WaveFormat
 
  //GHDL only
  case object VCDGZ extends WaveFormat("vcdgz")
  case object GHW extends WaveFormat("ghw")

  //Icarus Verilator only
  case object FST_SPEED extends WaveFormat("fst-speed")
  case object FST_SPACE extends WaveFormat("fst-space")
  case object LXT extends WaveFormat("lxt")
  case object LXT_SPEED extends WaveFormat("lxt-speed")
  case object LXT_SPACE extends WaveFormat("lxt-space")
  case object LXT2 extends WaveFormat("lxt2")
  case object LXT2_SPEED extends WaveFormat("lxt2-speed")
  case object LXT2_SPACE extends WaveFormat("lxt2-space")

  // VCS only
  case object VPD extends WaveFormat("vpd")
  case object FSDB extends WaveFormat("fsdb")
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

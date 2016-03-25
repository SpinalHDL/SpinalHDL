package spinal.lib.bus.avalon.mm

import spinal.core._
import spinal.lib._
/**
 * Created by PIC32F_USER on 25/03/2016.
 */
trait AddressUnits
object words extends AddressUnits
object symbols extends AddressUnits

case class AvalonMMConfig( addressWidth : Int,
                           dataWidth : Int,
                           burstCountWidth : Int,
                           useByteEnable : Boolean,
                           useDebugAccess : Boolean,
                           useRead : Boolean,
                           useWrite : Boolean,
                           useResponse : Boolean,
                           useLock : Boolean,
                           useWaitRequestn : Boolean,
                           useReadDataValid : Boolean,
                           useBurstCount : Boolean,

                           addressUnits : AddressUnits = symbols,
                           burstCountUnits : AddressUnits = words,
                           burstOnBurstBoundariesOnly : Boolean = false,
                           constantBurstBehavior : Boolean = false,
                           holdTime : Int = 0,
                           linewrapBursts : Boolean = false,
                           maximumPendingReadTransactions : Int = 1,
                           maximumPendingWriteTransactions : Int = 0, // unlimited
                           readLatency : Int = 0,
                           readWaitTime : Int = 1,
                           setupTime : Int = 0,
                           writeWaitTime : Int = 0
                           ) {
  val dataByteCount = dataWidth/8
  val getReadOnlyConfig = copy(
    useWrite = false,
    useByteEnable = false
  )
}

object AvalonMMConfig{
  def pipelined(addressWidth : Int,
                dataWidth : Int) = {
    AvalonMMConfig(
      addressWidth=addressWidth,
      dataWidth=dataWidth,
      burstCountWidth = -1,
      useByteEnable = true,
      useDebugAccess = false,
      useRead = true,
      useWrite = true,
      useResponse = false,
      useLock = false,
      useWaitRequestn = true,
      useReadDataValid = true,
      useBurstCount = false
    )
  }
}


object AvalonResponse extends SpinalEnum(sequancial){
  val OKAY,RESERVED,SLAVEERROR,DECODEERROR = newElement()
}

case class AvalonMMBus(c : AvalonMMConfig) extends Bundle with IMasterSlave{
  import c._
  val read = if(useRead) Bool else null
  val write = if(useWrite) Bool else null
  val waitRequestn = if(useWaitRequestn) Bool else null
  val lock = if(useLock) Bool else null
  val debugAccess = if(useDebugAccess) Bool else null
  val address = UInt(addressWidth bit)
  val burstCount = if(useBurstCount) UInt(burstCountWidth bit) else null
  val byteEnable = if(useByteEnable) Bits(dataByteCount bit) else null
  val writeData = if(useWrite) Bits(dataWidth bit) else null
  val response = if(useResponse) AvalonResponse() else null

  val readDataValid = if(useReadDataValid) Bool else null
  val readData = if(useRead) Bits(dataWidth bit) else null
  override def asMaster(): AvalonMMBus.this.type = {

    this
  }
}

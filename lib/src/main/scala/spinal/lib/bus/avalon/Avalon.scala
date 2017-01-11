package spinal.lib.bus.avalon

import spinal.core._
import spinal.lib._
/**
 * Created by PIC32F_USER on 25/03/2016.
 */
trait ScalaEnumeration{
  def getName = this.getClass.getName.split("[.]").last.split("\\$").last
}

trait AddressUnits extends ScalaEnumeration
object WORDS extends AddressUnits
object SYMBOLS extends AddressUnits

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
                           //useEndOfPacket : Boolean,

                           addressUnits : AddressUnits = SYMBOLS,
                           burstCountUnits : AddressUnits = WORDS,
                           burstOnBurstBoundariesOnly : Boolean = false,
                           constantBurstBehavior : Boolean = false,
                           holdTime : Int = 0,
                           linewrapBursts : Boolean = false,
                           maximumPendingReadTransactions : Int = 1,
                           maximumPendingWriteTransactions : Int = 0, // unlimited
                           readLatency : Int = 0,
                           readWaitTime : Int = 0,
                           setupTime : Int = 0,
                           writeWaitTime : Int = 0
                           ) {
  val dataByteCount = dataWidth/8
  def getReadOnlyConfig = copy(
    useWrite = false,
    useByteEnable = false
  )
  def getWriteOnlyConfig = copy(
    useRead = false,
    useReadDataValid = false
  )
}

object AvalonMMConfig{
  def fixed(addressWidth : Int,
            dataWidth : Int,
            readLatency : Int) = AvalonMMConfig(
    addressWidth=addressWidth,
    dataWidth=dataWidth,
    readLatency = readLatency,
    burstCountWidth = -1,
    useByteEnable = false,
    useDebugAccess = false,
    useRead = true,
    useWrite = true,
    useResponse = false,
    useLock = false,
    useWaitRequestn = true,
    useReadDataValid = false,
    useBurstCount = false,

    maximumPendingReadTransactions = 0
  )



  def pipelined(addressWidth : Int,
                dataWidth : Int) = AvalonMMConfig(
      addressWidth=addressWidth,
      dataWidth=dataWidth,
      burstCountWidth = -1,
      useByteEnable = false,
      useDebugAccess = false,
      useRead = true,
      useWrite = true,
      useResponse = false,
      useLock = false,
      useWaitRequestn = true,
      useReadDataValid = true,
      useBurstCount = false
    )


  def bursted(addressWidth : Int,
              dataWidth : Int,
              burstCountWidth : Int) = AvalonMMConfig(
      addressWidth=addressWidth,
      dataWidth=dataWidth,
      burstCountWidth = burstCountWidth,
      useByteEnable = false,
      useDebugAccess = false,
      useRead = true,
      useWrite = true,
      useResponse = false,
      useLock = false,
      useWaitRequestn = true,
      useReadDataValid = true,
      useBurstCount = true
    )
}

object AvalonMM{
  object Response extends SpinalEnum(binarySequential){
    val OKAY,RESERVED,SLAVEERROR,DECODEERROR = newElement()
  }
}



case class AvalonMM(config : AvalonMMConfig) extends Bundle with IMasterSlave{
  import config._
  val read          = if(useRead)          Bool else null
  val write         = if(useWrite)         Bool else null
  val waitRequestn  = if(useWaitRequestn)  Bool else null
  val lock          = if(useLock)          Bool else null
  val debugAccess   = if(useDebugAccess)   Bool else null
  val address       =                      UInt(addressWidth bit)
  val burstCount    = if(useBurstCount)    UInt(burstCountWidth bit) else null
  val byteEnable    = if(useByteEnable)    Bits(dataByteCount bit) else null
  val writeData     = if(useWrite)         Bits(dataWidth bit) else null
  val response      = if(useResponse)      AvalonMM.Response() else null

  val readDataValid = if(useReadDataValid) Bool else null
  val readData      = if(useRead)          Bits(dataWidth bit) else null

  def isValid = (if(useRead) read else False) || (if(useWrite) write else False)
  def isReady = (if(useWaitRequestn) waitRequestn else True)
  def fire = isValid && isReady

  def setOKEY : Unit = response := AvalonMM.Response.OKAY

  override def asMaster(): Unit = {
    outWithNull(read,write,lock,debugAccess,address,burstCount,byteEnable,writeData)
    inWithNull(waitRequestn,response,readDataValid,readData)
  }
}





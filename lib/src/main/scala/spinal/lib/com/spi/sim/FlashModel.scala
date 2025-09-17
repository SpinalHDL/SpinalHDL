package spinal.lib.com.spi.sim


import spinal.core._
import spinal.core.sim._
import spinal.lib.com.spi.ddr.SpiXdrMaster
import java.nio.file.{Files, Paths}


case class FlashModel(spi : SpiXdrMaster, cd : ClockDomain) {
  var sclkLast = false
  val spiDataWithIndex = spi.data.zipWithIndex
  var spiDataRead = Array.fill(spi.p.dataWidth)(0)

  var edgeHandler : (Boolean,  => Int) => Unit = idle
  var counter, buffer, buffer2, address = 0
  var miso = 0
  var cmd = 0

  val content = Array.fill[Byte](16 * 1024 * 1024)(0xAA.toByte)
  def loadBinary(path : String, offset : Int): Unit ={
    val bin = Files.readAllBytes(Paths.get(path))
    for((v,i) <- bin.zipWithIndex) content(i + offset) = v
  }

  def goto(that : (Boolean, => Int) => Unit): Unit = {
    edgeHandler = that
  }

  var dirty = false
  cd.onActiveEdges{
    if(spi.ss.toInt == 1){
      sclkLast = false
      counter = 8
      sclkLast = (spi.sclk.write.toInt & 1).toBoolean
      goto(command)
    } else {
      val sclkWrite = spi.sclk.write.toInt
      if(dirty) for(pin <- 0 until spi.p.dataWidth) {
        spi.data(pin).read #= spiDataRead(pin)
        spiDataRead(pin) = 0
      }
      dirty = false
      for(phase <- 0 until spi.p.ioRate){
        val sclk = ((sclkWrite >> phase) & 1) != 0
        if(sclkLast != sclk) {
          edgeHandler(sclk, {
            var mosi = 0
            for((v,pin) <- spiDataWithIndex) mosi |= ((v.write.toInt >> phase) & 1) << pin
            mosi
          })
          dirty = true
        }
        if(dirty) for(pin <- 0 until spi.p.dataWidth) spiDataRead(pin) |= ((miso >> pin) & 1) << phase
        sclkLast = sclk
      }
    }
  }

  def info(m : String) = {
    //    println(m)
  }

  def idle(rising : Boolean, mosi : => Int): Unit = {}
  def command(rising : Boolean, mosi : => Int): Unit = {
    if (rising){
      if(counter == 8){
        buffer = 0
        buffer2 = 0
      }
      counter -= 1
      buffer |= (mosi & 1) << counter
      buffer2 |= (mosi & 3) << (counter-4)*2
      counter match{
        case 4 =>
          buffer2 match {
            //            case 0x3B => counter = 24; address = 0; cmd = buffer2; println(s"CMD $cmd"); goto(readAddressDual)
            case _ =>
          }
        case 0 =>
          info(s"CMD ${buffer.toHexString}")
          buffer match {
            case 0x0B => counter = 24; address = 0; goto(readAddress)
            case 0x3B => counter = 24; address = 0; goto(readAddressDual)
            case 0x9F => counter = 24; goto(readId)
            case 0xD8 => counter = 24; address = 0; goto(eraseAddress)
            case 0x02 => counter = 24; address = 0; goto(writeAddress)
            case 0x05 => counter = 8; goto(readStatus)
            case _ =>
          }
        case _ =>
      }
    }
  }
  def eraseAddress(rising : Boolean, mosi : => Int): Unit = {
    if (rising){
      counter -= 1
      address |= (mosi & 1) << counter
      if (counter == 0) {
        info(s"ERASE ${address.toHexString}")
        address &= ~0xFFFF
        for(i <- address to address + 0xFFFF) content(i) = 0xFF.toByte
      }
    }
  }
  def writeAddress(rising : Boolean, mosi : => Int): Unit = {
    if (rising){
      counter -= 1
      address |= (mosi & 1) << counter
      if (counter == 0) {
        counter = 8; buffer = 0;
        goto(writePayload)
      }
    }
  }

  def writePayload(rising : Boolean, mosi : => Int): Unit = {
    if (rising){
      counter -= 1
      buffer |= (mosi & 1) << counter
      if (counter == 0) {
        info(s"WRITE [${address.toHexString}]=${buffer.toHexString}")
        content(address) = (content(address) & buffer).toByte
        address = (address & ~0xFF) | ((address + 1) & 0xFF)
        counter = 8; buffer = 0;
      }
    }
  }
  def readStatus(rising : Boolean, mosi : => Int): Unit = {
    if (rising){
      counter -= 1
      miso = ((0 >> counter) & 1)
    }
  }

  def readId(rising : Boolean, mosi : => Int): Unit = {
    if (rising){
      counter -= 1
      miso = ((0x123456 >> counter) & 1) << 1
    }
  }
  def readAddress(rising : Boolean, mosi : => Int): Unit = {
    if (rising){
      counter -= 1
      address |= (mosi & 1) << counter
      if (counter == 0) {
        counter = 8; goto(readDummy)
      }
    }
  }
  def readDummy(rising : Boolean, mosi : => Int): Unit = {
    if (rising){
      counter -= 1
      if (counter == 0) {
        counter = 8; goto(readPayload)
      }
    }
  }
  def readPayload(rising : Boolean, mosi : => Int): Unit = {
    if (!rising){
      counter -= 1
      miso = ((content(address) >> counter) & 1) << 1
      if (counter == 0) {
        address += 1
        counter = 8
      }
    }
  }

  def readAddressDual(rising : Boolean, mosi : => Int): Unit = {
    if (rising){
      counter -= 1
      address |= (mosi & 1) << counter
      if (counter == 0) {
        counter = 8; goto(readDummyDual)
      }
    }
  }
  def readDummyDual(rising : Boolean, mosi : => Int): Unit = {
    if (rising){
      counter -= 1
      if (counter == 0) {
        counter = 8; goto(readPayloadDual)
      }
    }
  }
  def readPayloadDual(rising : Boolean, mosi : => Int): Unit = {
    if (!rising){
      counter -= 2
      miso = ((content(address) >> counter) & 3)
      if (counter == 0) {
        address += 1
        counter = 8
      }
    }
  }
}

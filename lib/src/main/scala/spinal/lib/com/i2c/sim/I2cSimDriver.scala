package spinal.lib.com.i2c.sim

import spinal.core.sim._
import spinal.core._
import spinal.lib.io.ReadableOpenDrain

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class OpenDrainSoftConnection(interconnect: OpenDrainInterconnect) {
  var value = true

  def write(value: Boolean) = {
    if (this.value != value) {
      this.value = value
      interconnect.evaluate()
    }
  }

  def read() = interconnect.value
}

class OpenDrainInterconnect {
  val softConnections = ArrayBuffer[OpenDrainSoftConnection]()
  val hardWriters = ArrayBuffer[Bool]()
  val hardReaders = ArrayBuffer[Bool]()
  var value = true



  def newSoftConnection() = {
    val endpoint = new OpenDrainSoftConnection(this)
    softConnections.append(endpoint)
    endpoint
  }

  def pinWatcher(driver: Bool) = {
    sim.forkSensitive {
      if (driver.toBoolean != value) {
        evaluate();
      }
    }
  }

  def addHard(pin : ReadableOpenDrain[Bool]): Unit ={
    addHardDriver(pin.write)
    addHardReader(pin.read)
  }

  def addHardDriver(driver: Bool) = {
    hardWriters += driver
    pinWatcher(driver)
  }

  def addHardReader(reader: Bool) = {
    hardReaders += reader
    reader #= value
  }

  def evaluate() = {
    var newValue = true
    for (soft <- softConnections) {
      newValue &= soft.value

      for (hard <- hardWriters)
        newValue &= (hard.toBoolean)

      if (newValue != value) {
        value = newValue
        for (reader <- hardReaders)
          reader #= newValue
        //            if applyChange:
        //                applyChange(newValue)
      }
    }
  }
}


abstract class I2cSoftMaster(scl : OpenDrainSoftConnection,sda : OpenDrainSoftConnection, baudPeriod : Int){

    
//  def waitScl() = waitUntil(scl.read())
//  def wait(bauds) = sleep(period*bauds)

  class Event
  val START, STOP, ACK, NACK = new Event
  case class DATA(value : Int) extends Event

  var sdaOld = sda.read()
  var sclOld = scl.read()
  var counter = 0
  var buffer = 0
  val driveFeed = mutable.Queue[Boolean]()
  forkSensitive {
    val sdaNew = sda.read()
    val sclNew = scl.read()

    if(sclNew && sdaOld != sdaNew) {
      if(!sdaNew) {
        event(START)
        counter = 0
        buffer = 0
      } else {
        sda.write(true)
        event(STOP)
      }
    }
    if(sclOld != sclNew) {
      if(!sclNew) {
        //drive
        delayed(baudPeriod/4)( sda.write(if(driveFeed.nonEmpty) driveFeed.dequeue() else true))
      } else {
        //read
        counter = counter + 1
//        println(simTime() + " " + sda.read())
        buffer <<= 1
        buffer |= (if(sda.read()) 1 else 0)
        counter match {
          case 8 => event(new DATA(buffer))
          case 9 => event(if(sda.read()) NACK else ACK); buffer = 0; counter = 0
          case _ =>
        }
      }
    }
    sdaOld = sdaNew
    sclOld = sclNew
  }

  def event(e : Event): Unit

//  def waitStart() = {
//    var sdaOld = sda.read()
//    waitUntil{
//      val sdaNew = sda.read()
//      val hit = !sdaNew && sdaOld && scl.read()
//      sdaOld = sdaNew
//      !hit
//    }
//  }
//
//  def waitStop() = {
//    var sdaOld = sda.read()
//    waitUntil{
//      val sdaNew = sda.read()
//      val hit = sdaNew && !sdaOld && scl.read()
//      sdaOld = sdaNew
//      !hit
//    }
//  }
//
//  def readByte() = {
//    for(i <- 7 downto 0) {
//
//    }
//  }
}
//
//
//    def sendStart() = {
//        sda.write(false)
//        sleep(period)
//        scl.write(false)
//
//
//    def sendRestart() = {
//        sleep(period/2)
//        sda.write(true)
//        sleep(period/2)
//        scl.write(true)
//        waitScl()
//        sleep(period)
//        sendStart()
//
//
//
//    def sendBit(self, value,ret = None):
//        sleep(period / 2)
//        sda.write(value)
//        sleep(period / 2)
//        scl.write(true)
//        waitScl()
//        sleep(period)
//        if ret:
//            ret[0] = sda.read()
//        scl.write(false)
//
//
//    def sendBitCheck(self, value, expected):
//        buffer = [0]
//        sendBit(value, buffer)
//        assert buffer[0] == expected
//
//
//
//    def sendByte(self, value ,ret = None):
//        if ret != None :
//            ret[0] = 0
//        buffer = [false]
//        for i in xrange(8):
//            sendBit(testBit(value,7-i),buffer)
//            if ret != None:
//                ret[0] |= buffer[0] << (7-i)
//
//
//    def sendByteCheck(self, value, expected):
//        buffer = [0]
//        sendByte(value, buffer)
//        assert buffer[0] == expected
//
//
//    def sendStop() = {
//        sleep(period/2)
//        sda.write(false)
//        sleep(period/2)
//        scl.write(true)
//        waitScl()
//        sleep(period)
//        sda.write(true)
//        sleep(period)
//
//
//    def sendDrop() = {
//        sleep(period/2)
//        sda.write(true)
//        sleep(period/2)
//        scl.write(true)

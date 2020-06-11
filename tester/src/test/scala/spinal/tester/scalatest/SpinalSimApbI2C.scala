package spinal.tester.scalatest


import org.scalatest.FunSuite
import spinal.core._
import spinal.core.internals.GraphUtils
import spinal.core.sim._
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.com.i2c._
import spinal.sim._
import spinal.lib._

import scala.collection.mutable.ListBuffer


trait I2CEvent

case class Start_i2c() extends I2CEvent {
  override def toString: String = "Start"
}

case class Restart_i2c() extends I2CEvent {
  override def toString: String = "reStart"
}

case class Stop_i2c() extends I2CEvent {
  override def toString: String = "Stop"
}

case class DataRead_i2c(value: Byte, ack: Boolean)    extends I2CEvent {
  def ==(that: DataWrite_i2c) = (this.value == that.value) && (this.ack == that.ack)
  override def toString: String = f"Data Read (0x${value}%02X / ${ack})"
}

case class DataWrite_i2c(value: Byte, ack: Boolean)   extends I2CEvent {
  def ==(that: DataWrite_i2c) = (this.value == that.value) && (this.ack == that.ack)
  override def toString: String = f"Data Write (0x${value}%02X / ${ack})"
}




case class Apb3Sim(dut: Apb3, clockDomain: ClockDomain){

  def initIO(): Unit = {
    dut.PSEL #= 0
    dut.PADDR.randomize()
    dut.PENABLE.randomize()
    dut.PWRITE.randomize()
    dut.PWDATA.randomize()
  }

  def write(address: BigInt, data: BigInt, sel: BigInt = 1): Unit ={
    dut.PADDR   #= address
    dut.PSEL    #= sel
    dut.PENABLE #= false
    dut.PWRITE  #= true
    dut.PWDATA  #= data
    clockDomain.waitActiveEdge()
    dut.PENABLE #= true

    clockDomain.waitActiveEdgeWhere(dut.PREADY.toBoolean)
    initIO()
  }

  def read(address: BigInt, sel: BigInt = 1): BigInt = {
    dut.PADDR   #= address
    dut.PSEL    #= sel
    dut.PENABLE #= false
    dut.PWRITE  #= false
    dut.PWDATA.randomize()
    clockDomain.waitActiveEdge()

    dut.PENABLE #= true
    clockDomain.waitActiveEdgeWhere(dut.PREADY.toBoolean)
    initIO()

    return dut.PRDATA.toBigInt
  }
}

class OpenDrainSoftConnection(interconnect: OpenDrainInterconnect){

  var _value = true

  def write(value: Boolean): Unit ={
    if(_value != value){
      _value = value
      interconnect.evaluate()
    }
  }

  def read(): Boolean = interconnect.value
}


class OpenDrainInterconnect(clockDomain: ClockDomain){

  val hardWriters     = ListBuffer[Bool]()
  val hardReaders     = ListBuffer[Bool]()
  val softConnections = ListBuffer[OpenDrainSoftConnection]()
  var value           = true

  def pinWatcher(driver: Bool) = fork{
    var state = driver.toBoolean
    while(true){
      waitUntil(state != driver.toBoolean)
      state = driver.toBoolean
      evaluate()
    }
  }

  def newSoftConnection(): OpenDrainSoftConnection ={
    val endPoint = new OpenDrainSoftConnection(this)
    softConnections += endPoint
    return endPoint
  }

  def addHardDriver(driver: Bool) = {
    hardWriters += driver
    pinWatcher(driver )
  }

  def addHardReader(reader: Bool) = {
    hardReaders += reader
    reader #= true
  }

  def evaluate(): Unit   = {
    var newValue = true

    for(soft <- softConnections){
      newValue &= soft._value
    }

    for (hard <- hardWriters){
      newValue &= hard.toBoolean
    }

    if (newValue != value){
      value = newValue
      for (reader <- hardReaders){
        reader #= value
      }
    }

    clockDomain.waitActiveEdge()
  }
}



class SpinalSimApbI2C extends SpinalSimFunSuite {

  case class I2CSlaveModel(sda: OpenDrainSoftConnection, scl: OpenDrainSoftConnection){

    val cmdSlave  = new ListBuffer[I2CEvent]()

    def run() = fork{

      var busy = true
      var index = 0

      waitUntil(cmdSlave.length != 0)

      while(busy){

        val cmd = cmdSlave(index)

        if(cmd.isInstanceOf[Start_i2c] || cmd.isInstanceOf[Restart_i2c]){
//          println("I2cModel - Start ")
          var detector = false
          while(!detector){
            var lastSDA = sda.read()
            waitUntil(lastSDA != sda.read())
            if(lastSDA && !sda.read() && scl.read()){
              detector = true
            }
          }
          waitUntil(!scl.read())
        }else if(cmd.isInstanceOf[Stop_i2c]){
//          println("I2cModel - Stop ")
          var detector = false
          while(!detector){
            var lastSDA = sda.read()
            waitUntil(lastSDA != sda.read())
            if(!lastSDA && sda.read() && scl.read()){
              detector     = true
            }
          }
        }else if(cmd.isInstanceOf[DataRead_i2c]){

          val cmdData = cmd.asInstanceOf[DataRead_i2c]

          var cntBit = 0
          while(cntBit != 8){
            sda.write( ((cmdData.value >> (7 - cntBit)) & 0x01) == 0x01 )
            val sclLast = scl.read()
            waitUntil(sclLast != scl.read())
            cntBit += 1
            waitUntil(!scl.read())
          }

          sda.write(true)

          // Read Ack
          waitUntil(scl.read())
//          println(s"I2cModel - ACK ${sda.read()}")
          waitUntil(!scl.read())


        }else if(cmd.isInstanceOf[DataWrite_i2c]){

          val cmdData = cmd.asInstanceOf[DataWrite_i2c]

          var cntBit = 0
          var dataRead = 0
          while(cntBit != 8){
            val sclLast = scl.read()
            waitUntil(sclLast != scl.read())
            dataRead |= ((if(sda.read()) 0x01 else 0x00) << (7 - cntBit))
            cntBit += 1

            waitUntil(!scl.read())
          }

//          println(f"I2cModel - Data write ${dataRead}%02X")
//          assert(cmdData.value == dataRead.toByte, "Byte Write error")

          // Write  ACK

          sda.write(cmdData.ack)

          waitUntil(scl.read())

          waitUntil(!scl.read())

          sda.write(true)

          waitUntil(!scl.read())
        }

        index += 1


        if(index >= cmdSlave.length){
          busy = false
        }
      }
    }

  }

  case class I2CHelper(apb: Apb3Sim){

    case class Register(){
      def tx_data          = 0x00
      def tx_ack           = 0x04
      def rx_data          = 0x08
      def rx_ack           = 0x0C
      def interrupt        = 0x20
      def sampling_clock   = 0x28
      def timeout          = 0x2C
      def tsu_data         = 0x30
      def status_master    = 0x40
      def t_low            = 0x50
      def t_high           = 0x54
      def t_buf            = 0x58
      def status_filtering = 0x80
      def hit_context      = 0x84
      def filtering_config = 0x88
    }

    def reg = Register()

    def I2C_MASTER_START = 0x010
    def I2C_MASTER_STOP  = 0x020

    def I2C_TX_VALID     = 0x100
    def I2C_TX_ENABLE    = 0x200
    def I2C_TX_REPEAT    = 0x400
    def I2C_TX_CONFLIT   = 0x800

    def I2C_RX_VALID     = 0x100
    def I2C_RX_LISTEN    = 0x200
    def I2C_IS_BUSY      = 0x001

    var bufferCmd = new ListBuffer[I2CEvent]()


    def polling_rx_ack(): (Byte, Byte)  ={
      var status = BigInt(0)

      while((status & I2C_RX_VALID) != I2C_RX_VALID){
        status = apb.read(reg.rx_ack)
      }

      // read dummy data to release txvalid
      val dataRead = apb.read(reg.rx_data)

      return (dataRead.toByte, (status & 0x01) toByte)
    }

    def stop(): Unit = {
      apb.write(reg.status_master, I2C_MASTER_STOP)

      bufferCmd += Stop_i2c()

      // wait until the end of the emission of the stop bit
      while((apb.read(reg.status_master) & I2C_MASTER_STOP) != 0){}
    }

    def write(data: Int): Unit  ={

      apb.write(reg.tx_data, data | I2C_TX_VALID | I2C_TX_ENABLE )

      val(dataRead, ack) = polling_rx_ack()

      bufferCmd += DataWrite_i2c(dataRead, (ack == 0x01))

      ()
    }

    def read(ack: Boolean): Byte = {

      apb.write(reg.rx_data, I2C_RX_LISTEN)

      apb.write(reg.tx_data, 0x00 | I2C_TX_VALID)

      apb.write(reg.tx_ack, (if(ack) 0x01 else 0x00) | I2C_TX_VALID | I2C_TX_ENABLE)

      val (dataRead, ackRead) = polling_rx_ack()

      bufferCmd += DataRead_i2c(dataRead, (ackRead == 0x01))

      return apb.read(reg.rx_data).toByte
    }


    def  start(isRestart: Boolean = false): Unit = {
      apb.write(reg.status_master, I2C_MASTER_START)

      // wait until it started
      while((apb.read(reg.status_master) & I2C_MASTER_START) != 0){}

      apb.write(reg.rx_ack, I2C_RX_LISTEN)
      bufferCmd += (if(isRestart) Restart_i2c() else Start_i2c())
      ()
    }

    def restart(): Unit = start(true)

    def checkCmd(slave: ListBuffer[I2CEvent]) = {
      assert(bufferCmd.zip(slave).map(i2c => i2c._1 == i2c._2).reduce( _ && _ ), s"Mismatch between slave and master \nMaster : \n ${bufferCmd.mkString("\n ")} \nSlave : \n ${slave.mkString("\n ")}")
    }
  }
  
  test("1 Master <-> 1 Slave") {


    def configI2C = I2cSlaveMemoryMappedGenerics(
      ctrlGenerics = I2cSlaveGenerics(),
      addressFilterCount = 0,
      masterGenerics = I2cMasterMemoryMappedGenerics(timerWidth = 32)
    )

    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(50 MHz))).compile(new Apb3I2cCtrl(configI2C)).doSim { dut =>

      val apb = Apb3Sim(dut.io.apb, dut.clockDomain)
      val i2c = I2CHelper(apb)
      val frequency_i2c = 400 kHz

      val sdaInterconnect = new OpenDrainInterconnect(dut.clockDomain)
      sdaInterconnect.addHardDriver(dut.io.i2c.sda.write)
      sdaInterconnect.addHardReader(dut.io.i2c.sda.read)

      val sclInterconnect = new OpenDrainInterconnect(dut.clockDomain)
      sclInterconnect.addHardDriver(dut.io.i2c.scl.write)
      sclInterconnect.addHardReader(dut.io.i2c.scl.read)

      val mainClkPeriod = (1e12 / dut.clockDomain.frequency.getValue.toDouble).toLong

      dut.clockDomain.forkStimulus(mainClkPeriod)

      apb.initIO()

      dut.clockDomain.waitSampling()


      /**
        * I2C Configuration
        */

      // Master configuration
      apb.write(i2c.reg.t_buf, ((frequency_i2c.toTime / 2) * dut.clockDomain.frequency.getValue).toBigInt)
      apb.write(i2c.reg.t_high, ((frequency_i2c.toTime / 2) * dut.clockDomain.frequency.getValue).toBigInt)
      apb.write(i2c.reg.t_low, ((frequency_i2c.toTime / 2) * dut.clockDomain.frequency.getValue).toBigInt)

      // I2C Configuration
      apb.write(i2c.reg.sampling_clock, (dut.clockDomain.frequency.getValue / (10 MHz)).toBigInt) // sampling frequency 10 MHz
      apb.write(i2c.reg.timeout, (dut.clockDomain.frequency.getValue * 2).toBigDecimal.toBigInt) // Timeout after 2 secondes
      apb.write(i2c.reg.tsu_data, 25)


      /**
        * Create slave model
        */
      val slave = I2CSlaveModel(sdaInterconnect.newSoftConnection(), sclInterconnect.newSoftConnection())

      /**
        * Check Write operation
        */
      slave.cmdSlave.clear()
      slave.cmdSlave += Start_i2c()
      slave.cmdSlave += DataWrite_i2c(0x06.toByte, false)
      slave.cmdSlave += DataWrite_i2c(0xAA.toByte, true)
      slave.cmdSlave += Stop_i2c()
      slave.run()

      i2c.bufferCmd.clear()
      i2c.start()
      i2c.write(0x06)
      i2c.write(0xAA)
      i2c.stop()

      i2c.checkCmd(slave.cmdSlave)


      dut.clockDomain.waitActiveEdge(10)


      /**
        * Check Write and read operations
        */
      slave.cmdSlave.clear()
      slave.cmdSlave += Start_i2c()
      slave.cmdSlave += DataWrite_i2c(0x07.toByte, false)
      slave.cmdSlave += DataRead_i2c(0xAA.toByte, false)
      slave.cmdSlave += DataRead_i2c(0x55.toByte, true)
      slave.cmdSlave += Stop_i2c()
      slave.run()


      i2c.bufferCmd.clear()
      i2c.start()
      i2c.write(0x07)
      i2c.read(false)
      i2c.read(true)
      i2c.stop()

      i2c.checkCmd(slave.cmdSlave)


      dut.clockDomain.waitActiveEdge(10)

      /**
        * Check Write, Restart and read operations
        */
      slave.cmdSlave.clear()
      slave.cmdSlave += Start_i2c()
      slave.cmdSlave += DataWrite_i2c(0x07.toByte, false)
      slave.cmdSlave += DataWrite_i2c(0x00.toByte, false)
      slave.cmdSlave += Restart_i2c()
      slave.cmdSlave += DataWrite_i2c(0x07.toByte, false)
      slave.cmdSlave += DataRead_i2c(0x3D.toByte, false)
      slave.cmdSlave += DataRead_i2c(0x5A.toByte, true)
      slave.cmdSlave += Stop_i2c()
      slave.run()


      i2c.bufferCmd.clear()
      i2c.start()
      i2c.write(0x07)
      i2c.write(0x00)
      i2c.restart()
      i2c.write(0x07)
      i2c.read(false)
      i2c.read(true)
      i2c.stop()

      i2c.checkCmd(slave.cmdSlave)


      dut.clockDomain.waitActiveEdge(200)
    }
  }
}


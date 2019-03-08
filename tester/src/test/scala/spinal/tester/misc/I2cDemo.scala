package spinal.tester.misc
import spinal.core.sim._
import spinal.core._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import spinal.lib.com.i2c.{Apb3I2cCtrl, I2cMasterMemoryMappedGenerics, I2cSlaveGenerics, I2cSlaveMemoryMappedGenerics}

object I2cDemo extends App{
  SimConfig.withWave.compile(new Apb3I2cCtrl(
    I2cSlaveMemoryMappedGenerics(
      ctrlGenerics = I2cSlaveGenerics(
        samplingWindowSize = 3,
        samplingClockDividerWidth = 10 bits,
        timeoutWidth = 20 bits
      ),
      addressFilterCount = 4,
      masterGenerics = I2cMasterMemoryMappedGenerics(
        timerWidth = 16
      )
    ))).doSim{dut =>
    val apb = Apb3Driver(dut.io.apb, dut.clockDomain)

    dut.clockDomain.forkStimulus(20) //50 Mhz

    dut.clockDomain.onActiveEdges{
      delayed(100){ //100 ns Delay
        dut.io.i2c.scl.read #= dut.io.i2c.scl.write.toBoolean
        dut.io.i2c.sda.read #= dut.io.i2c.sda.write.toBoolean
      }
    }

    val samplingClockDivider = 0x28
    val timeout = 0x2C
    val tsuDat = 0x30
    val tLow = 0x50
    val tHigh = 0x54
    val tBuf = 0x58

    val masterStatus = 0x40
    val masterIsBusy = 1<<0
    val masterStart = 1<<4
    val masterStop = 1<<5


    val txData = 0x00
    val txAck = 0x04
    val txValid = 1<<8
    val txEnable = 1<<9
    val txRepeat = 1<<10
    val txDisableOnDataConflict = 1<<11


    dut.clockDomain.waitSampling(10000)

    //Configure the I2C peripheral
    apb.write(samplingClockDivider, 5)
    apb.write(timeout, 50000)
    apb.write(tsuDat, 5)
    apb.write(tLow, 250)
    apb.write(tHigh, 250)
    apb.write(tBuf, 1000)



    apb.write(txData, txValid | txEnable | 0x0C) //Preset the address with 0x0C
    apb.write(txAck, txValid | txEnable) //NACK
    apb.write(masterStatus, masterStart) //Start bit

    while((apb.read(txAck) & txValid) != 0){dut.clockDomain.waitSampling(10)} //wait until NACK is transmited
    for(i <- 0 to 1){
      apb.write(txData, txValid | txEnable | 0x10 + i) //Send payload
      apb.write(txAck, txValid | txEnable) //NACK
      while((apb.read(txAck) & txValid) != 0){dut.clockDomain.waitSampling(10)} //wait until NACK is transmited
    }
    apb.write(masterStatus, masterStop)
    while((apb.read(masterStatus) & masterIsBusy) != 0){dut.clockDomain.waitSampling(10)}

    dut.clockDomain.waitSampling(10000)

  }
}

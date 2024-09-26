package spinal.lib.bus.misc.args

import spinal.core
import spinal.core._
import spinal.lib._
import spinal.lib.com.i2c.{I2cMasterMemoryMappedGenerics, I2cSlaveGenerics, I2cSlaveMemoryMappedGenerics, TilelinkI2cCtrlFiber}
import spinal.lib.misc.plic.InterruptCtrlFiber

import scala.collection.mutable.ArrayBuffer

object I2cSpec{
  def addOption(parser: scopt.OptionParser[Unit], i2c: ArrayBuffer[I2cSpec]): Unit = {
    import parser._
    opt[Map[String, String]]("i2c").unbounded().action { (v, c) =>
      i2c += I2cSpec(
        address = v("address").toBigInt,
        name = v("name"),
        interruptId = Integer.decode(v("interruptId")).toInt,
        config = I2cSlaveMemoryMappedGenerics(
          ctrlGenerics = I2cSlaveGenerics(
            samplingWindowSize = 3,
            samplingClockDividerWidth = 10 bits,
            timeoutWidth = 20 bits
          ),
          addressFilterCount = 2,
          masterGenerics = I2cMasterMemoryMappedGenerics(
            timerWidth = 12
          )
        )
      )
    } text (s"Add a new I2C with the given name, address (relative to the apbBridge) and interrupt id,  Ex : --i2c name=portName,address=0x123000,interruptId=2")
  }
}

case class I2cSpec( name : String,
                    address : BigInt,
                    interruptId : Int,
                    config : I2cSlaveMemoryMappedGenerics)

class PeriphSpecs{
  val i2c = ArrayBuffer[I2cSpec]()

  def addOptions(parser: scopt.OptionParser[Unit]): Unit = {
    I2cSpec.addOption(parser, i2c)
  }
}

import bus.tilelink
class PeriphTilelinkFiber(p : PeriphSpecs,
                          ctrl : tilelink.fabric.Node,
                          intc : InterruptCtrlFiber) extends Area{
  val i2cCtrls = for(spec <- p.i2c) yield {
    val area = TilelinkI2cCtrlFiber(spec.config)
    area.setName(spec.name)
    area.ctrl at spec.address of ctrl
    intc.mapUpInterrupt(spec.interruptId, area.interrupt)
    core.fiber.hardFork(area.logic.i2c.setName(spec.name))
    area
  }
}
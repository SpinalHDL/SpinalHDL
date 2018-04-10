
import spinal.core._
import spinal.lib._
import spinal.lib.io.TriStateArray
import spinal.lib.bus.wishbone._

import spinal.lib.bus.wishbone._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.wishbone.sim._
import spinal.lib.io.InOutWrapper

class WishboneGPIO(config : WishboneConfig, gpioWidth : Int) extends Component{
  val io = new Bundle{
    val wb = slave(Wishbone(config))
    val gpio = master(TriStateArray(gpioWidth bits))
  }

  val ctrl = WishboneSlaveFactory(io.wb)

  //ctrl.read(io.gpio.read, 0)
  //ctrl.write(io.gpio.write, 0)
  //ctrl.driveAndRead(io.gpio.writeEnable, 4)
  //io.gpio.writeEnable.getDrivingReg init(0)

  ctrl.read(io.gpio.read, 0)
  ctrl.driveAndRead(io.gpio.write, 4)
  ctrl.driveAndRead(io.gpio.writeEnable, 8)
  io.gpio.writeEnable.getDrivingReg init(0)
}

object SpinalSimWishboneGpioTester{
  def main(args: Array[String]): Unit = {
    val compiled = SimConfig.allOptimisation.withWave.compile(
      rtl = new WishboneGPIO(
        config = WishboneConfig(8,8),
        gpioWidth = 8
      )
    )

    compiled.doSim("randomTransaction"){ dut =>
      dut.io.wb.CYC #= false
      dut.io.wb.ACK #= false
      dut.io.wb.STB #= false
      dut.io.wb.WE #= false
      dut.io.wb.ADR #= 0
      dut.io.wb.DAT_MOSI #= 0

      val d = BigInt(2)

      // WishboneTransaction(1) match{
      //     case WishboneTransaction(5,_,_,_,_) => println("rrrr")
      //     case _ => println("ffff")
      //     }

      dut.clockDomain.forkStimulus(period=10)
      dut.clockDomain.waitSampling(10)

      val driveMaster = new WishboneDrive(dut.io.wb, dut.clockDomain)

      driveMaster.write(WishboneTransaction(8,0))
      (0 to 10).suspendable.foreach{ tran =>
        dut.clockDomain.waitSampling()
        val Data = BigInt(scala.util.Random.nextInt(255))

        driveMaster.write(WishboneTransaction(4,Data))
        dut.clockDomain.waitSampling()
        //data = 0
        val ret = driveMaster.read(WishboneTransaction(0))
        ret match{
          case WishboneTransaction(_,Data,_,_,_) => println("Success")
          case _ => {
            println("Failure at %d, with DAT_MISO: %d DAT_MOSI: %d".format(tran,ret.data,Data))
            simFailure()
          }
        }
      }

      dut.clockDomain.waitSampling()
      simSuccess()
    }
  }
}

package spinal.lib.graphic.vga


import spinal.core._
import spinal.lib._
import spinal.lib.bus.avalon.{AvalonMMBus, AvalonMMConfig}
import spinal.lib.graphic._

case class AvalonVgaConfig(rgbConfig : RgbConfig){
//  def getControlConfig = AvalonMMConfig.fixed(4,32)
//  def getPictureConfig = AvalonMMConfig.fixed(4,32).getWriteOnlyConfig
}

class AvalonVgaCtrl(c : AvalonVgaConfig) extends Component{
  val io = new Bundle{
//    val control = slave (AvalonMMBus(c.getControlConfig))
    val pixel = slave Stream Fragment(Rgb(c.rgbConfig))
    val vga = master(Vga(c.rgbConfig))
  }

  val ctrl = new VgaCtrl(c.rgbConfig)
  ctrl.feedWith(io.pixel)


//  when(io.control.write){
//    switch(io.control.address){
//      is(0){
//        ctrl.io.timings.h.syncStart := io.control.writeData.asUInt.resized
//      }
//      is(1){
//        ctrl.io.timings.h.syncStart := io.control.writeData.asUInt.resized
//      }
//      is(2){
//        ctrl.io.timings.h.syncStart := io.control.writeData.asUInt.resized
//      }
//      is(3){
//        ctrl.io.timings.h.syncStart := io.control.writeData.asUInt.resized
//      }
//      is(4){
//        ctrl.io.timings.h.syncStart := io.control.writeData.asUInt.resized
//      }
//      is(5){
//        ctrl.io.timings.h.syncStart := io.control.writeData.asUInt.resized
//      }
//      is(6){
//        ctrl.io.timings.h.syncStart := io.control.writeData.asUInt.resized
//      }
//      is(7){
//        ctrl.io.timings.h.syncStart := io.control.writeData.asUInt.resized
//      }
//    }
//  }
}

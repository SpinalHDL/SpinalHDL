package spinal.lib.tools


import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config}
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}
import spinal.lib.com.uart.Uart
import spinal.lib.graphic.RgbConfig
import spinal.lib.graphic.vga.Vga
import spinal.lib.io.TriState
import spinal.tester.SpinalAnyFunSuite

case class UserDefineBus() extends Bundle with IMasterSlave {
  val aa = Bool()
  val bb = Bool()

  def asMaster() = {
    in(aa)
    out(bb)
  }
}

class InnerInnerApbModule extends Component {
  val apbConfig = Apb3Config(
    addressWidth = 12,
    dataWidth = 32,
    useSlaveError = true
  )
  val io = new Bundle {
    val apb_out = master(Apb3(apbConfig))
    val apb_in = slave(Apb3(apbConfig))
  }
  io.apb_out << io.apb_in
}

class InnerApbModule extends Component {
  val apbConfig = Apb3Config(
    addressWidth = 12,
    dataWidth = 32,
    useSlaveError = true
  )

  val io = new Bundle {
    val apb_out = master(Apb3(apbConfig))
    val apb_in = slave(Apb3(apbConfig))
  }
  val innerInnerApbModule = new InnerInnerApbModule()
  innerInnerApbModule.io.apb_in << io.apb_in
  io.apb_out << innerInnerApbModule.io.apb_out
}

class IPXACTGeneratorTestComponent extends Component {
  val apbConfig = Apb3Config(
    addressWidth = 12,
    dataWidth = 32,
    useSlaveError = true
  )
  val vgaConfig = RgbConfig(
    rWidth = 8, gWidth = 8, bWidth = 8
  )

  val innerApb3 = new InnerApbModule()
  val io = new Bundle {
    val apb_out = master(Apb3(apbConfig))
    val apb_in = slave(Apb3(apbConfig))
    val user_bus_in = slave port UserDefineBus()
    val user_bus_out = master port UserDefineBus()
    val stream_out = master(Stream(Bool()))
    val stream_in = slave(Stream(Bool()))
    val uart_out = master(Uart(ctsGen = true, rtsGen = true))
    val uart_in = slave(Uart(ctsGen = true, rtsGen = true))
    val vga_out = master(Vga(vgaConfig))
    val vga_in = slave(Vga(vgaConfig))
    val tri = slave(TriState(Bits(16 bits)))
    val analog = inout(Analog(Bits(16 bits)))
    val bits_input = in Bits ((8 bits))
    val bits_output = out Bits (8 bits)
  }
  io.bits_output := io.bits_input
  io.tri.read := io.analog
  when(io.tri.writeEnable) {
    io.analog := io.tri.write
  }
  io.vga_out << io.vga_in
  io.uart_out <> io.uart_in
  innerApb3.io.apb_in << io.apb_in
  io.apb_out << innerApb3.io.apb_out
  io.stream_out <-< io.stream_in
  io.user_bus_out <> io.user_bus_in

}

object IPXACTGeneratorDemo extends App {

  import IPXACTGenerator.IPXACTGenerator

  val componentRTL = SpinalConfig(
    mode = VHDL,
    oneFilePerComponent = false
  ).generate(new IPXACTGeneratorTestComponent)
  val componentGenerator = new IPXACTGenerator(componentRTL)
  //    componentGenerator.generateIPXACT2022Component()
  componentGenerator.generateIPXACTVivadoComponent()

  val designRTL = SpinalConfig(
    mode = VHDL,
    oneFilePerComponent = true
  ).generate(new IPXACTGeneratorTestComponent)
  val designGenerator = new IPXACTGenerator(designRTL)
  designGenerator.generateDesignView()
}


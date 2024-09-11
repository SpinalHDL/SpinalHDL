package spinal.lib.tools


import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config}
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}
import spinal.tester.SpinalAnyFunSuite
case class UserDefineBus() extends Bundle with IMasterSlave {
  val aa = Bool()
  val bb = Bool()
  def asMaster() = {
    in(aa)
    out(bb)
  }
}

class IPXACTGeneratorTestComponent extends Component{
  val ahbConfig = AhbLite3Config(
    addressWidth = 12,
    dataWidth    = 32
  )
  val apbConfig = Apb3Config(
    addressWidth = 12,
    dataWidth    = 32,
    useSlaveError = true
  )
  val axiConfig = Axi4Config(
    addressWidth = 32,
    dataWidth    = 32,
    idWidth      = 4
  )


  val io=new Bundle{
    val ahb_out = master(AhbLite3(ahbConfig))
    val ahb_in = slave(AhbLite3(ahbConfig))
    val apb_out = master(Apb3(apbConfig))
    val apb_in = slave(Apb3(apbConfig))
    val axi_out = master(Axi4(axiConfig))
    val axi_in = slave(Axi4(axiConfig))
    val user_bus_in = slave port UserDefineBus()
    val user_bus_out = master port UserDefineBus()
    val stream_out=master(Stream(Bool()))
    val stream_in=slave(Stream(Bool()))
  }
  io.stream_out<<io.stream_in
  io.user_bus_out <> io.user_bus_in
  io.axi_out<<io.axi_in
  io.ahb_out<>io.ahb_in
  io.apb_out<<io.apb_in

}
class IPXACTGeneratorTest extends SpinalAnyFunSuite{
  test("test IPXACTGeneratorTestComponent"){
    import IPXACTGenerator.IPXACTGenerator
    SpinalConfig(
      mode=Verilog,
      targetDirectory="./"
    ).generate(new IPXACTGeneratorTestComponent)

    val ipxactGenerrator=new IPXACTGenerator( SpinalVerilog(new IPXACTGeneratorTestComponent))
    ipxactGenerrator.generateIPXACT2022Component()
    ipxactGenerrator.generateIPXACTVivadoComponent()
  }
}


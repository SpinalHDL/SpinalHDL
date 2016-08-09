package spinal.lib.bus.amba3.ahblite

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._


object AhbLite3ToApb3BridgePhase extends SpinalEnum{
  val IDLE,SETUP,ACCESS = newElement
}

case class AhbLite3ToApb3Bridge(ahbConfig: AhbLite3Config,apbConfig: Apb3Config) extends Component{
  assert(ahbConfig.addressWidth == apbConfig.addressWidth)
  assert(ahbConfig.dataWidth == apbConfig.dataWidth)
  assert(apbConfig.selWidth == 1)
  import AhbLite3ToApb3BridgePhase._

  val io = new Bundle{
    val ahb = slave(AhbLite3(ahbConfig))
    val apb = master(Apb3(apbConfig))
  }

  val phase = RegInit(IDLE)
  val write = Reg(Bool)
  val address = Reg(ahbConfig.addressType)
  val readedData = Reg(ahbConfig.dataType)

  switch(phase){
    is(IDLE){
      io.apb.PSEL    := "0"
      io.apb.PENABLE := False
      io.ahb.HREADYOUT := True

      when(io.ahb.HSEL && io.ahb.HTRANS(1) && io.ahb.HREADY){
        phase := SETUP
        address := io.ahb.HADDR
        write := io.ahb.HWRITE
      }
    }
    is(SETUP){
      io.apb.PSEL    := "1"
      io.apb.PENABLE := False
      io.ahb.HREADYOUT := False
      phase := ACCESS
    }
    default{ //is(ACCESS)
      io.apb.PSEL    := "1"
      io.apb.PENABLE := True
      io.ahb.HREADYOUT := False

      when(io.apb.PREADY){
        readedData := io.ahb.HRDATA
        phase := IDLE
      }
    }
  }

  io.apb.PADDR  := address
  io.ahb.HRDATA := readedData
  io.apb.PWDATA := io.ahb.HWDATA
  io.apb.PWRITE := write
  io.ahb.HRESP := io.apb.PSLVERROR
}


object AhbLite3ToApb3Bridge{
  def main(args: Array[String]) {
    SpinalVhdl(AhbLite3ToApb3Bridge(AhbLite3Config(16,32),Apb3Config(16,32)))
  }
}
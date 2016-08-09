package spinal.lib.bus.amba3.ahblite


import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping

case class AhbLite3Decoder(AhbLite3Config: AhbLite3Config,decodings : Iterable[SizeMapping]) extends Component{
  val io = new Bundle{
    val input = slave(AhbLite3(AhbLite3Config))
    val outputs = Vec(master(AhbLite3(AhbLite3Config)),decodings.size)
  }
  val isIdle = io.input.isIdle
  val wasIdle = RegNextWhen(isIdle,io.input.HREADY) init(True)

  for((output,decoding) <- (io.outputs,decodings).zipped){
    output.HREADY    := io.input.HREADY
    output.HSEL      := decoding.hit(io.input.HADDR) && !isIdle
    output.HADDR     := io.input.HADDR
    output.HWRITE    := io.input.HWRITE
    output.HSIZE     := io.input.HSIZE
    output.HBURST    := io.input.HBURST
    output.HPROT     := io.input.HPROT
    output.HTRANS    := io.input.HTRANS
    output.HMASTLOCK := io.input.HMASTLOCK
    output.HWDATA    := io.input.HWDATA
  }

  val requestIndex = OHToUInt(io.outputs.map(_.HSEL))
  val dataIndex    = RegNextWhen(requestIndex,io.input.HREADY)
  io.input.HRDATA    := io.outputs(dataIndex).HRDATA
  io.input.HRESP     := io.outputs(dataIndex).HRESP
  io.input.HREADYOUT := io.outputs.map(_.HREADYOUT).reduce(_ & _)

  when(wasIdle){
    io.input.HRESP := False
  }
}
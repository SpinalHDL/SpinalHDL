package spinal.lib.bus.amba3.ahb


import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BaseSize

class Ahb3Decoder(ahb3Config: Ahb3Config,decodings : Seq[BaseSize]) extends Component{
  val io = new Bundle{
    val input = slave(Ahb3Master(ahb3Config))
    val outputs = Vec(master(Ahb3Slave(ahb3Config)),decodings.length)
  }

  val HREADY = io.outputs.map(_.HREADYOUT).reduce(_ & _)

  for((output,decoding) <- (io.outputs,decodings).zipped){
    output.HREADYIN  := HREADY
    output.HSEL      := decoding.hit(io.input.HADDR)
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
  io.input.HRDATA := io.outputs(dataIndex).HRDATA
  io.input.HRESP  := io.outputs(dataIndex).HRESP
  io.input.HREADY := HREADY
}
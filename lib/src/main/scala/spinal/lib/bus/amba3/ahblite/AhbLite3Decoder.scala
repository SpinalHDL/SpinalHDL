package spinal.lib.bus.amba3.ahblite


import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping

case class AhbLite3Decoder(AhbLite3Config: AhbLite3Config,decodings : Seq[SizeMapping]) extends Component{
  val io = new Bundle{
    val input = slave(AhbLite3(AhbLite3Config))
    val outputs = Vec(master(AhbLite3(AhbLite3Config)),decodings.size)
  }
  val isIdle = io.input.isIdle
  val wasIdle = RegNextWhen(isIdle,io.input.HREADY) init(True)
  val slaveReadyOutReduction = io.outputs.map(_.HREADYOUT).reduce(_ & _)
  val decodedSels = Vec(decodings.map(_.hit(io.input.HADDR) && !isIdle)).asBits
  val applyedSels = Bits(decodings.size bits)
  val previousSels = Reg(Bits(decodings.size bits)) init(0)
  val noneIdleSwitchDetected = previousSels =!= 0 && decodedSels =!= 0 && previousSels =!= decodedSels
  applyedSels      := !noneIdleSwitchDetected ? decodedSels     | 0
  val applyedHTRANS = !noneIdleSwitchDetected ? io.input.HTRANS | 0
  val applyedSlaveHREADY = noneIdleSwitchDetected ? slaveReadyOutReduction | io.input.HREADY
  when(applyedSlaveHREADY) {
    previousSels := applyedSels
  }

  for((output,decoding,sel) <- (io.outputs,decodings,applyedSels.asBools).zipped){
    output.HREADY    := applyedSlaveHREADY
    output.HSEL      := sel
    output.HADDR     := io.input.HADDR
    output.HWRITE    := io.input.HWRITE
    output.HSIZE     := io.input.HSIZE
    output.HBURST    := io.input.HBURST
    output.HPROT     := io.input.HPROT
    output.HTRANS    := applyedHTRANS
    output.HMASTLOCK := io.input.HMASTLOCK
    output.HWDATA    := io.input.HWDATA
  }

  val requestIndex = OHToUInt(io.outputs.map(_.HSEL))
  val dataIndex    = RegNextWhen(requestIndex,io.input.HREADY)
  val slaveHRDATA = io.outputs(dataIndex).HRDATA
  val slaveHRESP  = io.outputs(dataIndex).HRESP

  val switchBufferValid  = RegNextWhen(noneIdleSwitchDetected,applyedSlaveHREADY) init(False)
  val switchBufferHRDATA = RegNextWhen(slaveHRDATA,applyedSlaveHREADY)
  val switchBufferHRESP  = RegNextWhen(slaveHRESP,applyedSlaveHREADY)

  io.input.HRDATA    := switchBufferValid ? switchBufferHRDATA | slaveHRDATA
  io.input.HRESP     := switchBufferValid ? switchBufferHRESP  | slaveHRESP
  io.input.HREADYOUT := slaveReadyOutReduction && !noneIdleSwitchDetected

  when(wasIdle){
    io.input.HRESP := False
  }
}
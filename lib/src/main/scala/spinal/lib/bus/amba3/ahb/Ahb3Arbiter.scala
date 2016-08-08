package spinal.lib.bus.amba3.ahb

import spinal.core._
import spinal.lib._

//BUSY transfer, undefined length burst
//INCR
case class Ahb3Arbiter(ahb3Config: Ahb3Config,inputsCount : Int) extends Component{
  val io = new Bundle{
    val inputs = Vec(slave(Ahb3Slave(ahb3Config)),inputsCount)
    val output = master(Ahb3Slave(ahb3Config))
  }

  val locked = RegInit(False)
  val maskProposal = Bits(inputsCount bits)
  val maskLocked = Reg(Bits(inputsCount bits)) init(BigInt(1) << (inputsCount-1))
  val maskRouted = Mux(locked, maskLocked, maskProposal)

  when(io.output.HSEL) { //valid
    maskLocked := maskRouted
    locked := True

    when(io.output.HREADYOUT){ //fire
      when(io.output.last && !io.output.HMASTLOCK) { //End of burst and no lock
        locked := False
      }
    }
  }

  val requests = Vec(io.inputs.map(_.HSEL)).asBits
  maskProposal := OHMasking.roundRobin(requests,maskLocked(maskLocked.high-1 downto 0) ## maskLocked.msb)

  val requestIndex     = OHToUInt(maskRouted)
  io.output.HSEL      := (io.inputs, maskRouted.asBools).zipped.map(_.HSEL & _).reduce(_ | _)
  io.output.HADDR     := io.inputs(requestIndex).HADDR
  io.output.HREADYIN  := io.inputs(requestIndex).HREADYIN
  io.output.HWRITE    := io.inputs(requestIndex).HWRITE
  io.output.HSIZE     := io.inputs(requestIndex).HSIZE
  io.output.HBURST    := io.inputs(requestIndex).HBURST
  io.output.HPROT     := io.inputs(requestIndex).HPROT
  io.output.HTRANS    := io.output.HSEL ? io.inputs(requestIndex).HTRANS | "00"
  io.output.HMASTLOCK := io.inputs(requestIndex).HMASTLOCK

  val dataIndex        = RegNextWhen(requestIndex,io.output.HSEL && io.output.HREADYIN)
  io.output.HWDATA    := io.inputs(dataIndex).HWDATA

  for((input,requestRouted) <- (io.inputs,maskRouted.asBools).zipped){
    input.HRDATA    := io.output.HRDATA
    input.HRESP     := io.output.HRESP
    input.HREADYOUT := (io.output.HREADYOUT && requestRouted) || (!input.HSEL)
  }
}

package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

case class Axi4OnChipRam(axiConfig: Axi4Config,byteCount : BigInt) extends Component{
  assert(!axiConfig.useLock)
  val io = new Bundle {
    val axi = slave(Axi4(axiConfig))
  }

  val wordCount = byteCount / axiConfig.bytePerWord
  val ram = Mem(axiConfig.dataType,wordCount.toInt)
  val wordRange = log2Up(wordCount) + log2Up(axiConfig.bytePerWord)-1 downto log2Up(axiConfig.bytePerWord)



  val readLogic = new Area{
    case class State() extends Bundle{
      val busy = Bool
      val address = ram.addressType
      val id = axiConfig.idType
      val len = axiConfig.lenType
    }

    val stateNext = State()
    val state = RegNext(stateNext)
    val doMemRequest = Bool

    stateNext := state
    doMemRequest := state.busy

    when(!state.busy){
      io.axi.readCmd.ready := ???
    }

    when(io.axi.readCmd.fire){
      stateNext.busy := True
      doMemRequest := True
      stateNext.address := io.axi.readCmd.addr
      stateNext.id := io.axi.readCmd.id
      stateNext.len := io.axi.readCmd.len
    }

//    io.axi.readRsp.setOKAY()
//    io.axi.readRsp.data := ram.readSync(
//      address = doMemRequest,
//      enable = io.axi.HSEL && io.axi.HTRANS(1) && !io.axi.HWRITE && io.axi.HREADY
//    )
}



//  io.axi.readRsp.setOKAY()
//  io.axi.readRsp.data := ram.readSync(
//    address = io.axi.HADDR(wordRange),
//    enable = io.axi.HSEL && io.axi.HTRANS(1) && !io.axi.HWRITE && io.axi.HREADY
//  )
//
//  ram.write(
//    enable = pendingWrite.valid,
//    address = pendingWrite.address,
//    mask = pendingWrite.mask,
//    data = io.axi.HWDATA
//  )
}


object Axi4OnChipRam{
  def main(args: Array[String]) {
    SpinalVhdl(new Axi4OnChipRam(Axi4Config(32,32,4),1024))
  }
}
package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._

object Axi4SharedToApb3Bridge{
  def getConfigs(addressWidth : Int,dataWidth : Int,idWidth : Int) : Tuple2[Axi4Config,Apb3Config] =
  (
    Axi4Config(
      addressWidth = addressWidth,
      dataWidth = dataWidth,
      idWidth = idWidth,
      useLock = false,
      useRegion = false,
      useCache = false,
      useProt = false,
      useQos = false
    ),
    Apb3Config(
      addressWidth = addressWidth,
      dataWidth = dataWidth,
      selWidth = 1,
      useSlaveError = true
    )
  )

//  def apply(addressWidth : Int,dataWidth : Int,idWidth : Int) = new Axi4SharedToApb3Bridge(addressWidth,dataWidth,idWidth)
  def main(args: Array[String]) {
    SpinalVhdl(Axi4SharedToApb3Bridge(16,32,4).setDefinitionName("TopLevel"))
  }
}

object Axi4ToApb3BridgePhase extends SpinalEnum{
  val SETUP,ACCESS,RESPONSE = newElement
}

case class Axi4SharedToApb3Bridge(addressWidth : Int,dataWidth : Int,idWidth : Int) extends Component{
  val (axiConfig,apbConfig) = Axi4SharedToApb3Bridge.getConfigs(addressWidth,dataWidth,idWidth)
  import Axi4ToApb3BridgePhase._

  val io = new Bundle{
    val axi = slave (Axi4Shared(axiConfig))
    val apb = master(Apb3(apbConfig))
  }

  val phase = RegInit(SETUP)
  val write = Reg(Bool)
  val readedData = Reg(Bits(dataWidth bits))
  val id = Reg(UInt(idWidth bits))

  io.axi.sharedCmd.ready := False
  io.axi.writeData.ready    := False
  io.axi.writeRsp.valid     := False
  io.axi.readRsp.valid      := False

  io.apb.PSEL(0) := False
  io.apb.PENABLE := False

  switch(phase){
    is(SETUP){
      write := io.axi.sharedCmd.write
      id := io.axi.sharedCmd.id
      when(io.axi.sharedCmd.valid && (!io.axi.sharedCmd.write || io.axi.writeData.valid)) {
        phase := ACCESS
        io.apb.PSEL(0) := True
      }
    }
    is(ACCESS){
      io.apb.PSEL(0) := True
      io.apb.PENABLE := True

      when(io.apb.PREADY){
        readedData := io.apb.PRDATA
        phase := RESPONSE
        io.axi.sharedCmd.ready := True
        io.axi.writeData.ready    := write
      }
    }
    default { //RESPONSE
      when(write) {
        io.axi.writeRsp.valid := True
        when(io.axi.writeRsp.ready) {
          phase := SETUP
        }
      }otherwise {
        io.axi.readRsp.valid := True
        when(io.axi.writeRsp.ready) {
          phase := SETUP
        }
      }
    }
  }

  io.apb.PADDR  := io.axi.sharedCmd.addr
  io.apb.PWDATA := io.axi.writeData.data
  io.apb.PWRITE := io.axi.sharedCmd.write
  io.axi.readRsp.resp  := io.apb.PSLVERROR ## B"0"
  io.axi.writeRsp.resp := io.apb.PSLVERROR ## B"0"
  io.axi.readRsp.id  := id
  io.axi.writeRsp.id := id
  io.axi.readRsp.data := readedData
  io.axi.readRsp.last := True
}



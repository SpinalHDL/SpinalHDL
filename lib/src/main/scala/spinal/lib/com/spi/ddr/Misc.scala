package spinal.lib.com.spi.ddr


import spinal.core._
import spinal.lib._
import spinal.lib.com.spi.SpiKind

import scala.collection.mutable.ArrayBuffer



object SpiDdrMasterCtrl {
  def apply(p : Parameters) = new SpiDdrMasterCtrl(p)


  def main(args: Array[String]): Unit = {
    SpinalVerilog(new SpiDdrMasterCtrl(Parameters(8,12,SpiParameter(dataWidth = 4,ssWidth = 3)).addAllMods()))
  }
}




case class Ddr() extends Bundle with IMasterSlave{
  val writeEnable = Bool
  val read,write = Bits(2 bits)

  override def asMaster(): Unit = {
    out(write,writeEnable)
    in(read)
  }
}

case class SpiParameter(dataWidth : Int = 2,
                        ssWidth : Int = 1,
                        useSclk : Boolean = true)

case class SpiDdrMaster(p : SpiParameter) extends Bundle with IMasterSlave{
  import p._

  val sclk = ifGen(useSclk) (Ddr())
  val data = Vec(Ddr(), dataWidth)
  val ss   = if(ssWidth != 0) Bits(ssWidth bits) else null

  override def asMaster(): Unit = {
    master(sclk)
    if(ssWidth != 0) out(ss)
    data.foreach(master(_))
  }
}


//  case class ParameterMapping(position : Int, phase : Int)
case class ParameterMod(id : Int, writeMapping : Seq[Int], readMapping : Seq[Int]){
  assert(writeMapping.length == readMapping.length)
  def bitrate = readMapping.length
}
case class Parameters( dataWidth : Int,
                       timerWidth : Int,
                       spi : SpiParameter,
                       mods : ArrayBuffer[ParameterMod] = ArrayBuffer()){
  def ssGen = spi.ssWidth != 0
  def addFullDuplex(id : Int): this.type ={
    mods += ParameterMod(id, List(0), List(spi.dataWidth + 1))
    this
  }
  def addHalfDuplex(id : Int, spiWidth : Int, ddr : Boolean): this.type = {
    val low = 0 until spiWidth
    val top = spi.dataWidth until spi.dataWidth + spiWidth
    if(ddr)
      mods += ParameterMod(id, top ++ low, top ++ low)
    else
      mods += ParameterMod(id, low, top)
    this
  }
  def addAllMods(): this.type ={
    if(dataWidth >= 2) addFullDuplex(0)
    for((spiWidth, o) <- (2 to spi.dataWidth).filter(isPow2(_)).zipWithIndex){
      addHalfDuplex(2+o*2, spiWidth, false)
      if(spiWidth*2 <= dataWidth) addHalfDuplex(2+o*2 + 1, spiWidth, true)
    }
    this
  }
}

case class Config(p: Parameters) extends Bundle {
  val kind = SpiKind()
  val sclkToogle = UInt(p.timerWidth bits)
  val fullRate = Bool
  val mod = in UInt(log2Up(p.mods.map(_.id).max + 1) bits)

  val ss = ifGen(p.ssGen) (new Bundle {
    val activeHigh = Bits(p.spi.ssWidth bits)
    val setup = UInt(p.timerWidth bits)
    val hold = UInt(p.timerWidth bits)
    val disable = UInt(p.timerWidth bits)
  })
}

case class Cmd(p: Parameters) extends Bundle{
  val kind = Bool
  val read, write = Bool
  val data = Bits(p.dataWidth bits)

  def isData = !kind
  def isSs = kind
  def getSsEnable = data.msb
  def getSsId = U(data(0, log2Up(p.spi.ssWidth) bits))
}

case class Rsp(p: Parameters) extends Bundle{
  val data = Bits(p.dataWidth bits)
}

class SpiDdrMasterCtrl(p: Parameters) extends Component {

  val io = new Bundle {
    val config = in(Config(p))
    val cmd = slave(Stream(Cmd(p)))
    val rsp = master(Flow(Rsp(p)))
    val spi = master(master(SpiDdrMaster(p.spi)))
  }

  val timer = new Area{
    val counter = Reg(UInt(p.timerWidth bits))
    val reset = False
    val ss = ifGen(p.ssGen) (new Area{
      val setupHit    = counter === io.config.ss.setup
      val holdHit     = counter === io.config.ss.hold
      val disableHit  = counter === io.config.ss.disable
    })
    val sclkToogleHit = counter === io.config.sclkToogle

    counter := counter + 1
    when(reset){
      counter := 0
    }
  }



  val widths = p.mods.map(m => m.bitrate).distinct.sorted







  val fsm = new Area {
    val state = RegInit(False)
    val counter = Reg(UInt(log2Up(p.dataWidth) bits)) init(0)
    val counterPlus = counter +  io.config.mod.muxList(U(0), p.mods.map(m => m.id -> U(m.bitrate))).resized
    val readFill, readDone = RegNext(False) init(False)
    val ss = RegInit(B((1 << p.spi.ssWidth) - 1, p.spi.ssWidth bits))
    io.spi.ss := ss

    io.cmd.ready := False
    when(io.cmd.valid) {
      when(io.cmd.isData) {
        timer.reset := timer.sclkToogleHit
        when(timer.sclkToogleHit){
          state := !state
        }
        when((timer.sclkToogleHit && state) || io.config.fullRate) {
          counter := counterPlus
          readFill := True
          when(counterPlus === 0){
            io.cmd.ready := True
            readDone := io.cmd.read
          }
        }
      } otherwise {
        if (p.ssGen) {
          when(io.cmd.getSsEnable) {
            ss(io.cmd.getSsId) := False
            when(timer.ss.setupHit) {
              io.cmd.ready := True
            }
          } otherwise {
            when(!state) {
              when(timer.ss.holdHit) {
                state := True
                timer.reset := True
              }
            } otherwise {
              ss(io.cmd.getSsId) := True
              when(timer.ss.disableHit) {
                io.cmd.ready := True
              }
            }
          }
        }
      }
    }

    //Idle states
    when(!io.cmd.valid || io.cmd.ready){
      state := False
      counter := 0
      timer.reset := True
    }
  }


  val maxBitRate = p.mods.map(m => m.bitrate).max
  val outputPhy = new Area {

    val sclkWrite = Bits(2 bits)
    sclkWrite := 0
    when(io.cmd.valid && io.cmd.isData){
      when(io.config.fullRate){
        sclkWrite := !io.config.kind.cpha ## io.config.kind.cpha
      } otherwise {
        sclkWrite := (default -> (fsm.state ^ io.config.kind.cpha))
      }
    }


    io.spi.sclk.writeEnable := True
    io.spi.sclk.write := sclkWrite ^ B(sclkWrite.range -> io.config.kind.cpol)




    val dataWrite = Bits(maxBitRate bits)
    val widthSel = io.config.mod.muxList(U(0), p.mods.map(m => m.id -> U(widths.indexOf(m.bitrate))))
    dataWrite.assignDontCare()
    switch(widthSel){
      for((width, widthId) <- widths.zipWithIndex){
        is(widthId){
          dataWrite(0, width bits) := io.cmd.data.subdivideIn(width bits).reverse(fsm.counter >> log2Up(width))
        }
      }
    }


    io.spi.data.foreach(_.writeEnable := False)
    io.spi.data.foreach(_.write.assignDontCare())

    switch(io.config.mod){
      for(mod <- p.mods){
        val modIsDdr = mod.writeMapping.exists(_ >= p.spi.dataWidth)
        is(mod.id) {
          when(io.cmd.valid && io.cmd.write){
            mod.writeMapping.map(_ % p.spi.dataWidth).distinct.foreach(i => io.spi.data(i).writeEnable := True)
          }

          when(io.config.fullRate){
            for((targetId, sourceId) <- mod.writeMapping.zipWithIndex){
              io.spi.data(targetId % p.spi.dataWidth).write(targetId / p.spi.dataWidth) := dataWrite(sourceId)
              if(!modIsDdr) io.spi.data(targetId % p.spi.dataWidth).write(1-targetId / p.spi.dataWidth) := dataWrite(sourceId)
            }
          } otherwise {
            if(modIsDdr) {
              when(!fsm.state) {
                for ((targetId, sourceId) <- mod.writeMapping.zipWithIndex if targetId < p.spi.dataWidth) {
                  io.spi.data(targetId % p.spi.dataWidth).write := (default -> dataWrite(sourceId))
                }
              } otherwise {
                for ((targetId, sourceId) <- mod.writeMapping.zipWithIndex if targetId >= p.spi.dataWidth) {
                  io.spi.data(targetId % p.spi.dataWidth).write := (default -> dataWrite(sourceId))
                }
              }
            } else {
              for ((targetId, sourceId) <- mod.writeMapping.zipWithIndex) {
                io.spi.data(targetId % p.spi.dataWidth).write := (default -> dataWrite(sourceId))
              }
            }
          }
        }
      }
    }
  }


  val inputPhy = new Area{
    val mod = RegNext(io.config.mod)
    val fullRate = RegNext(io.config.fullRate)
    val buffer = Reg(Bits(p.dataWidth - p.mods.map(_.bitrate).min bits))
    val bufferNext = Bits(p.dataWidth bits).assignDontCare().allowOverride
    val widthSel = mod.muxList(U(0),p.mods.map(m => m.id -> U(widths.indexOf(m.bitrate))))
    val dataWrite, dataRead = Bits(maxBitRate bits)
    val dataReadBuffer = RegNextWhen(Cat(io.spi.data.map(_.read(1))), !RegNext(fsm.state, init = False))
    val dataReadSource = Cat(io.spi.data.map(_.read(0))) ## dataReadBuffer

    dataRead.assignDontCare()

    switch(mod){
      for(mod <- p.mods){
        val modIsDdr = mod.writeMapping.exists(_ >= p.spi.dataWidth)
        is(mod.id) {
          when(fullRate){
            for((sourceId, targetId) <- mod.readMapping.zipWithIndex) {
              dataRead(targetId) := io.spi.data(sourceId % p.spi.dataWidth ).read(sourceId / p.spi.dataWidth)
            }
          } otherwise {
            for((sourceId, targetId) <- mod.readMapping.zipWithIndex) {
              dataRead(targetId) := dataReadSource(sourceId)
            }
          }
        }
      }
    }


    switch(widthSel) {
      for ((width,widthId) <- widths.zipWithIndex) {
        is(widthId) {
          bufferNext := (buffer ## dataRead(0, width bits)).resized
          when(fsm.readFill) { buffer := bufferNext.resized }
        }
      }
    }

    io.rsp.valid := fsm.readDone
    io.rsp.data := bufferNext
  }
}
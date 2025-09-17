package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline.{StageCtrlPipeline, StagePipeline}


case class Axi4OnePerIdRemapParam(
                                   slots: Int = 4,
                                   addressMatchWidth: Int = 999
                                 ) {
  def getAxDownConfig(config: Axi4Config) = config.copy(idWidth = log2Up(slots))
}

// !! Assumes responses come back in the same order across ID !!
class Axi4AxOnePerIdRemap[T <: Axi4Ax](p: Axi4OnePerIdRemapParam,
                                       axUp: Stream[T],
                                       axDown: Stream[T],
                                       completion: Flow[UInt],
                                       boundaryWidth: Int = Axi4.boundaryWidth) extends Area {
  val axiConfig = axUp.config
  val addressMatchWidth = p.addressMatchWidth.min(axiConfig.addressWidth)

  assert(addressMatchWidth >= boundaryWidth)
  val slots = List.fill(p.slots)(new Area {
    val valid = RegInit(False)
    val id = Reg(UInt(axiConfig.idWidth bits))
    val page = Reg(UInt(addressMatchWidth - boundaryWidth bits))
    val startAt, endAt = Reg(UInt(boundaryWidth bits))
  })
  val slotsFreeId = Counter(p.slots)
  val slotsFull = slots.map(_.valid).read(slotsFreeId)

  val onAx = new StageCtrlPipeline {
    val inserter = new Ctrl(0) {
      up.arbitrateFrom(axUp)
      val CMD = up.insert(axUp.payload)
    }

    val calc = new Ctrl(0) {
      val END_AT = insert(inserter.CMD.addr.resize(boundaryWidth) + inserter.CMD.getBurstBytesMinusOne(boundaryWidth))
    }
    val checks = new Ctrl(1) {
      val offset = inserter.CMD.addr(boundaryWidth - 1 downto 0)
      val page = inserter.CMD.addr(addressMatchWidth - 1 downto boundaryWidth)
      val hits = slots.map(s => s.valid && s.id === inserter.CMD.id && s.page === page && s.startAt <= calc.END_AT && s.endAt >= offset)
      val hit = hits.orR
      haltWhen(hit)
      haltWhen(slotsFull)

      down.arbitrateTo(axDown)
      axDown.payload := inserter.CMD
      axDown.id.removeAssignments() := slotsFreeId

      when(down.isFiring) {
        slots.onSel(slotsFreeId) { s =>
          s.valid := True
          s.id := inserter.CMD.id
          s.page := page
          s.startAt := offset
          s.endAt := calc.END_AT
        }
        slotsFreeId.increment()
      }
    }
  }
  onAx.build()

  val onCompletion = new Area {
    when(completion.valid) {
      slots.onSel(completion.payload) { s => s.valid := False }
    }
    val upId = slots.map(_.id).read(completion.payload)
  }
}

// !! Assumes B responses come back in the same order across ID !!
class Axi4WriteOnlyOnePerIdRemap(axiConfig: Axi4Config, 
                                 p: Axi4OnePerIdRemapParam, 
                                 boundaryWidth : Int = Axi4.boundaryWidth) extends Component {
  val io = new Bundle {
    val up = slave port Axi4WriteOnly(axiConfig)
    val down = master port Axi4WriteOnly(p.getAxDownConfig(axiConfig))
  }

  val completion = Flow(io.down.aw.id)
  completion.valid := io.down.b.fire
  completion.payload := io.down.b.id

  val filter = new Axi4AxOnePerIdRemap(
    p,
    io.up.aw,
    io.down.aw,
    completion,
    boundaryWidth = boundaryWidth
  )

  io.down.w << io.up.w

  io.up.b << io.down.b
  io.up.b.id.removeAssignments() := filter.onCompletion.upId
}


object Axi4WriteOnlyOnePerIdRemap extends App {
  SpinalVerilog(new Axi4WriteOnlyOnePerIdRemap(
    Axi4Config(16, 32, 4),
    Axi4OnePerIdRemapParam()
  ))
}

// !! Assumes R responses come back in the same order across ID !!
class Axi4ReadOnlyOnePerIdRemap(axiConfig: Axi4Config, p: Axi4OnePerIdRemapParam, boundaryWidth : Int = Axi4.boundaryWidth) extends Component {
  val io = new Bundle {
    val up = slave port Axi4ReadOnly(axiConfig)
    val down = master port Axi4ReadOnly(p.getAxDownConfig(axiConfig))
  }

  val completion = Flow(io.down.ar.id)
  completion.valid := io.down.r.fire && io.down.r.last
  completion.payload := io.down.r.id

  val filter = new Axi4AxOnePerIdRemap(
    p,
    io.up.ar,
    io.down.ar,
    completion,
    boundaryWidth = boundaryWidth
  )

  io.up.r << io.down.r
  io.up.r.id.removeAssignments() := filter.onCompletion.upId
}


object Axi4ReadOnlyOnePerIdRemap extends App {
  SpinalVerilog(new Axi4ReadOnlyOnePerIdRemap(
    Axi4Config(16, 32, 4),
    Axi4OnePerIdRemapParam()
  ))
}
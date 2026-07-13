package spinal.lib.misc.aia

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class ImsicFileInfo(
  hartId        : Int,
  guestId       : Int,
  sourceIds     : Seq[Int],
  groupId       : Int,
  groupHartId   : Int
)

object ImsicFileInfo {
  def apply(hartId: Int, guestId: Int, sourceIds: Seq[Int]): ImsicFileInfo = ImsicFileInfo(
    hartId = hartId,
    guestId = guestId,
    sourceIds = sourceIds,
    groupId = 0,
    groupHartId = hartId
  )

  def apply(hartId: Int, sourceIds: Seq[Int]): ImsicFileInfo = ImsicFileInfo(
    hartId = hartId,
    guestId = 0,
    sourceIds = sourceIds,
    groupId = 0,
    groupHartId = hartId
  )
}

case class ImsicFile(hartId: Int, guestId: Int, sourceNum: Int) extends Area {
  val sourceIds = 1 until sourceNum
  val idWidth = log2Up(sourceNum)

  require(isPow2(sourceNum))

  val threshold = RegInit(U(0, idWidth bits))

  val interrupts = for ((sourceId, idx) <- sourceIds.zipWithIndex) yield new Area {
    val id = sourceId
    val ie = RegInit(False)
    val ip = RegInit(False)
    val iep = ie & ip
  }

  val trigger = Stream(UInt(ImsicTriggerMapper.registerWidth bits))
  when(trigger.valid) {
    switch (trigger.payload) {
      for (interrupt <- interrupts) {
        is (interrupt.id) {
          interrupt.ip.set()
        }
      }
    }
  }
  trigger.ready := True

  val ieps = interrupts.map{i => i.iep}.asBits ## False
  val result = CountTrailingZeroes(ieps)
  val identity = result.resize(idWidth).andMask(threshold === 0 || result < threshold)

  def claim(id: UInt) = new Area {
    switch(id) {
      for (i <- interrupts) {
        is (i.id) {
          i.ip.clear()
        }
      }
    }
  }

  def asImsicFileInfo(hartPerGroup: Int = 0): ImsicFileInfo = ImsicFileInfo(
    hartId      = hartId,
    guestId     = guestId,
    sourceIds   = sourceIds,
    groupId     = if (hartPerGroup == 0) 0 else (hartId / hartPerGroup),
    groupHartId = if (hartPerGroup == 0) hartId else (hartId % hartPerGroup)
  )
}

object ImsicFile {
  def apply(hartId: Int, sourceNum: Int): ImsicFile = ImsicFile(hartId, 0, sourceNum)

  def currentFileStatus(files: Seq[ImsicFile], mux: UInt) = new Area {
    assert(files.map(_.sourceIds).toSet.size == 1, "All file should have the same source configuration")
    val sourceIds = files(0).sourceIds
    val idWidth = files(0).idWidth
    val vecMux = mux.resized

    def toFileVec[T <: Data](f : ImsicFile => T) : Vec[T] = Vec(files.map(f(_)))

    val threshold = toFileVec(_.threshold)(vecMux)

    val interrupts = for ((sourceId, idx) <- sourceIds.zipWithIndex) yield new Area {
      val id = sourceId
      val ie = toFileVec(_.interrupts(idx).ie)(vecMux)
      val ip = toFileVec(_.interrupts(idx).ip)(vecMux)
    }

    val identity = toFileVec(_.identity)(vecMux)

    def claim(id: UInt) = new Area {
      switch(id) {
        for (i <- interrupts) {
          is (i.id) {
            i.ip.clear()
          }
        }
      }
    }
  }
}


object ImsicOp extends SpinalEnum {
  val READ, WRITE = newElement()
}

case class ImsicCmd(addressWidth: Int, xlen: Int) extends Bundle {
  val op = ImsicOp()
  val doIp = Bool()
  val address = UInt(addressWidth bits)
  val data = Bits(xlen bits)
  val mask = Bits(xlen bits)
}

case class ImsicRsp(xlen: Int) extends Bundle {
  val data = Bits(xlen bits)
}

case class ImsicAccess(addressWidth: Int, xlen: Int) extends Bundle with IMasterSlave {
  val cmd = Stream(ImsicCmd(addressWidth, xlen))
  val rsp = Flow(ImsicRsp(xlen))

  def asMaster() = {
    master(cmd)
    slave(rsp)
  }
}

case class ImsicFileParameters(
  hartId: Int,
  guestId: Int,
  sourceNum: Int,
  xlen: Int,
  portNum: Int = 2
)

case class ImsicFileRamLogic(p: ImsicFileParameters) extends Component {
  import p._

  require(isPow2(sourceNum))
  require(isPow2(xlen))

  val lineNum = sourceNum / xlen
  val lineWidth = log2Up(lineNum)
  val idWidth = log2Up(sourceNum)

  val ie = Mem.fill(lineNum)(Bits(xlen bits)) initBigInt(Seq.fill(lineNum)(0))
  val ip = Mem.fill(lineNum)(Bits(xlen bits)) initBigInt(Seq.fill(lineNum)(0))
  val iepCache = Vec.fill(lineNum)(RegInit(False))
  val identity = RegInit(U(0, idWidth bits))

  val io = new Bundle {
    val port = Vec.fill(portNum)(slave(ImsicAccess(lineWidth, xlen)))
    val identity = out UInt(idWidth bits)
  }

  io.identity := identity

  val arbiter = StreamArbiterFactory().lowerFirst.transactionLock.buildOn(io.port.map(_.cmd))
  val portOhReg = Reg(Bits(io.port.size bits))

  val port = new Area {
    val fire = False
    val address = UInt(lineWidth bits)
    val addressOld = RegNextWhen(address, fire)
    address := addressOld

    val doIp = Reg(Bool())

    val dataMask = B(xlen bits, 0 -> address.orR, default -> True)

    val read = new Area {
      val ipData = ip.readSync(address, fire) & dataMask
      val ieData = ie.readSync(address, fire) & dataMask
      val data = doIp.mux(ipData, ieData)

      val maskedData = doIp.mux(ieData, ipData)
    }

    val write = new Area {
      val valid = False
      val data = Reg(Bits(xlen bits))
      val mask = Reg(Bits(xlen bits))

      val writeData = ((read.data & ~mask) | data) & dataMask

      ie.write(address, writeData, valid & !doIp)
      ip.write(address, writeData, valid & doIp)
    }
  }

  arbiter.io.output.ready := False

  io.port.map(_.rsp).foreach { rsp =>
    rsp.valid := False
    rsp.data := port.read.data
  }

  val fsm = new StateMachine {
    val IDLE, READ, WRITE, QUERY, UPDATE = new State
    setEntry(IDLE)

    val op = Reg(ImsicOp())
    val mask = Reg(Bits(xlen bits))
    val data = Reg(Bits(xlen bits))

    IDLE whenIsActive {
      when(arbiter.io.output.valid) {
        portOhReg := arbiter.io.chosenOH
        op := arbiter.io.output.payload.op
        port.write.data := arbiter.io.output.payload.data
        port.write.mask := arbiter.io.output.payload.mask
        port.doIp := arbiter.io.output.payload.doIp
        port.address := arbiter.io.output.payload.address
        port.fire := True
        arbiter.io.output.ready := True

        switch (arbiter.io.output.payload.op) {
          is(ImsicOp.READ) {
            goto(READ)
          }
          is(ImsicOp.WRITE) {
            goto(WRITE)
          }
        }
      }
    }

    READ whenIsActive {
      io.port.onMask(portOhReg){port =>
        port.rsp.valid := True
      }

      goto(IDLE)
    }

    WRITE whenIsActive {
      port.write.valid := True
      iepCache(port.address) := (port.read.maskedData & port.write.writeData).orR

      goto(QUERY)
    }

    QUERY whenIsActive {
      val queryAddress = CountTrailingZeroes(iepCache.asBits).resized
      port.address := queryAddress
      port.fire := True
      goto(UPDATE)
    }

    UPDATE whenIsActive {
      val result = port.address @@ CountTrailingZeroes(port.read.ipData & port.read.ieData).resize(log2Up(xlen))
      identity := result.resize(idWidth)

      io.port.onMask(portOhReg){port =>
        port.rsp.valid := True
      }

      goto(IDLE)
    }
  }
}

case class ImsicFileRam(p: ImsicFileParameters) extends Area {
  import p._
  val logic = ImsicFileRamLogic(p)
  val idWidth = logic.idWidth
  val ports = logic.io.port
  val threshold = RegInit(U(0, idWidth bits))
  val result = logic.io.identity
  val identity = result.andMask(threshold === 0 || result < threshold)
  val interrupt = identity.orR

  def asImsicFileInfo(hartPerGroup: Int = 0): ImsicFileInfo = ImsicFileInfo(
    hartId      = hartId,
    guestId     = guestId,
    sourceIds   = 1 until sourceNum,
    groupId     = if (hartPerGroup == 0) 0 else (hartId / hartPerGroup),
    groupHartId = if (hartPerGroup == 0) hartId else (hartId % hartPerGroup)
  )
}

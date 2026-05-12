package spinal.lib.misc.aia

import spinal.core._
import spinal.lib._

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

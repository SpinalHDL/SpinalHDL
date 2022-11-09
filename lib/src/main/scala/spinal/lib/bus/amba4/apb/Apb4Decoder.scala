package spinal.lib.bus.amba4.apb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping

object Apb4Decoder{
  def apply(mst: Apb4, maps: Seq[SizeMapping]): Vec[Apb4] = {
    SizeMappingCheck(maps)

    val c = mst.c
    val slvs = Vec(master(Apb4(c.copy(selWidth = 1))), maps.size)
    val apbdec = Apb4(c.copy(selWidth = maps.size))

    maps.zipWithIndex.foreach{ case(sm, i) =>
      apbdec.PSEL(i) := sm.hit(mst.PADDR) && mst.PSEL.lsb
      slvs(i).PADDR   := mst.PADDR
      slvs(i).PENABLE := mst.PENABLE
      slvs(i).PSEL    := apbdec.PSEL(i).asBits
      slvs(i).PWRITE  := mst.PWRITE
      slvs(i).PWDATA  := mst.PWDATA
      slvs(i).PSTRB   := mst.PSTRB
      slvs(i).PPROT   := mst.PPROT
    }

    val pselid = OHToUInt(apbdec.PSEL)
    pselid.dontSimplifyIt().setName("psel_index")

    mst.PREADY := slvs(pselid).PREADY
    mst.PRDATA := slvs(pselid).PRDATA

    if(c.useSlaveError) {
      mst.PSLVERR  := apbdec.PSLVERR
      apbdec.PSLVERR  := slvs(pselid).PSLVERR
    }

    when(mst.PSEL.lsb && apbdec.PSEL === 0){
      mst.PREADY := True
      if(c.useSlaveError){
        mst.PSLVERR := True
      }
    }
    slvs
  }
}
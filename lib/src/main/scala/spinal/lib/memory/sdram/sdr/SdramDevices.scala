package spinal.lib.memory.sdram.sdr

import spinal.core._
import spinal.lib.memory.sdram.SdramGeneration._
import spinal.lib.memory.sdram.SdramLayout

object IS42x320D {
  def layout = SdramLayout(
    generation = SDR,
    bankWidth   = 2,
    columnWidth = 10,
    rowWidth    = 13,
    dataWidth   = 16
  )

  def timingGrade7 = SdramTimings(
    bootRefreshCount =   8,
    tPOW             = 100 us,
    tREF             =  64 ms,
    tRC              =  60 ns,
    tRFC             =  60 ns,
    tRAS             =  37 ns,
    tRP              =  15 ns,
    tRCD             =  15 ns,
    cMRD             =   2,
    tWR              =  10 ns,
    cWR              =   1
  )
}


object MT48LC16M16A2 {
  def layout = SdramLayout(
    generation = SDR,
    bankWidth = 2,
    columnWidth = 9,
    rowWidth = 13,
    dataWidth = 16
  )

  def timingGrade7 = SdramTimings(
    bootRefreshCount =   8,
    tPOW             = 100 us,
    tREF             =  64 ms,
    tRC              =  60 ns,
    tRFC             =  66 ns,
    tRAS             =  37 ns,
    tRP              =  15 ns,
    tRCD             =  15 ns,
    cMRD             =   2,
    tWR              =  7.5 ns,
    cWR              =  1
  )
}


object W9825G6JH6 {
  def layout = SdramLayout(
    generation = SDR,
    bankWidth = 2,
    columnWidth = 9,
    rowWidth = 13,
    dataWidth = 16
  )

  def timingGrade7 = SdramTimings(
    bootRefreshCount =   8,
    tPOW             = 200 us,
    tREF             =  64 ms,
    tRC              =  60 ns,
    tRFC             =  60 ns,
    tRAS             =  42 ns,
    tRP              =  18 ns,
    tRCD             =  18 ns,
    cMRD             =  2,
    tWR              =  7.5 ns,
    cWR              =  1
  )
}



object MT41K128M16JT{
  def layout = SdramLayout(
    generation = DDR3,
    bankWidth = 3,
    columnWidth = 10,
    rowWidth = 14,
    dataWidth = 16
  )
}


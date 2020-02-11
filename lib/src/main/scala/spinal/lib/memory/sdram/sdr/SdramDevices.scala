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


object AS4C32M16SB {
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
    tRFC             =  66 ns,
    tRAS             =  37 ns,
    tRP              =  15 ns,
    tRCD             =  15 ns,
    cMRD             =   2,
    tWR              =  10 ns,
    cWR              =   1
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

object EG4S20 {
  def layout = SdramLayout(
    generation = SDR,
    bankWidth = 2,
    columnWidth = 8,
    rowWidth = 11,
    dataWidth = 32
  )

  def timingGrade7 = SdramTimings(
    bootRefreshCount =   8,
    tPOW             = 200 us,
    tREF             =  64 ms,
    tRC              =  80 ns,
    tRFC             =  80 ns,
    tRAS             =  60 ns,
    tRP              =  40 ns,
    tRCD             =  40 ns,
    cMRD             =  3,
    tWR              =  60 ns,
    cWR              =  3
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



object MT47H64M16HR{
  def layout = SdramLayout(
    generation = DDR2,
    bankWidth = 3,
    columnWidth = 10,
    rowWidth = 13,
    dataWidth = 16
  )
}





/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package spinal

/**
 * Created by PIC18F on 30.01.2015.
 */
class VhdlTestBenchBackend extends VhdlBase {
  var out: java.io.FileWriter = null
  var library = "work"
  val packageName = "pkg_scala2hdl"
  // + Random.nextInt(100000)
  val outputFile = "out_tb"
  // + Random.nextInt(100000)
  val tbName = "testBench" // + Random.nextInt(100000)


  def elaborate(toplevel: Component): Unit = {
    val tbFile = new java.io.FileWriter(outputFile + ".vhd")

    val ret = new StringBuilder()

    ret ++= s"""library IEEE;
              |use IEEE.STD_LOGIC_1164.ALL;
              |use IEEE.NUMERIC_STD.all;
              |
              |entity $tbName is
              |end $tbName;
              |
              |architecture arch of $tbName is
              |""".stripMargin

    emitSignals(toplevel, ret)
    ret ++= "begin\n"
    emitComponentInstance(toplevel, ret)
    ret ++= "end arch;\n"


    tbFile.write(ret.result())
    tbFile.flush();
    tbFile.close();
  }

  def emitSignals(c: Component, ret: StringBuilder): Unit = {
    for (io <- c.getNodeIo) {
      ret ++= emitSignal(io, io);
    }
  }

  def emitComponentInstance(c: Component, ret: StringBuilder): Unit = {
    val definitionString = s"entity $library.${c.definitionName}"
    ret ++= s"  uut : $definitionString\n"
    ret ++= s"    port map (\n"
    for (data <- c.getNodeIo) {
      ret ++= s"      ${emitReference(data)} =>  ${emitReference(data)},\n"
    }
    ret.setCharAt(ret.size - 2, ' ')

    ret ++= s"    );"
    ret ++= s"\n"

  }

}

package spinal.lib.com.jtag

import spinal.lib._

// adding ECP5 JtagTap Support
import blackbox.lattice.ecp5.{JTAGG}
import spinal.lib.com.jtag.lattice.ecp5.{JtagTap => JtagTap_Lattice_ECP5}

//══════════════════════════════════════════════════════════════════════════════
// define JTagTapFactory object
//
object JtagTapFactory{

  // Generic JtagTap implementation
  // usage:
  // import spinal.lib.com.jtag.{Jtag, JtagTapFactory}
  // ...
  // val io    = new Bundle(
  //    val jtag     = slave(Jtag())
  // )
  //...
  // val debugtap = ClockDomain(io.jtag.tck, reset)(new Area{
  //    val tap = JtagTapFactory(io.jtag, 8)
  //    val idcodeArea = tap.idcode(B"x10001FFF")(1)
  //    val readArea = tap.read(B(0xAE, 8 bits))(2)
  //    val writeArea = tap.write(io.nLED, cleanUpdate=true)(3)
  // })
  def apply(jtag: Jtag, instructionWidth: Int) = {
    new JtagTap(jtag, instructionWidth)
  }

  // Lattice ECP5 JTAGG specific implementation
  // usage:
  // import spinal.lib.com.jtag.{JtagTapFactory}
  // import spinal.lib.blackbox.lattice.ecp5.{JTAGG, JtaggGeneric}
  //
  // val jtagg = new JTAGG(JtaggGeneric().copy())
  // val debugtap = ClockDomain(jtagg.io.JTCK, reset)(new Area{
  //    val tap = JtagTapFactory(jtagg, 8)
  //    val readArea1 = tap.read(B(0xAE, 8 bits))(0x38)
  //    val writeArea = tap.write(io.nLED, cleanUpdate=true)(0x32)
  // })
  def apply(jtag: JTAGG, instructionWidth: Int) = {
    new JtagTap_Lattice_ECP5(jtag.io, instructionWidth)
  }


}

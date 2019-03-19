package spinal.lib.eda.yosys

import scala.collection._
import java.nio.file.{Path, Paths}


// -v verbose (repeat to increase verbosity)

/** Compose the command line for creating the FPGA bynary or vice-versa (icepack) */
case class IcePack(_asc: Option[Path] = None,
                   _bin: Option[Path] = None,
                   _unpack: Boolean = false,
                   _deepsleep: Boolean = false,
                   _writeCram: Boolean = false,
                   _writeCramFill: Boolean = false,
                   _writeCramCheckerboard: Boolean = false,
                   _writeOnlyBram: Boolean = false,
                   _writeBank: Seq[Int] = Seq.empty[Int],
                   workDir: String = ".",
                   prerequisite: mutable.MutableList[Makeable]= mutable.MutableList[Makeable]())
    extends Makeable{

  //override val logFile = "icepack.log"

  /** Specify the path of the bin file
    *
    * @param bin the path of the bin file
    */
  def bin(bin: Path) = this.copy(_bin = Some(bin))

  /** Specify the path of the asc file
    *
    * @param asc the path of the asc file
    * @param allowUncostrained a flag to allow all uncostrained I/O
    */
  def asc(asc: Path) = this.copy(_asc = Some(asc))

  /** Create a asc file from a bin*/
  def unpack = this.copy(_unpack = true)

  /** Disable deep sleep after program */
  def disableDeepSleep = this.copy(_deepsleep = false)

  /** Write cram bitmap as netpbm file */
  def writeCram = this.copy(_writeCram = true)

  /** write cram bitmap (fill tiles) as netpbm file */
  def writeCramFill = this.copy(_writeCramFill = true)

  /** write cram bitmap (checkerboard) as netpbm file */
  def writeCramCheckerboard = this.copy(_writeCramCheckerboard = true)

  /** write bram data, not cram, to the netpbm file */
  def writeOnlyBram = this.copy(_writeOnlyBram = true)

  /** only include the specified bank in the netpbm file
    *
    * @param value the banks to write
    */
  def writeBank(value: Int*) = this.copy(_writeBank = value)

  /** Change the output folder of all the target/output
    *
    * @param path the path where redirect all the outputs
    */
  def outputFolder(path: Path): IcePack ={
    val newAsc  = if(_asc.nonEmpty) Some(path.resolve(_asc.get)) else None
    val newBin  = if(_bin.nonEmpty) Some(path.resolve(_bin.get)) else None
    val ret = if(_unpack) this.copy(_bin=newAsc) else this.copy(_asc=newAsc)
    ret
  }

  override def toString(): String = {
    val ret = new StringBuilder("icepack ")
    if (_unpack) ret.append("-u ") // -u unpack mode (implied when called as 'iceunpack')
    if (_deepsleep) ret.append("-s ") // -s disable final deep-sleep SPI flash command after bitstream is loaded
    if (_writeCram) ret.append("-b ") // -b write cram bitmap as netpbm file
    if (_writeCramFill) ret.append("-f ") // -f write cram bitmap (fill tiles) as netpbm file
    if (_writeCramCheckerboard) ret.append("-c ") // -c  write cram bitmap (checkerboard) as netpbm file repeat to flip the selection of tiles
    if (_writeOnlyBram) ret.append("-r ") // -r write bram data, not cram, to the netpbm file
    if (_writeBank.nonEmpty) _writeBank.foreach(v => ret.append(s"-B${v} ")) // -B0, -B1, -B2, -B3 only include the specified bank in the netpbm file

    if(_unpack){
      ret.append(s"""${_bin.getOrElse(Paths.get("icepack.bin"))} """)
      ret.append(s"""${_asc}.getOrElse(Paths.get("icepack.asc")) """)
    } else {
      ret.append(s"""${_asc}getOrElse(Paths.get("icepack.asc")) """)
      ret.append(s"""${_bin}.getOrElse(Paths.get("icepack.bin")) """)
    }

    ret.toString()
  }

  //make stuff
  override def needs = List(if(_unpack) "bin" else "asc")
  override def target = super.target ++ List(if(_unpack) _asc.get else _bin.get)
  override def makeComand: String =
    if(_unpack) this.bin(getPrerequisiteFromExtension("bin")).toString
    else        this.asc(getPrerequisiteFromExtension("asc")).toString
}

// Simple programming tool for FTDI-based Lattice iCE programmers.
// Usage: iceprog [-b|-n|-c] <input file>
//        iceprog -r|-R<bytes> <output file>
//        iceprog -S <input file>
//        iceprog -t
// General options:
//   -v                    verbose output
// Mode of operation:
//   [default]             write file contents to flash, then verify
//   -r                    read first 256 kB from flash and write to file
//   -R <size in bytes>    read the specified number of bytes from flash
//                           (append 'k' to the argument for size in kilobytes,
//                           or 'M' for size in megabytes)
//   -c                    do not write flash, only verify (`check')
//   -S                    perform SRAM programming
//   -t                    just read the flash ID sequence

// Erase mode (only meaningful in default mode):
//   [default]             erase aligned chunks of 64kB in write mode
//                           This means that some data after the written data (or
//                           even before when -o is used) may be erased as well.
//   -b                    bulk erase entire flash before writing
//   -e <size in bytes>    erase flash as if we were writing that number of bytes
//   -n                    do not erase flash before writing
//   -p                    disable write protection before erasing or writing
//                           This can be useful if flash memory appears to be
//                           bricked and won't respond to erasing or programming.

/** Compose the command line for programming/reading the FPGA (iceprog)
  * @TODO implement readback
  */
case class IceProg(_bin: Option[Path] = None,
                   _device: String = "i:0x0403:0x6010",
                   _interface: String = "A",
                   _slow: Boolean = false,
                   _offset: Long = 0,
                   _targetname: String = "program",
                   passFile: Option[Path] = None,
                   logFile: Option[Path] = None,
                   phony: Option[String] = None,
                   prerequisite: mutable.MutableList[Makeable]= mutable.MutableList[Makeable]())
    extends MakeablePhony with MakeableLog with PassFail with Executable{

  /** Specify the path of the bin file
    *
    * @param bin the path of the bin file
    */
  def bin(bin: Path) = this.copy(_bin = Option(bin))

  /** Specify the devstring of the programmer
    * default: i:0x0403:0x6010 or i:0x0403:0x6014
    * d:<devicenode>               (e.g. d:002/005)
    * i:<vendor>:<product>         (e.g. i:0x0403:0x6010)
    * i:<vendor>:<product>:<index> (e.g. i:0x0403:0x6010:0)
    * s:<vendor>:<product>:<serial-string>
    *
    * @param devString the devString of the programmer
    */
  def device(devString: String) = this.copy(_device = devString)

  /** Specify the interface to use for programming
    * default: A
    *
    * @param interface the programmer interface name, chose between [ABCD]
    */
  def interface(interface: String) = this.copy(_interface = interface)

  /** Specify the memory offset were write the binary
    * default: 0
    *
    * @param offset the offset in Bytes
    */
  def offset(offset: Long) = this.copy(_offset = offset)

  /** program the spi at 50KHz */
  def slow = this.copy(_slow = true)

  /** Specify the target name
    * default: program
    *
    * @param name the target name
    */
  def targetname(name: String) = this.copy(_targetname = name)

  /** Change the output folder of all the target/output
    *
    * @param path the path where redirect all the outputs
    */
  def outputFolder(path: Path): IceProg ={
      val newBin  = if(_bin.nonEmpty) Some(path.resolve(_bin.get)) else None
      this.copy(_bin=newBin)
    }

  override def toString(): String = {
    val ret = new StringBuilder("iceprog ")
    ret.append(s"-d ${_device} ")
    ret.append(s"-o ${_offset} ")
    if (_slow) ret.append("-s ")
    ret.append(s"${_bin.get} ")
    ret.toString()
  }

  def phony(name: String): IceProg = this.copy(phony=Some(name))
  def log(file: Path = Paths.get(this.getClass.getSimpleName + ".log")): IceProg = this.copy(logFile=Some(file))
  def pass(file: Path = Paths.get("PASS")): IceProg = this.copy(passFile=Some(file))

  //makepart
  override def needs = List("bin")
  def phonyTarget = _targetname
  override def target = super.target
  override def makeComand: String =
    this.bin(getPrerequisiteFromExtension("bin")).toString
}

/** Compose the command line for replacin the bram data (icebram)
 * @TODO finish this and check
*/
case class IceBram(_hexFrom: Option[Path] = None,
                   _hexTo: Option[Path] = None,
                   _ascIn: Option[Path] = None,
                   _ascOut: Option[Path] = None,
                   _seed: String = "",
                   _random: Boolean = false,
                   _width: Long = 0,
                   _depth: Long = 0,
                   prerequisite: mutable.MutableList[Makeable]= mutable.MutableList[Makeable]())
    extends Makeable {

  /** Specify the path where the hex file use for syntesys reside
    * and specify were save the random hex file
    *
    * @param hex the path of the hex file
    */
  def hexFrom(hex: Path) = this.copy(_hexFrom = Some(hex))

  /** Specify the path of the new bram content
    *
    * @param hex the path of the hex file
    */
  def hexTo(hex: Path) = this.copy(_hexTo = Some(hex))

  /** Specify the path where to read the asc file
    *
    * @param asc the path of the asc file
    */
  def ascIn(asc: Path) = this.copy(_ascIn=Some(asc))

  /** Specify the path where to write the asc file
    * default: icebram.asc
    *
    * @param asc the path of the asc file
    */
    def ascOut(asc: Path) = this.copy(_ascIn=Some(asc))

  /** Specify the seed for the random data
    *
    * @param seed the seed string
    */
  def seed(seed: String) = this.copy(_random = true, _seed = seed)

  /** Populate the bram with random data
    * use this to generate the hex file used during synthesis, then
    * use the same file as <from_hexfile> later
    */
  def random = this.copy(_random = true)

  /** the size of the random hex */
  def size(width: Long, depth: Long) = this.copy(_width = width, _depth = depth)

  /** Change the output folder of all the target/output
    *
    * @param path the path where redirect all the outputs
    */
  def outputFolder(path: Path): IceBram ={
      val newAsc  = if(_ascOut.nonEmpty) Some(path.resolve(_ascOut.get)) else None
      this.copy(_ascOut=newAsc)
    }

  override def toString: String = {
    val ret = new StringBuilder("icebram ")
    if (_random) ret.append(s"-g ")
    if (_random && _seed.nonEmpty) {
      ret.append(s"-s ${_seed} ")
      ret.append(s"${_width} ${_depth} ")
      ret.append(s""" > ${_ascOut.getOrElse(Paths.get("icebram.from.hex"))} """)
    } else {
      ret.append(s"${_hexFrom} ${_hexTo} ")
      ret.append(s"""< ${_ascIn} > ${_ascOut.getOrElse(Paths.get("icebram.asc"))} """)
    }
    ret.toString
  }

  //make part
  override def needs = List("from.hex","hex")
  override def target = super.target ++ List(_ascOut.getOrElse(Paths.get("icebram.asc")))
  override def makeComand: String =
    if(_random) this.hexFrom(getPrerequisiteFromExtension("from.hex")).toString
    else this.ascIn(getPrerequisiteFromExtension("asc"))
             .hexFrom(getPrerequisiteFromExtension("from.hex"))
             .hexTo(getPrerequisiteFromExtension("hex")).toString
}

// case class IceCompr(_ascIn: Option[Path] = None,
//                     _ascOut: Option[Path] = None,
//                     prerequisite: mutable.MutableList[Makeable]= mutable.MutableList[Makeable]())
//     extends Makeable {

//   def ascIn(asc: Path) = this.copy(_ascIn = Some(asc))
//   def ascOut(asc: Path) = this.copy(_ascIn = Some(asc))
//   override def toString: String = {
//     assert(_ascIn.nonEmpty, "IceCompr: please define a ascIn file")
//     val ret = new StringBuilder("icecompr ")
//     ret.append(s"${_ascIn} ")
//     ret.append(s"${_ascOut} ")
//     ret.toString
//   }

//   /** Change the output folder of all the target/output
//     *
//     * @param path the path where redirect all the outputs
//     */
//   def outputFolder(path: Path): IceCompr ={
//     val newAsc  = if(_ascOut.nonEmpty) Some(path.resolve(_ascOut.get)) else None
//     this.copy(_ascOut=newAsc)
//   }

//   //make stuff
//   def target = List(_ascOut.getOrElse(Paths.get("icecompr.asc")))
//   override def needs = List("asc")
//   override def makeComand: String =
//     this.ascIn(getPrerequisiteFromExtension("asc")).toString
// }

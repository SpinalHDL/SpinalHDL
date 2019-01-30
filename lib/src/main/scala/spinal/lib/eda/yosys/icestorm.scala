
package spinal.lib.eda.yosys

// -v verbose (repeat to increase verbosity)



case class Icepack( asc: String,
                    bin: String,
                    _unpack: Boolean = false,
                    _deepsleep: Boolean = false,
                    _writeCram: Boolean = false,
                    _writeCramFill: Boolean = false,
                    _writeCramCheckerboard : Boolean = false,
                    _writeOnlyBram : Boolean = false,
                    _writeBank: Seq[Int] = Seq.empty[Int],
                    workDir: String = ".") extends Executable {

    override val logFile = "icepack.log"
    def unpack                 = this.copy(_unpack=true)
    def disableDeepSleep       = this.copy(_deepsleep=false)
    def writeCram              = this.copy(_writeCram=true)
    def writeCramFill          = this.copy(_writeCramFill=true)
    def writeCramCheckerboard  = this.copy(_writeCramCheckerboard=true)
    def writeOnlyBram          = this.copy(_writeOnlyBram=true)
    def writeBank(value: Int*) = this.copy(_writeBank=value)

    override def toString(): String = {
        val ret = new StringBuilder("icepack ")
        if(_unpack)                ret.append("-u ")  // -u unpack mode (implied when called as 'iceunpack')
        if(_deepsleep)             ret.append("-s ")  // -s disable final deep-sleep SPI flash command after bitstream is loaded
        if(_writeCram)             ret.append("-b ")  // -b write cram bitmap as netpbm file
        if(_writeCramFill)         ret.append("-f ")  // -f write cram bitmap (fill tiles) as netpbm file
        if(_writeCramCheckerboard) ret.append("-c ")  // -c  write cram bitmap (checkerboard) as netpbm file repeat to flip the selection of tiles
        if(_writeOnlyBram)         ret.append("-r ")  // -r write bram data, not cram, to the netpbm file
        if(_writeBank.nonEmpty) _writeBank.foreach(v => ret.append(s"-B${v} "))     // -B0, -B1, -B2, -B3 only include the specified bank in the netpbm file

        if(_unpack){
          ret.append(s"${bin} ")
          ret.append(s"${asc} ")
        } else {
          ret.append(s"${asc} ")
          ret.append(s"${bin} ")
        }
        ret.toString()
    }
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


case class Iceprog( bin: String,
                    _device: String = "i:0x0403:0x6010",
                    _interface: String = "A",
                    _slow: Boolean = false,
                    _offset: Long = 0,
                    workDir: String = ".") extends Executable {

    override val logFile = "iceprog.log"

    def device(devString: String)       = this.copy(_device=devString)
    def interface(interface: String)    = this.copy(_interface=interface)
    def offset(offset: Long)            = this.copy(_offset=offset)

    override def toString(): String = {
        val ret = new StringBuilder("iceprog ")

        ret.append(s"-d ${_device} ")//   -d <device string>    use the specified USB device [default: i:0x0403:0x6010 or i:0x0403:0x6014]
        //                           d:<devicenode>               (e.g. d:002/005)
        //                           i:<vendor>:<product>         (e.g. i:0x0403:0x6010)
        //                           i:<vendor>:<product>:<index> (e.g. i:0x0403:0x6010:0)
        //                           s:<vendor>:<product>:<serial-string>
        ret.append(s"-I ${_interface} ") //   -I [ABCD]             connect to the specified interface on the FTDI chip
        //                           [default: A]
        ret.append(s"-o ${_offset} ") //   -o <offset in bytes>  start address for read/write [default: 0]
        //                           (append 'k' to the argument for size in kilobytes,
        //                           or 'M' for size in megabytes)
        if(_slow) ret.append("-s ") //   -s                    slow SPI (50 kHz instead of 6 MHz)

        ret.append(s"${bin} ")

        ret.toString()
    }
}

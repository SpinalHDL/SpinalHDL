package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._
import scala.collection.Seq

/** Factory for [[spinal.lib.bus.wishbone.WishboneDecoder]] instances. */
object WishboneDecoder {
  /** Create an instance of a wishbone decoder/multiplexer.
  * @param config it will use for configuring all the input/output wishbone port
  * @param decodings it will use for configuring the partial address decoder
  * @return a [[spinal.lib.bus.wishbone.WishboneDecoder]] instance
  */
  def apply(config: WishboneConfig, decodings: Seq[AddressMapping]) = new WishboneDecoder(config, decodings)
  def apply(config: WishboneConfig, decodings: Seq[AddressMapping], byteAddressedDecodings: Boolean) = new WishboneDecoder(config, decodings, byteAddressedDecodings)

  /** Create an instance of WishboneDecoder, and autoconnect all input/outputs.
    * @param master connect the input to this master interface, the [[spinal.lib.bus.wishbone.Wishbone.config]] will be used for all input/output
    * @param slaves connect the output to the slaves, with the correct address
    * @return a [[spinal.lib.bus.wishbone.WishboneDecoder]] instance with all the input and output connected automatically
    */
  def apply(master: Wishbone, slaves: Seq[(Wishbone, AddressMapping)]): WishboneDecoder =
    apply(master, slaves, byteAddressedDecodings = false)

  /** Create an instance of WishboneDecoder, and autoconnect all input/outputs.
    * @param master connect the input to this master interface, the [[spinal.lib.bus.wishbone.Wishbone.config]] will be used for all input/output
    * @param slaves connect the output to the slaves, with the correct address
    * @param byteAddressedDecodings when true, the word address on the bus is converted to a byte address
    *                               before matching; decodings must then be specified in bytes
    * @return a [[spinal.lib.bus.wishbone.WishboneDecoder]] instance with all the input and output connected automatically
    */
  def apply(master: Wishbone, slaves: Seq[(Wishbone, AddressMapping)], byteAddressedDecodings: Boolean): WishboneDecoder = {
    val decoder = new WishboneDecoder(master.config, slaves.map(_._2), byteAddressedDecodings)
    decoder.io.input <> master
    (slaves.map(_._1), decoder.io.outputs).zipped.map(_ << _)
    decoder.setPartialName(master, "decoder")
  }
}

/** Create a wishbone decoder/demultiplexer
  * @param config it will use for configuring all the input/output wishbone port
  * @param decodings it will use for configuring the partial address decoder
  * @param byteAddressedDecodings when true, the word address on the bus is converted to a byte address
  *                               before matching; decodings must then be specified in bytes.
  *                               Defaults to false to preserve existing behaviour.
  */
class WishboneDecoder(config: WishboneConfig, decodings: Seq[AddressMapping], byteAddressedDecodings: Boolean = false) extends Component {
  val io = new Bundle {
    val input = slave(Wishbone(config))
    val outputs = Vec(master(Wishbone(config)), decodings.size)
  }

  // Permanently drive some slave input signal to save on logic usage.
  io.outputs.map { out =>
    out.STB       := io.input.STB
    out.DAT_MOSI  := io.input.DAT_MOSI
    out.WE        := io.input.WE
    out.ADR       := io.input.ADR
    Wishbone.driveWeak(io.input.SEL, out.SEL, null, false, false)
    Wishbone.driveWeak(io.input.LOCK, out.LOCK, null, false, false)
    Wishbone.driveWeak(io.input.CTI, out.CTI, null, false, false)
    Wishbone.driveWeak(io.input.TGD_MOSI, out.TGD_MOSI, null, false, false)
    Wishbone.driveWeak(io.input.TGA, out.TGA, null, false, false)
    Wishbone.driveWeak(io.input.TGC, out.TGC, null, false, false)
    Wishbone.driveWeak(io.input.BTE, out.BTE, null, false, false)
  }

  // Optionally convert the bus word address to a byte address so that AddressMapping
  // objects can be specified in bytes, consistent with WishboneSlaveFactory.
  val selectAddress = if (byteAddressedDecodings) io.input.byteAddress(AddressGranularity.WORD) else io.input.ADR

  // the selector will use the decodings list to select the right slave based on the address line
  val selector = Vec(decodings.map(_.hit(selectAddress) && io.input.CYC))
  val selectorIndex = OHToUInt(selector)

  // Generate the CYC signal for the selected slave
  (io.outputs.map(_.CYC), selector).zipped.foreach(_ := _)

  // Implementing the multiplexer logic, it takes the one Hot bit vector/bit array as input
  val selectedOutput = io.outputs(selectorIndex)

  io.input.ACK := selectedOutput.ACK
  io.input.DAT_MISO := selectedOutput.DAT_MISO

  if(config.useSTALL) io.input.STALL    := selectedOutput.STALL
  if(config.useERR)   io.input.ERR      := selectedOutput.ERR
  if(config.useRTY)   io.input.RTY      := selectedOutput.RTY
  if(config.useTGD)   io.input.TGD_MISO := selectedOutput.TGD_MISO
}

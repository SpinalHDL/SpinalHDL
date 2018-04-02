package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._

/** Factory for [[spinal.lib.bus.wishbone.WishboneDecoder]] instances. */
object WishboneDecoder{
  /** Create a istance of a wishbone decoder/multiplexer
  * @param config it will use for configuring all the input/output wishbone port
  * @param decodings it will use for configuring the partial address decoder
  * @return a WishboneDecoder instance
  */
  def apply(config: WishboneConfig, decodings: Seq[SizeMapping]) = new WishboneDecoder(config,decodings)

  /** Create an istance of WishboneDecoder, and autocconect all input/outputs
    * @param master connect the input to this master interface, the [[spinal.lib.bus.wishbone.Wishbone.config]] will be used for all input/output
    * @param slaves connect the ouput to the slaves, with the correct address
    * @return a WishboneDecoder instance with all the input and output connected automatically
    */
  def apply(master: Wishbone, slaves: Seq[(Wishbone, SizeMapping)]): WishboneDecoder = {
    val decoder = new WishboneDecoder(master.config, slaves.map(_._2))
    decoder.io.input <> master
    (slaves.map(_._1), decoder.io.outputs).zipped.map(_ <> _)
    decoder.setPartialName(master,"decoder")
  }
}

/** Create a wishbone decoder/demultiplexer
  * @param config it will use for configuring all the input/output wishbone port
  * @param decodings it will use for configuring the partial address decoder
  */
class WishboneDecoder(config : WishboneConfig, decodings : Seq[SizeMapping]) extends Component {
  val io = new Bundle {
    val input = slave(Wishbone(config))
    val outputs = Vec(master(Wishbone(config)),decodings.size)
  }

  io.outputs.map(_.clearAll)
  io.input.clearAll()

  for((slave, select) <- decodings.zipWithIndex){
    when(slave.hit(io.input.ADR) && io.input.CYC){
      io.outputs(select) <> io.input
    }
  }
}
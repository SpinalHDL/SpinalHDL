package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._

/** Factory for [[spinal.lib.bus.wishbone.WishboneArbiter]] instances. */
object WishboneArbiter{
  /** Create a wishbone Arbiter/multiplexer
    * the arbiter will not switch to other interfaces until the selected master/input CYC line goes to zero
    * @constructor create a WishboneArbiter instance
    * @param config it will use for configuring all the input/output wishbone port
    * @param inputCount the number of master interface
    * @return a instantiated [[spinal.lib.bus.wishbone.WishboneArbiter]] class
    */
  def apply(config : WishboneConfig, inputCount : Int) = new WishboneArbiter(config, inputCount)

  /** Create a wishbone Arbiter/multiplexer, use the slave config fot the multiplexer
    * @param masters a Seq of master interfaces
    * @param slave the slave interface, the [[spinal.lib.bus.wishbone.Wishbone.config]] will be used for all input/output
    * @return a instantiated [[spinal.lib.bus.wishbone.WishboneArbiter]] class
    */
  def apply(masters: Seq[Wishbone], slave: Wishbone): WishboneArbiter = {
    val arbiter = new WishboneArbiter(slave.config, masters.size)
    arbiter.io.output <> slave
    (arbiter.io.inputs, masters).zipped.map(_ <> _)
    arbiter.setPartialName(slave,"arbiter")
  }
}

/** Create a wishbone Arbiter/multiplexer
  * the arbiter will not switch to other interfaces until the selected master/input CYC line goes to zero
  * @constructor create a WishboneArbiter instance
  * @param config it will use for configuring all the input/output wishbone port
  * @param inputCount the number of master interface
  * @param priority wich interface is considerate first in the roundrobin algorithm
  */
class WishboneArbiter(config : WishboneConfig, inputCount : Int) extends Component{
  val io = new Bundle{
    val inputs = Vec(slave(Wishbone(config)), inputCount)
    val output = master(Wishbone(config))
  }

  //default some slave output flow control signal to false,
  //so we don't create involuntary latched logic
                      io.inputs.map(_.ACK := False)
  if(config.useSTALL) io.inputs.map(_.STALL := False)
  if(config.useERR)   io.inputs.map(_.ERR := False)
  if(config.useRTY)   io.inputs.map(_.RTY := False)

  //permanently connect DAT_MISO and TGD_MISO (if used) for save on logic usage
  /**@todo: in.TGD_MISO seams a typo.. if regression still checks up, change the regression test*/
  io.inputs.map{ in =>
    in.DAT_MISO := io.output.DAT_MISO
    Wishbone.driveWeak(io.output.TGD_MISO, in.TGD_MISO, null, false, false)
  }

  //implement a blocking round robin algorytm.
  //The channel will remain selected is either LOCK or CYC are equals to true
  //and pass to the next channel after the master clears CYC and LOCK
  //this output a One Hot encoded vector/bit array
  val requests =  if(config.useLOCK)  Vec(io.inputs.map(func => func.CYC && !func.LOCK))
                  else                Vec(io.inputs.map(_.CYC))
  val maskLock = Reg(Bits(inputCount bits)) init(1)

  val selector = RegNextWhen(OHMasking.roundRobin(requests.asBits, maskLock), !io.output.CYC) init(1)
  when(io.output.CYC && selector =/= 0){
    maskLock := selector
  }
  //Implementing the multiplexer logic, it thakes the one Hot bit vector/bit array as input
  MuxOH(selector, Vec(io.inputs.map(_.ACK))) := io.output.ACK
  if(config.useSTALL) MuxOH(selector, Vec(io.inputs.map(_.STALL)))  := io.output.STALL
  if(config.useERR)   MuxOH(selector, Vec(io.inputs.map(_.ERR)))    := io.output.ERR
  if(config.useRTY)   MuxOH(selector, Vec(io.inputs.map(_.RTY)))    := io.output.RTY

  io.output.CYC       := MuxOH(selector, Vec(io.inputs.map(_.CYC)))
  io.output.STB       := MuxOH(selector, Vec(io.inputs.map(_.STB)))
  io.output.WE        := MuxOH(selector, Vec(io.inputs.map(_.WE)))
  io.output.ADR       := MuxOH(selector, Vec(io.inputs.map(_.ADR)))
  io.output.DAT_MOSI  := MuxOH(selector, Vec(io.inputs.map(_.DAT_MOSI)))

  if(config.useSEL)   io.output.SEL       := MuxOH(selector, Vec(io.inputs.map(_.SEL)))
  if(config.useLOCK)  io.output.LOCK      := MuxOH(selector, Vec(io.inputs.map(_.LOCK)))
  if(config.useCTI)   io.output.CTI       := MuxOH(selector, Vec(io.inputs.map(_.CTI)))
  if(config.useTGD)   io.output.TGD_MOSI  := MuxOH(selector, Vec(io.inputs.map(_.TGD_MOSI)))
  if(config.useTGA)   io.output.TGA       := MuxOH(selector, Vec(io.inputs.map(_.TGA)))
  if(config.useTGC)   io.output.TGC       := MuxOH(selector, Vec(io.inputs.map(_.TGC)))
  if(config.useBTE)   io.output.BTE       := MuxOH(selector, Vec(io.inputs.map(_.BTE)))
}
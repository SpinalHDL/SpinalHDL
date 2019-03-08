package spinal.lib.eda.yosys

import spinal.core._

//  -l [ --log ] arg            log file, all log messages are written to this file regardless of -q
//  --debug                     debug output
//  -f [ --force ]              keep running after errors
//  --run arg                   python file to execute instead of default flow
//  --pre-pack arg              python file to run before packing
//  --pre-place arg             python file to run before placement
//  --pre-route arg             python file to run before routing
//  --post-route arg            python file to run after routing
//  --slack_redist_iter arg     number of iterations between slack redistribution
//  --cstrweight arg            placer weighting for relative constraint satisfaction
//  -V [ --version ]            show version
//  --test                      check architecture database integrity
//  --save arg                  project file to write
//  --load arg                  project file to read

trait NextPNR extends Executable{
  val _json: String
  val _freq: HertzNumber
  val _pack_only: Boolean
  val _verbose: Boolean
  val _quiet: Boolean
  val _seed: String
  val _randomize_seed: Boolean
  val _gui: Boolean
  val _no_tmdriv: Boolean

  val family: String = "generic"

  override def toString(): String = {
    val ret = new StringBuilder(s"nextpnr-${family} ")
                              ret.append(s"--json ${_json} ")                   // --json arg               JSON design file to ingest
    if(_freq.toDouble > 0 )   ret.append(s"--freq ${_freq.toDouble/1000000} ")  // --freq arg               set target frequency for design in MHz
    if(_pack_only)            ret.append("--pack-only ")                        // --pack-only              pack design only without placement or routing
    if(_verbose)              ret.append("--verbose ")                          // -v [ --verbose ]         verbose output
    if(_quiet)                ret.append("--quiet ")                            // -q [ --quiet ]           quiet mode, only errors and warnings displayed
    if(_seed.nonEmpty)        ret.append("--seed ${_seed} ")                    // --seed arg               seed value for random number generator
    else if(_randomize_seed)  ret.append("--randomize-seed ")                   // -r [ --randomize-seed ]  randomize seed value for random number generator
    if(_no_tmdriv)            ret.append("--no-tmdriv ")                        // --no-tmdriv              disable timing-driven placement
    if(_gui)                  ret.append("--gui ")                              // --gui                    start gui
    ret.toString
  }
}

object Ice40{
  val lp384 = "lp384"
  val lp1k  = "lp1k"
  val lp8k  = "lp8k"
  val hx1k  = "hx1k"
  val hx8k  = "hx8k"
  val up5k  = "up5k"
  object pack{
    val swg16tr  = "swg16tr"
    val uwg30    = "uwg30"
    val cm36     = "cm36"
    val cm49     = "cm49"
    val cm81     = "cm81"
    val cm81_4k  = "cm81:4k"
    val cm121_4k = "cm121:4k"
    val cm121    = "cm121"
    val cm225_4k = "cm225:4k"
    val cm225    = "cm225"
    val qn32     = "qn32"
    val sg48     = "sg48"
    val qn84     = "qn84"
    val cb81     = "cb81"
    val cb121    = "cb121"
    val cb132    = "cb132"
    val cb132_4k = "cb132:4k"
    val vq100    = "vq100"
    val tq144    = "tq144"
    val tq144_4k = "tq144:4k"
    val bg121_4k = "bg121:4k"
    val bg121    = "bg121"
    val ct256    = "ct257"
  }
}

//Architecture specific options:
//  --lp384                     set device type to iCE40LP384
//  --lp1k                      set device type to iCE40LP1K
//  --lp8k                      set device type to iCE40LP8K
//  --hx1k                      set device type to iCE40HX1K
//  --hx8k                      set device type to iCE40HX8K
//  --up5k                      set device type to iCE40UP5K
//  --read arg                  asc bitstream file to read

case class NextPNR_ice40( _json: String="",
                          _pcf: String="",
                          _asc: String="nextpnr_ice40.asc",
                          _no_tmdriv: Boolean = true,
                          _freq: HertzNumber = 0 Hz,
                          _pack_only: Boolean = false,
                          _verbose: Boolean = false,
                          _quiet: Boolean = true,
                          _seed: String = "",
                          _randomize_seed: Boolean = true,
                          _gui: Boolean = false,
                          _target : String = Ice40.hx1k,
                          _pack : String = Ice40.pack.ct256,
                          _promote_logic: Boolean = false,
                          _no_promote_globals : Boolean = false,
                          _opt_timing: Boolean = false,
                          _tmfuzz: Boolean = false,
                          _pcf_allow_unconstrained : Boolean = false,
                          workDir : String = ".") extends NextPNR with Makeable{
  override val family = "ice40"

  def openGui = this.copy(_gui=true)
  def targetFrequency(frequency: HertzNumber) = this.copy(_freq=frequency,_no_tmdriv=false)
  def seed(seed: String) = this.copy(_seed=seed,_randomize_seed=false)
  def json(j: String) = this.copy(_json=j)
  def asc(a: String) = this.copy(_asc=a)

  def setTarget(target: String, pack: String="") = this.copy(_target=target,_pack=pack)
  def withPCF(path: String, allowUncostrained: Boolean = false) = this.copy(_pcf=path,_pcf_allow_unconstrained = allowUncostrained)
  def workPath(path: String) = this.copy(workDir=path)

  override def toString(): String = {
    val ret = new StringBuilder(super.toString())
                                  ret.append(s"--pcf ${_pcf} ")             // --pcf arg                  PCF constraints file to ingest
                                  ret.append(s"--${target} ")
                                  ret.append(s"--asc ${_asc} ")             //  --asc arg                   asc bitstream file to write
    if(_pack.nonEmpty)            ret.append(s"--package ${_pack} ")        // --package arg              set device package
    if(_promote_logic)            ret.append( "--promote-logic ")           //  --promote-logic           enable promotion of 'logic' globals (in addition to clk/ce/sr by default)
    if(_no_promote_globals)       ret.append( "--no-promote-globals ")      // --no-promote-globals       disable all global promotion
    if(_opt_timing)               ret.append( "--opt-timing ")              // --opt-timing               run post-placement timing optimisation pass
    if(_tmfuzz)                   ret.append( "--tmfuzz ")                  // --tmfuzz                   run path delay estimate fuzzer
    if(_pcf_allow_unconstrained)  ret.append( "--pcf-allow-unconstrained ") // --pcf-allow-unconstrained  don't require PCF to constrain all IO
    ret.toString
  }

  //make stuff
  def target = List(_asc)
  override def makeComand: String =
    this.copy(/* _pcf = getPrerequisiteFromName(".*.pcf"), */ _json = getPrerequisiteFromName(".*.json")).toString
}

package spinal.lib.eda.yosys
import scala.collection.mutable._
import java.io.File
import java.io.PrintWriter
import java.nio.file.Paths

//import org.apache.commons.io.FileUtils
import spinal.core._
import scala.sys.process._

object YosysFlow{
    def model(name: String, mode: String, multiclock: Boolean=false) : Yosys ={
        val modelGen = new Yosys()
        //modelGen + read_sv(file,name)
        print(modelGen.commands)
        if(name == "base") modelGen.addCommand("memory_nordff") else modelGen.addCommand("memory_map")
        if(multiclock){
          modelGen.addCommand("clk2fflogic")
        } else {
          modelGen.addCommand("async2sync")
          modelGen.addCommand("chformal","-assume","-early")
        }
        if(mode == Mode.bmc || mode == Mode.prove) modelGen.addCommand("chformal","-live","-fair","-cover","-remove")
        if(mode == Mode.cover) modelGen.addCommand("chformal","-live","-fair","-remove")
        if(mode == Mode.live){
          modelGen.addCommand("chformal","-assert2assume")
          modelGen.addCommand("chformal","-cover","-remove")
        }
        modelGen.addCommand("opt_clean")
        modelGen.addCommand("setundef","-anyseq")
        modelGen.addCommand("opt","-keepdc","-fast")
        modelGen.addCommand("check")
        modelGen.addCommand("hierarchy","-simcheck")
        modelGen
      }
}

class YosysFlow(rtl: SpinalReport[_],workDir: String = "."){
    def formal(mode: String = Mode.bmc, multiclock: Boolean = false) = {
        val logs = new File(workDir,"logs")
        logs.mkdirs

        val opt = new Yosys(logs.getAbsolutePath())
        rtl.rtlSourcesPaths.foreach(c => opt.addCommand("read_verilog","-sv",new File(c).getAbsolutePath()))
        opt.addCommand("prep","-top",rtl.toplevelName)
        opt.append(YosysFlow.model(rtl.toplevelName,mode,multiclock))
        val smt2 = new File(logs.toString,rtl.toplevelName + ".smt2")
        opt.addCommand("write_smt2","-wires",smt2.getAbsolutePath())
        opt.run()

        val vcd = new File(workDir,"test.vcd")
        FormalCommand(smt2 = smt2.getAbsolutePath(),
                      top = rtl.toplevelName,
                      solver = Solver.z3,
                      mode = mode,
                      dumpVCD= vcd.getAbsolutePath(),
                      workDir=logs.getAbsolutePath()).run()
    }

    def synthesize(target: String) : String = {
      val logs = new File(workDir,"logs")
      logs.mkdirs
      val opt = new Yosys(logs.getAbsolutePath())
      rtl.rtlSourcesPaths.foreach(c => opt.addCommand("read_verilog",new File(c).getAbsolutePath()))
      val json = new File(workDir,rtl.toplevelName+".json")
      opt.addCommand("synth_"+ target,"-top",rtl.toplevelName,"-json",json.getAbsolutePath())
      opt.run()
      json.getAbsolutePath()
    }

    def nextpnr_ice40(target: String,pack: String, jsonPath : String = "", pcfPath : String): String = {
      val logs = new File(workDir,"logs")
      val json = new File(if(jsonPath.isEmpty) synthesize("ice40") else jsonPath)
      val pcf = new File(pcfPath)
      val asc = new File(workDir,rtl.toplevelName+".asc")
      NextPNR_ice40(json = json.getAbsolutePath(),
                    pcf = pcf.getAbsolutePath(),
                    asc = asc.getAbsolutePath(),
                    no_tmdriv = true,
                    freq = 0 Hz,
                    pack_only = false,
                    verbose = true,
                    quiet = false,
                    seed = "",
                    randomize_seed = true,
                    gui = false,
                    target = target,
                    pack = pack,
                    promote_logic = false,
                    no_promote_globals = false,
                    opt_timing = false,
                    tmfuzz = false,
                    pcf_allow_unconstrained = false,
                    workDir  = logs.getAbsolutePath()).run()
      asc.getAbsolutePath()
    }
}
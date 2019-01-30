package spinal.lib.eda.yosys

import java.io.File
import spinal.core._
import scala.collection._

object YosysFlow{
    def model(name: String, mode: String, multiclock: Boolean=false) : Yosys ={
        val modelGen = Yosys()
        //modelGen + read_sv(file,name)
        //@TODO if(name == "base") modelGen.addCommand("memory_nordff") else modelGen.addCommand("memory_map")
        modelGen.addCommand("memory_nordff")
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
    def formal(mode: String = Mode.bmc, solver: String = Solver.abc,multiclock: Boolean = false, step: Int = 20) : FormalCommand = {
        val wkr = new File(workDir)
        wkr.mkdirs

        val opt = Yosys(rtl.toplevelName+".ys", wkr.getAbsolutePath())
        rtl.rtlSourcesPaths.foreach(c => opt.addCommand("read_verilog","-sv",new File(c).getAbsolutePath()))
        opt.addCommand("prep","-top",rtl.toplevelName)
        opt.append(YosysFlow.model(rtl.toplevelName,mode,multiclock))
        val smt2 = new File(wkr.toString,rtl.toplevelName + ".smt2")
        opt.addCommand("write_smt2","-wires",smt2.getAbsolutePath())
        opt.run()

        val vcd = new File(workDir,rtl.toplevelName+".vcd")
        FormalCommand(smt2 = smt2.getAbsolutePath(),
                      top = rtl.toplevelName,
                      solver = solver,
                      mode = mode,
                      step = step,
                      dumpVCD= vcd.getAbsolutePath(),
                      workDir= wkr.getAbsolutePath())
    }

    def synthesize(target: String) : String = {
      val wkr = new File(workDir)
      wkr.mkdirs
      val opt = Yosys(rtl.toplevelName+".ys", wkr.getAbsolutePath())
      // rtl.rtlSourcesPaths.foreach(c => opt.addCommand("read_verilog",new File(c).getAbsolutePath()))
      rtl.rtlSourcesPaths.foreach(c => opt.addInputFile(new File(c).getAbsolutePath(),"verilog"))

      val json = new File(workDir,rtl.toplevelName+".json")
      opt.addCommand("synth_"+ target,"-top",rtl.toplevelName)
      opt.addOutputFile(json.getAbsolutePath(),"json")
      opt.run()
      json.getAbsolutePath()
    }

    def nextpnr_ice40(jsonPath : String = "", pcfPath : String = ""): NextPNR_ice40 = {
      val wrk = new File(workDir)
      val json = new File(if(jsonPath.isEmpty) synthesize("ice40") else jsonPath)
      val pcf = new File(workDir,pcfPath)
      val asc = new File(workDir,rtl.toplevelName+".asc")
      NextPNR_ice40(json = json.getAbsolutePath(),
                    pcf = pcf.getAbsolutePath(),
                    asc = asc.getAbsolutePath(),
                    workDir  = wrk.getAbsolutePath())
    }
}

class YosysReport{
  val modelsPaths = mutable.LinkedHashSet[(String,String)]()
  val binaryPaths = mutable.LinkedHashSet[(String,String)]()

}

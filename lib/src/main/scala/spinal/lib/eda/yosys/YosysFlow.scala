package spinal.lib.eda.yosys

import java.io.File
import spinal.core._

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
        val logs = new File(workDir,"logs")
        logs.mkdirs

        val opt = Yosys()
        rtl.rtlSourcesPaths.foreach(c => opt.addCommand("read_verilog","-sv",new File(c).getAbsolutePath()))
        opt.addCommand("prep","-top",rtl.toplevelName)
        opt.append(YosysFlow.model(rtl.toplevelName,mode,multiclock))
        val smt2 = new File(logs.toString,rtl.toplevelName + ".smt2")
        opt.addCommand("write_smt2","-wires",smt2.getAbsolutePath())
        opt.run(workDir=logs.getAbsolutePath())

        val vcd = new File(workDir,rtl.toplevelName+".vcd")
        FormalCommand(smt2 = smt2.getAbsolutePath(),
                      top = rtl.toplevelName,
                      solver = solver,
                      mode = mode,
                      step = step,
                      dumpVCD= vcd.getAbsolutePath(),
                      workDir=logs.getAbsolutePath())
    }

    def synthesize(target: String) : String = {
      val logs = new File(workDir,"logs")
      logs.mkdirs
      val opt = Yosys()
      rtl.rtlSourcesPaths.foreach(c => opt.addCommand("read_verilog",new File(c).getAbsolutePath()))
      val json = new File(workDir,rtl.toplevelName+".json")
      opt.addCommand("synth_"+ target,"-top",rtl.toplevelName,"-json",json.getAbsolutePath())
      opt.run(workDir=logs.getAbsolutePath())
      json.getAbsolutePath()
    }

    def nextpnr_ice40(jsonPath : String = "", pcfPath : String = ""): NextPNR_ice40 = {
      val logs = new File(workDir,"logs")
      val json = new File(if(jsonPath.isEmpty) synthesize("ice40") else jsonPath)
      val pcf = new File(workDir,pcfPath)
      val asc = new File(workDir,rtl.toplevelName+".asc")
      NextPNR_ice40(json = json.getAbsolutePath(),
                    pcf = pcf.getAbsolutePath(),
                    asc = asc.getAbsolutePath(),
                    workDir  = logs.getAbsolutePath())
    }
}

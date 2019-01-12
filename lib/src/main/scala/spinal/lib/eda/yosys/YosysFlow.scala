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
}
package spinal.core

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class PhaseVerilog(pc : PhaseContext) extends PhaseMisc with VerilogBase {
  import pc._

  var outFile: java.io.FileWriter = null

  override def impl(pc: PhaseContext): Unit = {
    outFile = new java.io.FileWriter(pc.config.targetDirectory + "/" +  (if(pc.config.netlistFileName == null)(topLevel.definitionName + ".v") else pc.config.netlistFileName))
    outFile.write(VhdlVerilogBase.getHeader("//",topLevel))
    if(pc.config.dumpWave != null) {
      outFile.write("`timescale 1ns/1ps ")
    }
    emitEnumPackage(outFile)
    for (c <- sortedComponents) {
      if (!c.isInBlackBoxTree) {
        SpinalProgress(s"${"  " * (1 + c.level)}emit ${c.definitionName}")
        compile(c)
      }
    }
    outFile.flush();
    outFile.close();
  }

  val allocateAlgoIncrementaleBase = globalData.allocateAlgoIncrementale()
  def compile(component: Component): Unit = {
    val componentBuilderVerilog = new ComponentEmiterVerilog(component, this, allocateAlgoIncrementaleBase, config.mergeAsyncProcess, config.asyncResetCombSensitivity, globalData.anonymSignalPrefix, emitedComponentRef)
    if(component.parentScope == null && pc.config.dumpWave != null) {
      componentBuilderVerilog.logics ++=
        s"""
  initial begin
    $$dumpfile("${pc.config.dumpWave.vcdPath}");
    $$dumpvars(${pc.config.dumpWave.depth}, ${component.definitionName});
  end
"""
    }
    val trace = componentBuilderVerilog.getTrace()
    val oldComponent = emitedComponent.getOrElse(trace, null)
    val text = if (oldComponent == null) {
      emitedComponent += (trace -> component)
      componentBuilderVerilog.result
    } else {
      emitedComponentRef.put(component, oldComponent)
      s"\n//${component.definitionName} remplaced by ${oldComponent.definitionName}\n\n"
    }

    outFile.write(text)
  }




  val emitedComponent = mutable.Map[ComponentEmiterTrace, Component]()
  val emitedComponentRef = new java.util.concurrent.ConcurrentHashMap[Component,Component]()




  def emitEnumPackage(out: java.io.FileWriter): Unit = {
    val ret = new StringBuilder();


    ret ++= "\n"
    for ((enumDef, encodings) <- enums) {
      val enumName = enumDef.getName()
      for (encoding <- encodings) {
        val encodingName = encoding.getName()
        val bitCount = encoding.getWidth(enumDef)
        val vhdlEnumType = emitEnumType(enumDef, encoding,"")
        ret ++= s"`define $vhdlEnumType [${bitCount - 1}:0]\n"
        for (element <- enumDef.elements) {
          ret ++= s"`define ${emitEnumLiteral(element, encoding,"")} ${idToBits(element, encoding)}\n"
        }
        ret ++= "\n"
      }
    }

    def idToBits[T <: SpinalEnum](enum: SpinalEnumElement[T], encoding: SpinalEnumEncoding): String = {
      val str = encoding.getValue(enum).toString(2)
      val length = encoding.getWidth(enum.spinalEnum)
      length.toString + "'b" + ("0" * (length - str.length)) + str
    }


    out.write(ret.result())
  }


  def emitFunctions(component: Component, ret: StringBuilder): Unit = {
    val alreadyEmitted = mutable.Set[String]()
    component.dslBody.walkStatements(s => s.walkExpression {
      case node: CastEnumToEnum => {
        val encodingSrc = node.input.getEncoding
        val enumDef = node.getDefinition
        val encodingDst = node.getEncoding
        val fName = getReEncodingFuntion(enumDef, encodingSrc, encodingDst)
        if (!alreadyEmitted.contains(fName)) {
          alreadyEmitted += fName
          ret ++= s"  function $fName(${emitEnumType(enumDef, encodingSrc)} that);\n"
          ret ++= "  begin\n"
          ret ++= "    case(that) \n"
          for (e <- enumDef.elements) {
            ret ++= s"      ${emitEnumLiteral(e, encodingSrc)} : $fName =  ${emitEnumLiteral(e, encodingDst)};\n"
          }
          ret ++= s"      default : $fName =  ${emitEnumLiteral(enumDef.elements.head, encodingDst)};\n"
          ret ++= "    endcase\n"
          ret ++= "  end\n"
          ret ++= "  endfunction\n\n"
        }
      }
      case _ =>
    })
  }
}

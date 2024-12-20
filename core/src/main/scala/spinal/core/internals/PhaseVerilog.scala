/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.core.internals

import spinal.core._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}


class PhaseVerilog(pc: PhaseContext, report: SpinalReport[_]) extends PhaseMisc with VerilogBase {
  import pc._
  globalPrefix = pc.config.globalPrefix

  var outFile: java.io.FileWriter = null
  def rtlName = (if(pc.config.netlistFileName == null)(topLevel.definitionName + (if(pc.config.isSystemVerilog) ".sv" else ".v")) else pc.config.netlistFileName)
  def targetPath = pc.config.targetDirectory + "/" + rtlName

  override def impl(pc: PhaseContext): Unit = {

    report.toplevelName = pc.topLevel.definitionName
    if (!pc.config.oneFilePerComponent) {
      report.generatedSourcesPaths += targetPath
      outFile = new java.io.FileWriter(targetPath)
      outFile.write(VhdlVerilogBase.getHeader("//", pc.config.rtlHeader, topLevel, config.headerWithDate, config.headerWithRepoHash))

      if(pc.config.withTimescale) outFile.write("`timescale 1ns/1ps")

      emitEnumPackage(outFile)

      if(!svInterface.isEmpty) {
        outFile.write("\n")
        for ((name, interface) <- svInterface) {
          //for (interfaceString <- interface) {
            outFile.write(interface.result())
          //}
        }
      }

      val componentsText = ArrayBuffer[() => String]()
      for (c <- sortedComponents) {
        if (!c.isInBlackBoxTree) {
          componentsText += compile(c)
        }
      }

      for(e <- componentsText.reverse){
        outFile.write(e())
      }

      val bbImplStrings = mutable.HashSet[String]()
      sortedComponents.foreach{
        case bb : BlackBox if bb.impl != null => {
          val str = bb.impl.getVerilog()
          if(!bbImplStrings.contains(str)) {
            bbImplStrings += str
            outFile.write("\n")
            outFile.write(str)
            outFile.write("\n")
          }
        }
        case _ =>
      }

      outFile.flush()
      outFile.close()
    }
    else {
      val fileList: mutable.LinkedHashSet[String] = new mutable.LinkedHashSet()
      // dump Enum define to define.v instead attach that on every .v file
      if(enums.nonEmpty){
        val defineFileName = pc.config.targetDirectory + "/enumdefine" + (if(pc.config.isSystemVerilog) ".sv" else ".v")
        val defineFile = new java.io.FileWriter(defineFileName)
        emitEnumPackage(defineFile)
        defineFile.flush()
        defineFile.close()
        fileList += defineFileName
      }

      // dump Interface define to (interface.definitionName).v
      if(!svInterface.isEmpty) {
        for ((name, interface) <- svInterface) {
          val ifFileName = pc.config.targetDirectory + "/" + name + (if(pc.config.isSystemVerilog) ".sv" else ".v")
          val ifFile = new java.io.FileWriter(ifFileName)
          ifFile.write("\n")
          //for (interfaceString <- interface.interfaceStringSet) {
            ifFile.write(interface.result())
          //}
          ifFile.flush()
          ifFile.close()
          fileList += ifFileName
        }
      }

      val bbImplStrings = mutable.HashSet[String]()
      for (c <- sortedComponents) {
        val moduleContent = compile(c)()
        val targetFilePath = pc.config.targetDirectory + "/" +  (if(pc.config.netlistFileName == null)(c.definitionName + (if(pc.config.isSystemVerilog) ".sv" else ".v")) else pc.config.netlistFileName)
        report.generatedSourcesPaths += targetFilePath

        if (!moduleContent.contains("replaced by")) {
          if (!c.isInBlackBoxTree) {
            outFile = new java.io.FileWriter(targetFilePath)
            outFile.write(VhdlVerilogBase.getHeader("//", pc.config.rtlHeader, c, config.headerWithDate, config.headerWithRepoHash))
            if(pc.config.withTimescale) outFile.write("`timescale 1ns/1ps ")
//            emitEnumPackage(outFile)
            outFile.write(moduleContent)
            outFile.flush()
            outFile.close()
            fileList += targetFilePath
          }
          c match {
            case bb: BlackBox =>
              fileList ++= bb.listRTLPath
              if (bb.impl != null) {
                val str = bb.impl.getVerilog()
                if(!bbImplStrings.contains(str)) {
                  outFile = new java.io.FileWriter(targetFilePath)
                  outFile.write(VhdlVerilogBase.getHeader("//", pc.config.rtlHeader, c, config.headerWithDate, config.headerWithRepoHash))
                  if(pc.config.withTimescale) outFile.write("`timescale 1ns/1ps ")
                  outFile.write(str)
                  outFile.flush()
                  outFile.close()
                  fileList += targetFilePath
                  bbImplStrings += str
                }
              }
            case _ =>
          }
        }
      }

      if(pc.config.printFilelist){
        val fileListFile = new java.io.FileWriter(pc.config.targetDirectory + "/" + topLevel.definitionName + ".lst")
        fileList.foreach(file => fileListFile.write(file.replace("//", "/") + "\n"))
        fileListFile.flush()
        fileListFile.close()
      }
    }
  }

  val allocateAlgoIncrementaleBase = globalData.allocateAlgoIncrementale()
  val usedDefinitionNames = mutable.HashSet[String]()


  val romCache = mutable.HashMap[String, String]()
  def compile(component: Component): () => String = {
    val componentBuilderVerilog = new ComponentEmitterVerilog(
      c                           = component,
      systemVerilog               = pc.config.isSystemVerilog,
      verilogBase                 = this,
      algoIdIncrementalBase       = allocateAlgoIncrementaleBase,
      mergeAsyncProcess           = config.mergeAsyncProcess,
      asyncResetCombSensitivity   = config.asyncResetCombSensitivity,
      anonymSignalPrefix          = if(pc.config.anonymSignalUniqueness) globalData.anonymSignalPrefix + "_" + component.definitionName else globalData.anonymSignalPrefix,
      nativeRom                   = config.inlineRom,
      nativeRomFilePrefix         = rtlName,
      caseRom                     = config.caseRom,
      emitedComponentRef          = emitedComponentRef,
      emitedRtlSourcesPath        = report.generatedSourcesPaths,
      spinalConfig                = pc.config,
      pc                          = pc,
      romCache                    = romCache
    )

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

    if (oldComponent == null || component.definitionNameNoMerge && component.definitionName != oldComponent.definitionName) {
      assert(!usedDefinitionNames.contains(component.definitionName) || component.isInBlackBoxTree, s"Component '${component}' with definition name '${component.definitionName}' was already used once for a different layout\n${component.getScalaLocationLong}")
      usedDefinitionNames += component.definitionName
      emitedComponent += (trace -> component)
      () => componentBuilderVerilog.result
    } else {
      emitedComponentRef.put(component, oldComponent)
      val originalName = component.definitionName
      component match{
        case x: BlackBox =>
        case _ => component.definitionName = oldComponent.definitionName
      }
      () => s"\n//${originalName} replaced by ${oldComponent.definitionName}\n"
    }
  }

  val emitedComponent    = mutable.Map[ComponentEmitterTrace, Component]()
  val emitedComponentRef = new java.util.concurrent.ConcurrentHashMap[Component,Component]()

  def emitEnumPackage(out: java.io.FileWriter): Unit = {
    val ret = new StringBuilder()

    ret ++= "\n"
    for ((enumDef, encodings) <- enums) {
      val enumName = enumDef.getName()
      for (encoding <- encodings) {
        val encodingName = encoding.getName()
        val bitCount     = encoding.getWidth(enumDef)
        val vhdlEnumType = emitEnumType(enumDef, encoding, "")

//        ret ++= s"`define $vhdlEnumType [${bitCount - 1}:0]\n"
        if(enumDef.isGlobalEnable) {
          for (element <- enumDef.elements) {
            ret ++= s"`define ${emitEnumLiteral(element, encoding, "")} ${idToBits(element, encoding)}\n"
          }

          ret ++= "\n"
        }
      }
    }

    def idToBits[T <: SpinalEnum](senum: SpinalEnumElement[T], encoding: SpinalEnumEncoding): String = {
      val str    = encoding.getValue(senum).toString(2)
      val length = encoding.getWidth(senum.spinalEnum)
      length.toString + "'b" + ("0" * (length - str.length)) + str
    }

    out.write(ret.result())
  }


  def emitFunctions(component: Component, ret: StringBuilder): Unit = {
    val alreadyEmitted = mutable.Set[String]()
    component.dslBody.walkStatements(s => s.walkExpression {
      case node: CastEnumToEnum =>
        val encodingSrc = node.input.getEncoding
        val enumDef     = node.getDefinition
        val encodingDst = node.getEncoding
        val fName       = getReEncodingFuntion(enumDef, encodingSrc, encodingDst)

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
      case _ =>
    })
  }

  
}

class PhaseInterface(pc: PhaseContext) extends PhaseNetlist{
  sealed trait CmpResultKind
  object CmpResultKind {
    case object Same extends CmpResultKind
    case object Diff extends CmpResultKind
    case object Other extends CmpResultKind
  }
  def doCompare(
    nodeData: Data,
    otherNodeData: Data,
  ): CmpResultKind = {
    //--------
    nodeData match {
      case nodeIntf: Interface => {
        otherNodeData match {
          //case otherBt: BaseType => {
          //  return false
          //}
          case otherIntf: Interface => {
            if (
              emitInterface(nodeIntf, false).result()
              == emitInterface(otherIntf, false).result()
            ) {
              if (nodeIntf.elementsCache != null && otherIntf.elementsCache != null) {
                if (nodeIntf.elementsCache.size == otherIntf.elementsCache.size) {
                  for ((nodeElemName, nodeElem) <- nodeIntf.elementsCache.view) {
                    otherIntf.elementsCache.find{otherElem => {
                      otherElem._1 == nodeElemName
                    }} match {
                      case Some((otherElemName, otherElem)) => {
                        if (doCompare(
                          nodeData=nodeElem,
                          otherNodeData=otherElem,
                        ) == CmpResultKind.Diff) {
                          return CmpResultKind.Diff
                        }
                      }
                      case None => {
                        println(
                          s"eek! couldn't find this nodeElemName:${nodeElemName}"
                        )
                        assert(false)
                        //return CmpKind.Other
                        return null
                        //return false
                      }
                    }
                  }
                  return CmpResultKind.Same
                  //return true
                } else {
                  //return false
                  return CmpResultKind.Diff
                }
              } else {
                //return true
                return CmpResultKind.Same
              }
            } else {
              //return false
              return CmpResultKind.Diff
            }
          }
          case _ => {
            //return false
            //return CmpResultKind.Other
            return CmpResultKind.Diff
          }
        }
      }
      // TODO: support non-`Interface` `Bundle`s
      //case nodeBndl: Bundle => {
      //  otherNodeData match {
      //    case otherBndl: Bundle => {
      //    }
      //    case _ => {
      //    }
      //  }
      //}
      case nodeVec: Vec[_] => {
        otherNodeData match {
          case otherVec: Vec[_] => {
            if (nodeVec.size == otherVec.size) {
              for (vecIdx <- 0 until nodeVec.size) {
                val cmpResult = doCompare(
                  nodeData=nodeVec(vecIdx),
                  otherNodeData=otherVec(vecIdx),
                  //atTop=false,
                  //parentsAreVecs=true,
                )
                if (cmpResult != CmpResultKind.Same) {
                  //return false
                  return CmpResultKind.Diff
                }
              }
              //return true
              return CmpResultKind.Same
            } else {
              //return false
              return CmpResultKind.Diff
            }
          }
          case _ => {
            return CmpResultKind.Diff
          }
        }
      }
      case _ => {
        return CmpResultKind.Other
      }
    }
    assert(false)
    return null
  }
  def emitInterface(interface: Interface, convertIntfVec: Boolean=true): StringBuilder = {
    import pc._
    var ret = new StringBuilder()
    val theme = new Tab2 //TODO add into SpinalConfig
    val generic = if(interface.genericElements.isEmpty) ""
      else
        "#(\n" + interface.genericElements.map{case (name, useless, default) =>
          if(default == null)
            s"${theme.porttab}parameter ${name},\n"
          else
            s"${theme.porttab}parameter ${name} = ${default},\n"
        }.reduce(_ + _).stripSuffix(",\n") + "\n) "
    ret ++= s"interface ${interface.definitionName} ${generic}() ;\n\n"
    val sizeZero = mutable.HashSet[String]()
    def genBase[T <: Data](ret: StringBuilder, name: String, name1: String, elem: T, subIntfVecSize: Int): Unit = {
      elem match {
        case _: Bool => {
          val size = ""
          ret ++= f"${theme.porttab}logic  ${size}%-8s ${name}_${name1} ;\n"
        }
        case node: BitVector => {
          val size = interface.widthGeneric.get(node) match {
            case Some(x) => s"[${x}-1:0]"
            case None => if (node.getWidth > 0)
              s"[${node.getWidth - 1}:0]"
            else {
              globalData.nodeAreInferringWidth = false
              val width = node.getWidth
              globalData.nodeAreInferringWidth = true
              s"[${width - 1}:0]"
            }
          }
          if (size != "[-1:0]")
            ret ++= f"${theme.porttab}logic  ${size}%-8s ${name}_${name1} ;\n"
          else
            sizeZero.add(name)
        }
        case x => {
          genSig(
            ret,
            if (subIntfVecSize == 0 || !convertIntfVec) (
              s"${name}_${name1}"
            ) else (
              s"${name}[${subIntfVecSize}]"
            ),
            x,
          )
        }
      }
    }
    def genSig[T <: Data](ret: StringBuilder, name: String, elem: T): Unit = {
      elem match {
        case node: Interface if !node.thisIsNotSVIF => {

          val genericFlat = node.genericElements

          val t = if (genericFlat.nonEmpty) {
            val ret = genericFlat.map{ e =>
              interface.IFGeneric.get((node, e._1)) match {
                case Some(value) => e._1 -> value
                case None => {
                  e match {
                    //TODO:case (name: String, bt: BaseType, _)      => name -> s"${emitExpression(bt.getTag(classOf[GenericValue]).get.e)}"
                    case (name: String, rs: VerilogValues, _) => name -> s"${rs.v}"
                    case (name: String, s: String, _)         => name -> s"""\"$s\""""
                    case (name: String, i: Int, _)            => name -> s"$i"
                    case (name: String, d: Double, _)         => name -> s"$d"
                    case (name: String, b: Boolean, _)        => name -> s"${if(b) "1'b1" else "1'b0"}"
                    case (name: String, b: BigInt, _)         => name -> s"${b.toString(16).size*4}'h${b.toString(16)}"
                    case _                                 => SpinalError(s"The generic type ${"\""}${e._1} - ${e._2}${"\""} of the interface ${"\""}${node.definitionName}${"\""} is not supported in Verilog")
                  }
                }
              }
            }
            val namelens = ret.map(_._1.size).max
            val exprlens = ret.map(_._2.size).max
            val params   = ret.map(t =>  s"    .%-${namelens}s (%-${exprlens}s )".format(t._1, t._2))
            s"""${node.definitionName} #(
               |${params.mkString(",\n")}
               |  )""".stripMargin
          } else f"${node.definitionName}%-15s"
          val  cl = if(genericFlat.nonEmpty) "\n" else ""
          ret ++= f"${theme.porttab}${t} ${name}();\n${cl}"//TODO:parameter
        }
        case nodes: Bundle => {
          for ((name1, node) <- nodes.elementsCache) {
            genBase(ret, name, name1, node, 0)
          }
        }
        case nodes: Vec[_] => {
          var haveAllSameIntf: Boolean = convertIntfVec
          if (haveAllSameIntf) {
            for (idx <- 0 until nodes.size) {
              println(
                s"checking this one: "
                + s"nodes(${idx}) ${nodes(idx).getName()}"
              )
              if (idx > 0) {
                doCompare(
                  nodeData=nodes(idx),
                  otherNodeData=nodes(idx - 1),
                ) match {  //!= CmpResultKind.Same
                  case CmpResultKind.Same => {
                  }
                  case _ => {
                    haveAllSameIntf = false
                  }
                }
              }
            }
          }
          if (haveAllSameIntf) {
            genBase(ret, name, "0", nodes(0), nodes.size)
          } else {
            for ((node, idx) <- nodes.zipWithIndex) {
              genBase(ret, name, idx.toString, node, 0)
            }
          }
        }
        case _ => {
          val size = elem match {
            case _: Bool => ""
            case node: BitVector => interface.widthGeneric.get(node) match {
              case Some(x) => s"[${x}-1:0]"
              case None => if(node.getWidth > 0)
                s"[${node.getWidth - 1}:0]"
              else {
                globalData.nodeAreInferringWidth = false
                val width = node.getWidth
                globalData.nodeAreInferringWidth = true
                s"[${width - 1}:0]"
              }
            }
            case x => LocatedPendingError(s"The SystemVerilog interface feature is still an experimental feature. In interface, ${x} is not supported yet")
          }
          if(size != "[-1:0]")
            ret ++= f"${theme.porttab}logic  ${size}%-8s ${name} ;\n"
          else
            sizeZero.add(name)
        }
      }
    }
    for ((name, elem) <- interface.elementsCache) {
      genSig(ret, name, elem)
    }
    ret ++= "\n"
    if(pc.config.svInterfaceIncludeModport && !interface.thisIsNotSVModport) {
      interface.allModPort
        .foreach{case x =>
          var modportString = new StringBuilder()
          modportString ++= s"${theme.porttab}modport ${x} (\n"

          val toplevel = globalData.toplevel
          val phase = globalData.phaseContext.topLevel
          globalData.toplevel = null
          globalData.phaseContext.topLevel = null
          val c = new Component {
            val y = interface.clone().asInstanceOf[interface.type]
            y.callModPort(x)
          }
          globalData.toplevel = toplevel
          globalData.phaseContext.topLevel = phase

          def genModportSig[T <: Data](modportString: StringBuilder, name: String, elem: T): Unit = {
            elem match {
              case elem: Interface if !elem.thisIsNotSVIF => {
                //TODO:check more than one modport has same `in` `out` direction
                val modport = if(elem.checkModport().isEmpty) {
                  LocatedPendingError(s"no suitable modport found for ${elem}")
                  ""
                } else {
                  elem.checkModport().head
                }
                modportString ++= f"${theme.porttab}${theme.porttab}.${name}(${name}.${modport}),\n"
              }
              case elem: Bundle => {
                for((name1, node) <- elem.elementsCache) {
                  genModportSig(modportString, s"${name}_${name1}", node)
                }
              }
              case elem: Vec[_] => {
                for((node, idx) <- elem.zipWithIndex) {
                  genModportSig(modportString, s"${name}_${idx}", node)
                }
              }
              case elem => {
                val dir = elem.dir match {
                  case `in`    => "input "
                  case `out`   => "output"
                  case `inout` => "inout "
                  case _       => throw new Exception(s"Unknown direction in interface ${interface}: ${elem}"); ""
                }
                if(!sizeZero.contains(name))
                  modportString ++= f"${theme.porttab}${theme.porttab}${dir}%-15s ${name},\n"
              }
            }
          }
          for ((name, elem) <- c.y.elementsCache) {
            genModportSig(modportString, name, elem)
          }
          ret ++= modportString.toString().stripSuffix(",\n") + "\n" + s"${theme.porttab});\n\n"
        }
      }
    ret ++= "endinterface\n\n"
    ret
  }

  override def impl(pc: PhaseContext): Unit = {
    import pc._

    //locate root interface name
    val allocated = mutable.HashSet[Data]()
    //rename myif_a to myif.a
    def mkNewName(name: String, count: Int) = (
      Interface.mkNewName(name=name, count=count)
    )
    walkDeclarations {
      case node: BaseType if(node.hasTag(IsInterface)) => {
        def doAllocateName(
          interface: Data,
          rootIFList: List[Interface],
          someName: String,
          count: Int,
        ): Option[String] = {
          val newName = (
            mkNewName(s"zzz_Interface_${someName}", count)
          )
          if(
            interface.IFparent == null
            || rootIFList.size == 0
          ) {
            //interface.setName(interface.component.localNamingScope.allocateName(
            //  s"zzz_Interface_${someName}"
            //))
            //(interface.component.localNamingScope.haveName(newName)._1, newName)

            // I believe this will be handled with the `rootIF()` stuff below... right?
            //None
            val myHaveName = node.component.localNamingScope.haveName(newName)
            if (myHaveName._1) {
              doAllocateName(
                interface=interface,
                rootIFList=rootIFList,
                someName=someName,
                //doFinal=doFinal,
                //firstIter=false,
                count=count + 1
              )
            } else {
              //None
              Some(newName)
            }
          } else {
            rootIFList.last match {
              case parentIntf: Interface => {
                assert(
                  parentIntf != interface
                )
                //println(
                //  s"parentIntf == interface? ${parentIntf == interface}"
                //)
                parentIntf.elementsCache.find{elem => (
                  (
                    elem._1 == newName 
                    || elem._2.getName() == newName
                  ) && (
                    elem._2 != interface
                  )
                )} match {
                  case Some(_) => {
                    //println(
                    //  s"already had this newName: ${newName}"
                    //)
                    doAllocateName(
                      interface=interface,
                      rootIFList=rootIFList,
                      someName=someName,
                      //doFinal=doFinal,
                      //firstIter=false,
                      count=count + 1
                    )
                  }
                  case None => {
                    //intfParent.setName(newName)
                    Some(newName)
                  }
                }
              }
              case _ => {
                interface match {
                  case interface: Interface => {
                    println(
                      s"eek! (${interface.definitionName})"
                    )
                  }
                  case _ => {
                    println(
                      "eek! (unknown `interface.definitionName`)"
                    )
                  }
                }
                None //null
              }
            }
          }
        }
        def outerDoAllocateName(
          interface: Data,
        ): Unit = {
          if (interface == null) {
            return
          }
          interface match {
            case interface: Interface => {
              if (interface.getName() == null || interface.getName() == "") {
                doAllocateName(
                  interface=interface,
                  rootIFList=interface.rootIFList(),
                  someName=interface.definitionName,
                  count=0,
                ) match {
                  case Some(newName) => {
                    //println(
                    //  s"setName: "
                    //  + s"${interface.definitionName} ${newName}"
                    //)
                    interface.setName(newName)
                  }
                  case None => {
                    //println(
                    //  s"no set name? ${interface.definitionName}"
                    //)
                  }
                }
                outerDoAllocateName(
                  interface=interface.IFparent
                )
              }
            }
            case other: Data => {
              outerDoAllocateName(
                interface=other.IFparent
              )
            }
            case _ =>
          }
        }
        outerDoAllocateName(
          interface=node.IFparent
        )
      }
      case _ =>
    }
    //--------
    //--------
    walkDeclarations {
      case node: BaseType if (node.hasTag(IsInterface)) => {
        //last match {
        //  case interface: Interface => {
        //    insertIFmap(
        //      interface=interface,
        //      //firstIter=true,
        //      count=0
        //    )
        //  }
        //  case _ =>
        //}
        val rootIF = node.rootIF()
        if(!allocated.contains(rootIF)) {
          rootIF.setName(node.component.localNamingScope.allocateName(rootIF.getName()))
          allocated += rootIF
        }
        val IFlist = node.rootIFList()
        def getElemName(
          cache: ArrayBuffer[(String, Data)], name: String
        ): Option[(String, Data)] = {
          cache.flatMap{
            case (a, x: Bundle) => getElemName(x.elementsCache, s"${name}_${a}").map(x => (x._1.stripPrefix("_"), x._2))
            case (a, x: Vec[_]) => {
              getElemName(x.elements, s"${name}_${a}").map(x => (x._1.stripPrefix("_"), x._2))
            }
            case (a, x) => if(x == node) Some((s"${name}_${a}".stripPrefix("_"), x)) else None
          }.headOption
        }
        val newName = IFlist match {
          case head :: tail => tail.foldLeft((head, List(head.getName()))){case ((nowIf, nameList), node) =>
            //(node, nowIf.elementsCache.find(_._2 == node).get._1 :: nameList)//TODO:error handle on find.get
            nowIf.elementsCache.find(current => (
              current._2 == node
              //|| current._2.getName() == node.getName()
            )) match {
              case Some(pair) => {
                (node, pair._1 :: nameList)
              }
              case None => {
                println(
                  s"Interface node: "
                  + s"${node.getName()} ${node.getClass.getSimpleName}; "
                  + s"${node.definitionName} "
                )
                //node match {
                //  case interface: Interface => {
                //  }
                //  //case data: Data => {
                //  //  println(
                //  //    s"Data node: "
                //  //    + s"${data.getName()} ${data.getClass.getSimpleName}; "
                //  //  )
                //  //}
                //  case _ => {
                //    println(
                //      s"unknown node type: "
                //      + s"${node.getName()} ${node.getClass.getSimpleName}"
                //    )
                //  }
                //}
                (node, node.getName() :: nameList)
              }
            }
          }._2.reverse.reduce(_ + "." + _) + "." +
            getElemName(IFlist.last.elementsCache, "").getOrElse("no_name", null)._1//TODO:error handle on find.get
        }
        node.name = newName
      }
      case _ =>
    }
    //val svIntfRoot = mutable.LinkedHashSet[Interface]()
    def mkNewGraph(
      interface: Interface,
    ): SvifGraph = {
      return new SvifGraph(
        intfSet={
          val intfSet = mutable.LinkedHashSet[Interface]()
          intfSet += interface
          intfSet
        },
      )
    }
    val svIntfWalkDataMap = mutable.HashMap[String, mutable.HashSet[Interface]]()
    //println(
    //  s"creating nodeWalkDataMap etc." 
    //)
    //var maxRootIFListSize: Int = 0
    def updateWalkData(
      nodeData: Data
    ): Unit = {
      def doAppend(someIntf: Interface): Unit = {
        svIntfWalkDataMap.get(someIntf.origDefinitionName) match {
          case Some(intfSet) => {
            intfSet += someIntf
          }
          case None => {
            val intfSet = mutable.HashSet[Interface]()
            intfSet += someIntf
            svIntfWalkDataMap += (someIntf.origDefinitionName -> intfSet)
          }
        }
      }
      nodeData match {
        case nodeIntf: Interface => {
          nodeIntf.origDefinitionName = nodeIntf.definitionName
          doAppend(someIntf=nodeIntf)
          if (nodeIntf.elementsCache != null) {
            for ((name, elem) <- nodeIntf.elementsCache) {
              updateWalkData(nodeData=elem)
            }
          }
        }
        case nodeVec: Vec[_] => {
          for (vecIdx <- 0 until nodeVec.size) {
            updateWalkData(nodeVec(vecIdx))
          }
        }
        case _ =>
      }
    }
    walkDeclarations {
      case node: BaseType if(node.hasTag(IsInterface)) => {
        val rootIF = node.rootIF()
        if (!svRootIntfFound.contains(rootIF)) {
          svRootIntfFound += rootIF
          //--------
          updateWalkData(nodeData=rootIF)
        }
      }
      case _ =>
    }
    def doUpdateSvifGraph(
      nodeOdn: String,
      nodeIntf: Interface,
      nodeGraph: SvifGraph,
      //nodeIntfSet: mutable.HashSet[Interface],
    ): Unit = {
      def innerUpdate(
        nodeIntf: Interface,
        otherNodeIntf: Interface,
      ): Boolean = {
        val cmpResult = doCompare(
          nodeData=nodeIntf,
          otherNodeData=otherNodeIntf,
        )
        cmpResult match {
          case CmpResultKind.Same => {
            return true
          }
          case CmpResultKind.Diff => {
            return false
          }
          case CmpResultKind.Other => {
            assert(false)
            return false
          }
        }
      }
      //assert(!nodeGraph.intfSet.contains(nodeIntf))
      if (innerUpdate(
        nodeIntf=nodeIntf,
        otherNodeIntf=nodeGraph.anyIntf,
      )) {
        nodeGraph.intfSet += nodeIntf
        return
      } else if (nodeGraph.child != null) {
        doUpdateSvifGraph(
          nodeOdn=nodeOdn,
          nodeIntf=nodeIntf,
          nodeGraph=nodeGraph.child,
        )
      } else {
        nodeGraph.addChild(
          mkNewGraph(interface=nodeIntf)
        )
      }
    }
    for ((nodeOdn, nodeIntfSet) <- svIntfWalkDataMap.view) {
      for (nodeIntf <- nodeIntfSet.view) {
        svIntfGraphMap.get(nodeOdn) match {
          case Some(graphRoot) => {
            doUpdateSvifGraph(
              nodeOdn=nodeOdn,
              nodeIntf=nodeIntf,
              nodeGraph=graphRoot
              //nodeIntfSet=nodeIntfSet,
            )
          }
          case None => {
            svIntfGraphMap += (
              nodeOdn -> mkNewGraph(interface=nodeIntf)
            )
          }
        }
      }
    }

    def lastPasses(graph: SvifGraph, mode: Int): Unit = {
      if (mode == 0) {
        globalScope.lock = false
        val newName = globalScope.allocateName(mkNewName(graph.origDefinitionName, graph.count))
        globalScope.lock = true
        def func(interface: Interface) {
          //if (interface.origDefinitionName != graph.origDefinitionName) {
          //  println(
          //    s"eek! origDefinitionName not same: "
          //    + s"interface:${interface.origDefinitionName} "
          //    + s"graph:${graph.origDefinitionName}"
          //  )
          //}
          //assert(interface.origDefinitionName == graph.origDefinitionName)
          interface.setDefinitionName(name=newName)
        }
        for (interface <- graph.intfSet.view) {
          func(interface=interface)
        }
      } else if (mode == 1) {
        svInterface += (
          graph.anyIntf.definitionName -> emitInterface(graph.anyIntf, true)
        )
      }
      if (
        graph.child != null
      ) {
        lastPasses(
          graph=graph.child,
          mode=mode
        )
      }
    }
    for (mode <- 0 to 1) {
      for ((name, graph) <- svIntfGraphMap) {
        lastPasses(graph=graph, mode=mode)
      }
    }

    //println("lastPass phase done" )
    //--------
  }
}

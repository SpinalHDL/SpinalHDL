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
  def outerDoCompareVec[
    T <: Data
  ](
    nodes: Vec[T],
    vecChainArr: mutable.ArrayBuffer[Vec[_]],
    doConvertIntfVec: Boolean,
  ): Boolean = {
    var ret: Boolean = doConvertIntfVec
    if (ret) {
      if (nodes.size > 1) {
        for (idx <- 0 until nodes.size) {
          if (idx > 0) {
            doCompare(
              nodeData=nodes(idx),
              otherNodeData=nodes(idx - 1),
              vecChainArr=(
                if (idx == 1) (
                  vecChainArr
                ) else (
                  null
                )
              ),
            ) match {  //!= CmpResultKind.Same
              case CmpResultKind.Same => 
              case _ => {
                ret = false
              }
            }
          }
        }
      } else if (nodes.size == 1) {
        vecChainArr += nodes
      }
    }
    return ret
  }
  def doCompare(
    nodeData: Data,
    otherNodeData: Data,
    vecChainArr: mutable.ArrayBuffer[Vec[_]]=null,
  ): CmpResultKind = {
    //--------
    nodeData match {
      case nodeIntf: Interface if !nodeIntf.thisIsNotSVIF => {
        otherNodeData match {
          case otherIntf: Interface if !otherIntf.thisIsNotSVIF => {
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
                        return null
                      }
                    }
                  }
                  if (
                    vecChainArr != null
                    && nodeIntf.IFvecParent != null
                    && otherIntf.IFvecParent != null
                  ) {
                    if (
                      //vecChainArr.size > 0 && vecChainArr.last(0) != nodeIntf.IFvecParent
                      vecChainArr.size == 0
                    ) {
                      nodeIntf.IFvecParent match {
                        case vecParent: Vec[_] => {
                          vecChainArr.find(_ == vecParent) match {
                            case Some(_) => {
                              assert(false)
                            }
                            case None => {
                              vecChainArr.prepend(vecParent)
                            }
                          }
                        }
                        case _ =>
                      }
                    }
                    //else {
                    //}
                  }
                  return CmpResultKind.Same
                } else {
                  return CmpResultKind.Diff
                }
              } else {
                return CmpResultKind.Same
              }
            } else {
              return CmpResultKind.Diff
            }
          }
          case _ => {
            //return CmpResultKind.Other
            return CmpResultKind.Diff
          }
        }
      }
      // TODO: support non-`Interface` `Bundle`s
      case nodeBndl: Bundle => {
        otherNodeData match {
          case otherBndl: Bundle => {
            if (nodeBndl.elementsCache != null && otherBndl.elementsCache != null) {
              if (nodeBndl.elementsCache.size == otherBndl.elementsCache.size) {
                for ((nodeElemName, nodeElem) <- nodeBndl.elementsCache.view) {
                  otherBndl.elementsCache.find{otherElem => {
                    otherElem._1 == nodeElemName
                  }} match {
                    case Some((otherElemName, otherElem)) => {
                      if (doCompare(
                        nodeData=nodeElem,
                        otherNodeData=otherElem,
                        vecChainArr=vecChainArr,
                      ) == CmpResultKind.Diff) {
                        return CmpResultKind.Diff
                      }
                    }
                    case None => {
                      //println(
                      //  s"eek! couldn't find this nodeElemName:${nodeElemName}"
                      //)
                      //assert(false)
                      //return null
                      return CmpResultKind.Diff
                    }
                  }
                }
                return CmpResultKind.Same
              } else {
                return CmpResultKind.Diff
              }
            } else {
              return (
                CmpResultKind.Same
                //CmpResultKind.Diff
              )
            }
          }
          case _ => {
            return CmpResultKind.Diff
          }
        }
      }
      case nodeVec: Vec[_] => {
        otherNodeData match {
          case otherVec: Vec[_] => {
            if (nodeVec.size == otherVec.size) {
              for (vecIdx <- 0 until nodeVec.size) {
                val cmpResult = doCompare(
                  nodeData=nodeVec(vecIdx),
                  otherNodeData=otherVec(vecIdx),
                  vecChainArr=(
                    if (vecIdx == 0) (
                      vecChainArr
                    ) else (
                      null
                    )
                  ),
                  //atTop=false,
                  //parentsAreVecs=true,
                )
                if (cmpResult != CmpResultKind.Same) {
                  //return false
                  return CmpResultKind.Diff
                }
              }
              if (
                vecChainArr != null) {
                //vecSizeArr += nodeVec.size
                vecChainArr.find(_ == nodeVec) match {
                  case Some(_) => {
                    if (vecChainArr.size > 1) {
                      assert(false)
                    }
                  }
                  case None => {
                    vecChainArr.prepend(nodeVec)
                  }
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
  def getParentVec(
    someNode: Data,
    //lastRoot: Data,
    svInterfaceVecFound: mutable.HashSet[Data],
    shouldStopFunc: (Data) => Boolean,
  ): Data = {
    if (!(
      shouldStopFunc(someNode)
      || svInterfaceVecFound.contains(someNode)
      || someNode.IFvecParent == null
    )) {
      someNode.IFvecParent match {
        case parentVec: Vec[_] => {
          var found: Boolean = false
          for ((elem, idx) <- parentVec.view.zipWithIndex) {
            if (!svInterfaceVecFound.contains(elem)) {
              svInterfaceVecFound += elem
              if (elem == someNode) {
                //svInterfaceVecFound += elem
                found = true
              }
            }
          }
          if (found) {
            return parentVec
          }
        }
        case _ => {
        }
      }
    }
    return someNode
  }
  def getElemName(
    node: Data, cache: ArrayBuffer[(String, Data)], name: String
  ): Option[(String, Data)] = {
    cache.flatMap{
      case (a, x: Bundle) => if(x != node) {
        getElemName(node, x.elementsCache, s"${name}_${a}").map(x => (x._1.stripPrefix("_"), x._2))
      } else {
        Some((s"${name}_${a}".stripPrefix("_"), x))
      }
      case (a, x: Vec[_]) => //if(x != node) {
        getElemName(node, x.elements, s"${name}_${a}").map(x => (x._1.stripPrefix("_"), x._2))
      //} else {
      //  Some((s"${name}_${a}".stripPrefix("_"), x))
      //}
      case (a, x) => if(x == node) Some((s"${name}_${a}".stripPrefix("_"), x)) else None
    }.headOption
  }
  def emitInterface(interface: Interface, convertIntfVec: Boolean=true): StringBuilder = {
    import pc._
    val svInterfaceVecFound = mutable.HashSet[Data]()

    var ret = new StringBuilder()
    val theme = new Tab2 //TODO add into SpinalConfig
    val generic = if(interface.genericElements.isEmpty) {
      ""
    } else if (interface.thisIsSVstruct) {
      LocatedPendingError(s"sv struct is still under develop. By now sv generics are not allowed.")
      ""
    } else {
      "#(\n" + interface.genericElements.map{case (name, useless, default) =>
        if(default == null)
          s"${theme.porttab}parameter ${name},\n"
        else
          s"${theme.porttab}parameter ${name} = ${default},\n"
      }.reduce(_ + _).stripSuffix(",\n") + "\n) "
    }
    ret ++= (
      if (!interface.thisIsSVstruct) (
        s"interface ${interface.definitionName} ${generic}() ;\n\n"
      ) else (
        s"typedef struct {\n\n"
      )
    )
    val sizeZero = mutable.HashSet[String]()
    def genBase[T <: Data](ret: StringBuilder, name: String, name1: String, elem: T): Unit = {
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
            //if (subIntfVecSize == 0) (
              s"${name}_${name1}",
            //) else (
            //  s"${name}[${subIntfVecSize}]"
            //),
            x,
          )
        }
      }
    }
    def genSig[T <: Data](ret: StringBuilder, name: String, elem: T, doConvertIntfVec: Boolean=false): Unit = {
      elem match {
        case node: Interface if (!node.thisIsNotSVIF) => {
          @inline def myGetParentVec(
            someNode: Data,
            //lastRoot: Data,
          ): Data = (
            getParentVec(
              someNode=someNode,
              svInterfaceVecFound=svInterfaceVecFound,
              shouldStopFunc=(
                otherNode => (
                  !convertIntfVec
                  || node.noConvertSVIFvec
                )
              )
            )
          )
          val myParentVec: Data = myGetParentVec(node)
          if (myParentVec != node) {
            svInterfaceVecFound += node
            genSig(
              ret=ret,
              name=name,
              elem=myParentVec,
              doConvertIntfVec=true,
            )
          } else {
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
            ret ++= (
              if (!node.thisIsSVstruct) {
                f"${theme.porttab}${t} ${name}();\n${cl}"//TODO:parameter
              } else {
                f"${theme.porttab}${t} ${name};\n${cl}"
              }
            )
          }
        }
        case nodes: Bundle => {
          for ((name1, node) <- nodes.elementsCache) {
            genBase(ret, name, name1, node)
          }
        }
        case nodes: Vec[_] => {
          val vecChainArr = mutable.ArrayBuffer[Vec[_]]()
          val haveAllSameIntf = outerDoCompareVec(
            nodes=nodes,
            vecChainArr=vecChainArr,
            doConvertIntfVec=doConvertIntfVec,
          )

          if (haveAllSameIntf && vecChainArr.size > 0) {
            for ((chainVec, chainIdx) <- vecChainArr.view.zipWithIndex) {
              for ((vecElem, vecElemIdx) <- chainVec.zipWithIndex) {
                vecElem match {
                  case vecElem: Data => {
                    svInterfaceVecFound += vecElem
                  }
                  case _ => {
                    println(
                      s"eek! chainVec:(${chainVec.getName()} ${chainVec.size}) ${vecElemIdx}"
                    )
                    assert(false)
                  }
                }
              }
            }
            var intfDimString: String = ""
            for (vec <- vecChainArr) {
              intfDimString = intfDimString + s"[${vec.size}]"
            }
            val elemName = vecChainArr(0).IFvecNamePrefix
            vecChainArr.last(0) match {
              case intf: Interface => {
                genSig(ret, (elemName + intfDimString), intf)
              }
              case _ =>
            }
          } else if (!svInterfaceVecFound.contains(nodes)) {
            for ((node, idx) <- nodes.zipWithIndex) {
              if (!svInterfaceVecFound.contains(node)) {
                genBase(ret, name, idx.toString, node)
              }
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
      if (!svInterfaceVecFound.contains(elem)) {
        genSig(ret, name, elem)
      }
    }
    ret ++= "\n"
    if(pc.config.svInterfaceIncludeModport && !interface.thisIsNotSVModport) {
      if (interface.thisIsSVstruct) {
        LocatedPendingError(s"sv struct cannot contain sv modport")
      } else {
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
                case elem: Interface if elem.thisIsSVstruct => {
                  val dir = elem.dir match {
                    case `in`    => "input "
                    case `out`   => "output"
                    case `inout` => "inout "
                    case _       => throw new Exception(s"Unknown direction in interface ${interface}: ${elem}"); ""
                  }
                  modportString ++= f"${theme.porttab}${theme.porttab}${dir}%-15s ${name},\n"
                }
                case elem: Interface if !elem.thisIsNotSVIF => {
                  //if (elem.thisIsSVstruct) {
                  //  LocatedPendingError(s"sv struct cannot contain sv modport")
                  //}
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
    }
    //else {
    //}
    ret ++= (
      if (!interface.thisIsSVstruct) {
        "endinterface\n\n"
      } else {
        s"} ${interface.definitionName};\n\n"
      }
    )
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
    //val svIntfRoot = mutable.LinkedHashSet[Interface]()
    def mkNewGraph(
      interface: Interface,
    ): SvifGraph = {
      return new SvifGraph(
        intfSet={
          val intfSet = mutable.HashSet[Interface]()
          intfSet += interface
          intfSet
        },
      )
    }
    val svIntfWalkDataMap = mutable.HashMap[String, mutable.HashSet[Interface]]()
    println(
      s"creating nodeWalkDataMap etc." 
    )
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
        case nodeBndl: Bundle => {
          if (nodeBndl.elementsCache != null) {
            for ((name, elem) <- nodeBndl.elementsCache) {
              updateWalkData(nodeData=elem)
            }
          }
        }
        case nodeVec: Vec[_] => {
          for (vecIdx <- 0 until nodeVec.size) {
            updateWalkData(nodeData=nodeVec(vecIdx))
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
            val newGraph = mkNewGraph(interface=nodeIntf)
            svIntfGraphMap += (
              nodeOdn -> newGraph
            )
            //svIntfGraphArr.prepend((nodeOdn, newGraph))
          }
        }
      }
    }
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
        //val IFlist = node.rootIFList()
        def innerFunc(
          someNode: Data,
          //IFlist: List[Interface],
        ): Unit = {
          //@inline def myGetElemName(
          //  cache: ArrayBuffer[(String, Data)], name: String
          //): Option[(String, Data)] = {
          //  getElemName(someNode, cache, name)
          //}
          val IFlist = someNode.rootIFList().reverse

          var newName: String = ""
          //(
          //  getElemName(someNode, IFlist(0).elementsCache, "").getOrElse("no_name", null)._1
          //)
          //var tempName: String = ""
          //var (prevIntfIsInterface, prevIntf) = IFlist(0)
          var prevIntf = IFlist(0)
          var prevName: String = (
            getElemName(someNode, prevIntf.elementsCache, "").getOrElse("no_name", null)._1
          )
          newName = prevName
          //newName = prevName
          for ((/*(intfIsInterface,*/ intf/*)*/, intfIdx) <- IFlist.view.zipWithIndex) {
            //val tempNode: Data = (
            //  if (intfIdx == IFlist.view.size - 1) (
            //    someNode
            //  ) else (
            //    IFlist.view(intfIdx + 1)
            //  )
            //)
            if (intfIdx > 0) {
              //val myFound = intf.elementsCache.find{
              //  current => {
              //    //current._1 == prevName
              //    current._2 == (
              //      prevIntf
              //    )
              //  }
              //} match { //.getOrElse("no_name", null)
              //  case Some((name, elem)) => {
              //    (name, elem)
              //  }
              //  case None => {
              //    ("no_name", null)
              //  }
              //}
              val myFound = getElemName(
                node=prevIntf, cache=intf.elementsCache, name="" //prevName
              ).getOrElse("no_name", null)
              //println(
              //  s"debug: "
              //  + s"${myFound._1}; "
              //  + s"${prevName} ${prevIntf.getName()}; "
              //  + s"${intf.getName()}; "
              //  + s"${intfIdx}"
              //)
              prevName = myFound._1
              var tempName: String = prevName //+ ""

              val tempSvInterfaceVecFound = mutable.HashSet[Data]()
              val myParentVec: Data = getParentVec(
                someNode=myFound._2,
                svInterfaceVecFound=tempSvInterfaceVecFound,
                shouldStopFunc=(
                  (otherNode) => otherNode match {
                    case otherIntf: Interface if (
                      !(otherIntf.thisIsNotSVIF || otherIntf.noConvertSVIFvec)
                    ) => false
                    case _ => true
                  }
                )
              )
              if (myParentVec != myFound._2) {
                myParentVec match {
                  case parentVec: Vec[_] => {
                    val vecChainArr = mutable.ArrayBuffer[Vec[_]]()
                    val haveAllSameIntf = outerDoCompareVec(
                      nodes=parentVec,
                      vecChainArr=vecChainArr,
                      doConvertIntfVec=true,
                    )
                    if (haveAllSameIntf && vecChainArr.size > 0) {
                      var didFind: Boolean = false
                      for ((vecElem, vecIdx) <- parentVec.view.zipWithIndex) {
                        if (vecElem == myFound._2) {
                          tempName = (
                            prevName.stripSuffix(s"_${vecIdx}") + s"[${vecIdx}]"
                          )
                          didFind = true
                        }
                      }
                      if (!didFind) {
                        println(
                          s"eek! didFind == false: "
                          + s"${parentVec.getName()} ${parentVec.size}"
                        )
                        assert(false)
                      }
                    }
                  }
                  case _ => {
                    println(
                      s"eek! !myParentVec.isInstanceOf[Vec[...]]: "
                      + s"${myParentVec.getName()}"
                    )
                    assert(false)
                  }
                }
                //val temp = (s"${name}[${a}]", x)
                //println(
                //  s"testificate: ${temp}"
                //)
                //if(x == node) Some(temp) else None
              }
              newName = (
                tempName
                //+ (if (intfIdx != 0) "." else "")
                //+ (if (prevIntfIsInterface) "." else "_")
                + "."
                + newName
                //+ {
                //  tempName = getElemName(
                //    tempNode, intf.elementsCache, tempName
                //  ).getOrElse("no_name", null)._1
                //  tempName
                //}
              )
            }
            //prevIntfIsInterface = intfIsInterface
            prevIntf = intf
            if (intfIdx == IFlist.view.size - 1) {
              newName = (
                intf.getName()
                //+ (if (prevIntfIsInterface) "." else "_")
                + "."
                + newName
              )
            }
          }
          someNode.name = newName
          //innerFunc(
          //  someNode=IFlist.last,
          //)
        }
        innerFunc(
          someNode=node,
          //IFlist=node.rootIFList(),
        )
        //def outerFunc(
        //  //someNode: Data
        //): Unit = {
        //  val IFlist = someNode.rootIFList()
        //}
      }
      case _ =>
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
      } else {
        val tempIntf = graph.anyIntf
        if (
          (mode == 1 && tempIntf.thisIsSVstruct)
          || (mode == 2 && !tempIntf.thisIsSVstruct)
        ) {
          svInterface += (
            graph.anyIntf.definitionName -> emitInterface(tempIntf)
          )
        }
      }
      if (mode == 0 || mode == 2) {
        if (graph.child != null) {
          // have to iterate backwards
          lastPasses(
            graph=graph.child,
            mode=mode
          )
        }
      }
    }
    //for ((name, graph) <- svIntfGraphMap) {
    //  svIntfGraphArr.prepend((name, graph))
    //}
    for (mode <- 0 to 2) {
      println(
        s"now on mode ${mode}"
      )
      if (mode == 0 || mode == 2) {
        for ((name, graph) <- svIntfGraphMap) {
          lastPasses(graph=graph, mode=mode)
        }
      } else if (mode == 1) {
        val svInterfaceFound = mutable.HashSet[Interface]()
        walkDeclarations{
          case node: BaseType if (node.hasTag(IsInterface)) => {
            val rootIFList = node.rootIFList()
            def func(idx: Int): Unit = {
              val intf = rootIFList.view(idx)
              if (svInterfaceFound.contains(intf)) {
                return
              }
              svInterfaceFound += intf
              svIntfGraphMap.get(intf.origDefinitionName) match {
                case Some(graphRoot) => {
                  graphRoot.findChildInterface(intf) match {
                    case Some(graph) => {
                      lastPasses(graph=graph, mode=mode)
                    }
                    case None => {
                      println(
                        s"eek! (inner) ${intf.getName()} ${intf.origDefinitionName}"
                      )
                      assert(false)
                    }
                  }
                }
                case None => {
                  println(
                    s"eek! (outer) ${intf.getName()} ${intf.origDefinitionName}"
                    //s"Found `Bundle`? (outer) ${intf.getName()} ${intf.origDefinitionName}"
                  )
                  assert(false)
                }
              }
              if (idx - 1 >= 0) {
                func(idx - 1)
              }
            }
            func(rootIFList.size - 1)
          }
          case _ =>
        }
      }
      //for ((name, graph) <- svIntfGraphArr) {
      //  //lastPasses(graph=graph, mode=mode)
      //}
    }

    //println("lastPass phase done" )
    //--------
  }
}

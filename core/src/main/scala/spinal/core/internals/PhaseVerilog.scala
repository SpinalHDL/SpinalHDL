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
  def emitInterface(interface: Interface): StringBuilder = {
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
        case x => genSig(ret, s"${name}_${name1}", x)
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
            genBase(ret, name, name1, node)
          }
        }
        case nodes: Vec[_] => {
          for ((node, idx) <- nodes.zipWithIndex) {
            genBase(ret, name, idx.toString, node)
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
      //name + (
      //  if (count == 0) (
      //    ""
      //  ) else (
      //    "_" + count
      //  )
      //)
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
          //rootIF.setName(node.component.localNamingScope.allocateName(rootIF.getName()))
          //someInterface.setName(someInterface.component.localNamingScope.allocateName(
          //  s"zzz_Interface_${someDefinitionName}"
          //))
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
              //case parentVec: Vec[_] => {
              //  doAllocateName(
              //    interface=parentVec,
              //    someName=someName,
              //    //firstIter=firstIter,
              //    count=count,
              //  )
              //}
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
              //println(
              //  s"outerDoAllocateName(): this type found: ${interface.getClass.getSimpleName}"
              //)
              outerDoAllocateName(
                interface=other.IFparent
              )
            }
            case _ => {
              //println(
              //  s"outerDoAllocateName(): unknown type: ${interface.getClass.getSimpleName}"
              //)
            }
          }
        }
        outerDoAllocateName(
          interface=node.IFparent
        )
      }
      case _ =>
    }
    //walkDeclarations {
    //  case node: BaseType if(node.hasTag(IsInterface)) => {
    //    node.rootIFList().foreach{interface => {
    //      interface.origDefinitionName = interface.definitionName
    //      //interface.definitionNameCount = 0
    //    }}
    //  }
    //  case _ =>
    //}
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
        def getElemName(cache: ArrayBuffer[(String, Data)], name: String): Option[(String, Data)] = {
          cache.flatMap{
            case (a, x: Bundle) => getElemName(x.elementsCache, s"${name}_${a}").map(x => (x._1.stripPrefix("_"), x._2))
            case (a, x: Vec[_]) => getElemName(x.elements, s"${name}_${a}").map(x => (x._1.stripPrefix("_"), x._2))
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
      //rootIFList: List[Interface],
      //idx: Int,
    ): SvifGraph = {
      return new SvifGraph(
        //intfCache={
        //  val intfCache = mutable.ArrayBuffer[(StringBuilder, mutable.LinkedHashSet[Interface])]()
        //  intfCache += (
        //    emitInterface(interface) -> {
        //      val intfSet = mutable.LinkedHashSet[Interface]()
        //      intfSet += interface
        //      intfSet
        //    }
        //  )
        //  intfCache
        //},
        //origDefinitionName=interface.origDefinitionName,
        intfSet={
          //val intfMap = mutable.LinkedHashMap[Interface, (List[Interface], Int)]()
          //intfMap += (interface -> (rootIFList, idx))
          //intfMap
          val intfSet = mutable.LinkedHashSet[Interface]()
          intfSet += interface
          intfSet
        },
        //interfaceString=emitInterface(interface)
      )
    }
    //val svIntfToGraphMap = mutable.LinkedHashMap[Interface, SvifGraph]()
    //case class NodeWalkData(
    //  //val node: BaseType,
    //  val rootIFList: List[Interface],
    //  val rootIFMap: mutable.LinkedHashMap[Interface, StringBuilder],
    //  var cnt: Int,
    //) {
    //}
    //val nodeWalkDataMap = mutable.LinkedHashMap[Interface, NodeWalkData]()
    val svIntfWalkDataMap = mutable.HashMap[String, mutable.HashSet[Interface]]()
    ////val svRootIFListMap
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
        //val nodeRootIFList = node.rootIFList()
        val rootIF = node.rootIF()
        if (!svRootIntfFound.contains(rootIF)) {
          svRootIntfFound += rootIF
          //--------
          //for (nodeIntf <- nodeRootIFList.view) {
            updateWalkData(nodeData=rootIF)
          //}
        }
        //--------
        //svIntfWalkDataMap.get(nodeRootIF
        //val tempRootIFList = node.rootIFList()
        //val currNodeWalkData: NodeWalkData = nodeWalkDataMap.get(tempRootIFList.last) match {
        //  case Some(myNodeData) => {
        //    myNodeData
        //  }
        //  case None => {
        //    val tempRootIFMap = mutable.LinkedHashMap[Interface, StringBuilder]()
        //    for (interface <- tempRootIFList) {
        //      tempRootIFMap += (interface -> emitInterface(interface))
        //    }
        //    val myNodeData = NodeWalkData(
        //      //node=node,
        //      rootIFList=tempRootIFList,
        //      rootIFMap=tempRootIFMap,
        //      cnt=(
        //        tempRootIFList.size - 1
        //        //0
        //        //-1
        //      ),
        //    )
        //    //if (maxRootIFListSize < tempRootIFMap.size) {
        //    //  maxRootIFListSize = tempRootIFMap.size
        //    //}
        //    nodeWalkDataMap += (tempRootIFList.last -> myNodeData)
        //    myNodeData
        //  }
        //}
        //def rootIFList = currNodeWalkData.rootIFList
        ////val rootIFMap = currNodeWalkData.rootIFMap
        //def updateIntfData(
        //  interface: Interface
        //): Unit = {
        //  svIntfWalkDataMap.get(interface.origDefinitionName) match {
        //    case Some(btSet) => {
        //      btSet += tempRootIFList.last
        //    }
        //    case None => {
        //      val btSet = mutable.LinkedHashSet[Interface]()
        //      btSet += tempRootIFList.last
        //      svIntfWalkDataMap += (
        //        interface.origDefinitionName -> btSet
        //      )
        //    }
        //  }
        //}
        //for (interface <- rootIFList) {
        //  updateIntfData(interface=interface)
        //}
      }
      case _ =>
    }
    //walkDeclarations {
    //  case intf: Interface => {
    //    updateWalkData(nodeIntf=intf)
    //    //svIntfWalkDataMap.get(intf.origDefinitionName) match {
    //    //  case Some(intfSet) => {
    //    //    intfSet += 
    //    //  }
    //    //  case None => {
    //    //    // handle the empty `Interface` ones we missed before
    //    //    // `emitInterface()` will print out empty `Interface`s!
    //    //    updateWalkData(nodeIntf=intf)
    //    //  }
    //    //}
    //  }
    //  case _ =>
    //}
    ////println(
    ////  s"updating nodeWalkDataMap count values"
    ////)
    ////for ((node, nodeWalkData) <- nodeWalkDataMap.view) {
    ////  nodeWalkData.cnt = maxRootIFListSize - 1
    ////}
    ////println("--------")
    println(
      s"finished creating nodeWalkDataMap etc."
    )
    def doUpdateSvifGraph(
      nodeOdn: String,
      nodeIntf: Interface,
      nodeGraph: SvifGraph,
      //nodeIntfSet: mutable.HashSet[Interface],
    ): Unit = {
      //def myGetName(
      //  someIntf: Data
      //): String = {
      //  if (someIntf.IFparent == null) {
      //    return someIntf.getName() // this is set via the code above with getElemName
      //  } else {
      //    someIntf.IFparent match {
      //      case intfParent: Interface => {
      //        assert(intfParent.elementsCache != null)
      //        for ((name, elem) <- intfParent.elementsCache) {
      //          if (elem == someIntf) {
      //            return name
      //          }
      //        }
      //        println(
      //          s"eek! this interface wasn't found: "
      //          + s"${someIntf.getName()}"
      //        )
      //        assert(false)
      //      }
      //      case _ => {
      //        return myGetName(someIntf=someIntf.IFparent)
      //      }
      //    }
      //    return ""
      //  }
      //}
      sealed trait CmpResultKind
      object CmpResultKind {
        case object Same extends CmpResultKind
        case object Diff extends CmpResultKind
        case object Other extends CmpResultKind
      }
      def doCompare(
        nodeData: Data,
        //nodeName: String,
        otherNodeData: Data,
        //otherNodeName: String,
        //atTop: Boolean,
        //parentsAreVecs: Boolean,
      ): CmpResultKind = {
        //--------
        //if (nodeData.isInstanceOf[BaseType] && otherNodeData.isInstanceOf[BaseType]) {
        //  return false
        //} else if (nodeData.isInstanceOf[Interface] && otherNodeData.isInstanceOf[Interface]) {
        //} else if (nodeData.isInstanceOf[Bundle] && otherNodeData.isInstanceOf[Bundle]) {
        //} else if (nodeData.isInstanceOf[Vec[_]] && otherNodeData.isInstanceOf[Vec[_]]) {
        //} else {
        //  return false
        //}
        //--------
        nodeData match {
          //case nodeBt: BaseType => {
          //  return false
          //}
          case nodeIntf: Interface => {
            otherNodeData match {
              //case otherBt: BaseType => {
              //  return false
              //}
              case otherIntf: Interface => {
                if (
                  emitInterface(nodeIntf).result()
                  == emitInterface(otherIntf).result()
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
                //return false
                // TODO: does this need to be `CmpKind.Other`?
                return CmpResultKind.Diff
                //assert(false)
                //return null
              }
            }
          }
          //case nodeBndl: Bundle => {
          //  return false
          //}
          case _ => {
            return CmpResultKind.Other
          }
        }
        //return false
        //return CmpKind.Other
        assert(false)
        return null
      }
      def innerUpdate(
        nodeIntf: Interface,
        otherNodeIntf: Interface,
      ): Boolean = {
        //val nodeRootIF = nodeIntf.rootIF()
        //val otherNodeRootIF = otherNodeIntf.rootIF()
        val cmpResult = doCompare(
          nodeData=nodeIntf,
          otherNodeData=otherNodeIntf,
          //atTop=true,
          //parentsAreVecs=false,
        )
        //if (cmpResult == CmpKind.Same) {
        //  //println(
        //  //  s"This one is the same: "
        //  //)
        //  //println(
        //  //  s"${otherNodeIntf.getName()}"
        //  //)
        //  ////println("-- --")
        //  ////println(emitInterface(nodeIntf).result())
        //  //println(emitInterface(otherNodeIntf).result())
        //  //println("-- --")
        //  ////println("----")
        //} else {
        //}
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
      //for (nodeIntf <- nodeIntfSet.view) {
        //println(
        //  s"nodeIntf(${nodeIntf.getName()} ${nodeIntf.origDefinitionName})"
        //)
        //println(emitInterface(nodeIntf).result())
        //--------
        //for (otherNodeIntf <- nodeIntfSet.view) {
        //  if (nodeIntf != otherNodeIntf) {
        //    //println("-- --")
        //    if (innerUpdate(
        //      nodeIntf=nodeIntf,
        //      otherNodeIntf=otherNodeIntf,
        //    )) {
        //      return
        //    }
        //  }
        //}
        assert(!nodeGraph.intfSet.contains(nodeIntf))
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
        //--------
        //println("----")
      //}
    }
    //println("--------")
    for ((nodeOdn, nodeIntfSet) <- svIntfWalkDataMap.view) {
      //println(
      //  s"current odn:${nodeOdn}"
      //)
      //println("--- ---")
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
      //println("--------")
    }
    //var finishedWalk: Boolean = false
    ////while (!finishedWalk) {
    ////  finishedWalk = true
    //  //walkDeclarations {
    //  //  case node: BaseType if(node.hasTag(IsInterface)) => {
    //  for ((nodeIntf, nodeWalkData) <- nodeWalkDataMap) {
    //    val currNodeWalkData = nodeWalkDataMap.getOrElse(nodeIntf, null)
    //    val rootIFList = currNodeWalkData.rootIFList.view
    //    //if (!didFirstWalk) {
    //    //  def updateIntfData(
    //    //    interface: Interface
    //    //  ): Unit = {
    //    //    svIntfWalkDataMap.get(interface) match {
    //    //      case Some(btSet) => {
    //    //        btSet += node
    //    //      }
    //    //      case None => {
    //    //        val btSet = mutable.LinkedHashSet[BaseType]()
    //    //        btSet += node
    //    //        svIntfWalkDataMap += (
    //    //          interface -> btSet
    //    //        )
    //    //      }
    //    //    }
    //    //  }
    //    //  for (interface <- rootIFList) {
    //    //    updateIntfData(interface=interface)
    //    //  }
    //    //}
    //    //svIntfRoot += node.rootIFList() //node.rootIFList().last
    //    //svIntfRoot += node.rootIF()
    //    //val rootIFList = node.rootIFList()
    //    def func(
    //      outerIdx: Int,
    //    ): Boolean = {
    //      if (outerIdx >= rootIFList.view.size) {
    //        return true
    //      }
    //      val interface = rootIFList.view(outerIdx)
    //      //if (
    //      //  outerIdx == 0
    //      //) {
    //        //if (svInterfaceFound.contains(interface)) {
    //        //  return false
    //        //}
    //        //svInterfaceFound += interface
    //      //}
    //      def walk(
    //        argWalkIntf: Interface,
    //        argWalkIdx: Int,
    //        //argFirstWalkIdx: Int,
    //        //argFoundKindArr: mutable.ArrayBuffer[Int],
    //      ): (Int, Interface, Boolean, SvifGraph) = {
    //        svIntfGraphMap.get(argWalkIntf.origDefinitionName) match {
    //          case Some(graphRoot) => {
    //            //println(
    //            //  s"argWalkIntf:(${argWalkIntf.getName()} ${argWalkIntf.origDefinitionName}); "
    //            //  + s"argWalkIdx:${argWalkIdx}"
    //            //)
    //            val btSet = svIntfWalkDataMap.getOrElse(argWalkIntf.origDefinitionName, null) 
    //            for (bt <- btSet.view) {
    //              //argFoundKindArr(0) = 0
    //              var myFoundKind: Int = 0
    //              val btWalkData = nodeWalkDataMap.getOrElse(bt, null)
    //              if (
    //                //bt.rootIFList().last != node.rootIFList().last
    //                //&& bt != node
    //                //emitInterface(bt.rootIFList().last).result()
    //                //== emitInterface(node.rootIFList().last).result()
    //                //!bt.rootIFList().contains(argWalkIntf)
    //                !btWalkData.rootIFMap.contains(argWalkIntf)
    //                //!btWalkData.rootIFMap.contains(interface)
    //                //true
    //              ) {
    //                //val nodeWalkData = nodeWalkDataMap.getOrElse(node, null)
    //                val btRootIFList = btWalkData.rootIFList
    //                //for ((btIntf, btIntfIdx) <- btRootIFList.view.zipWithIndex) {
    //                //}
    //                def innerWalk(
    //                  argTempWalkIdx: Int,
    //                  argBtTempWalkIdx: Int,
    //                ): Unit = {
    //                  //println(
    //                  //  s"innrWalk(${argTempWalkIdx}, ${argBtTempWalkIdx})"
    //                  //)
    //                  val argTempWalkIntf = rootIFList.view(argTempWalkIdx)
    //                  if (
    //                    (argTempWalkIdx >= 0)
    //                    //&& (argTempWalkIdx < rootIFList.size)
    //                    && (argBtTempWalkIdx >= 0)
    //                    //&& (argBtTempWalkIdx < btRootIFList.size)
    //                  ) {
    //                    val argBtTempWalkIntf = btRootIFList.view(argBtTempWalkIdx)
    //                    //println(
    //                    //  s"innerWalk() check: "
    //                    //)
    //                    //println(
    //                    //  s"arg:("
    //                    //    + s"${argTempWalkIntf.component.definitionName} "
    //                    //    + s"${argTempWalkIntf.getName()} "
    //                    //    + s"${argTempWalkIntf.origDefinitionName}"
    //                    //  + s") "
    //                    //)
    //                    //println(
    //                    //  s"bt:("
    //                    //    + s"${argBtTempWalkIntf.component.definitionName} "
    //                    //    + s"${argBtTempWalkIntf.getName()} "
    //                    //    + s"${argBtTempWalkIntf.origDefinitionName}"
    //                    //  + s") "
    //                    //)
    //                    //println(
    //                    //  s"innerWalk:(${argWalkIntf.getName()} ${argWalkIntf.origDefinitionName}); "
    //                    //  + s"argWalkIdx:${argTempWalkIdx}"
    //                    //)
    //                    //println("----")
    //                    if (
    //                      //argTempWalkIntf.origDefinitionName
    //                      //== argBtTempWalkIntf.origDefinitionName
    //                      true
    //                    ) {
    //                      //println(
    //                      //  s"innerWalk() check: "
    //                      //)
    //                      //println(
    //                      //  s"arg:("
    //                      //    + s"${argTempWalkIntf.component.definitionName} "
    //                      //    + s"${argTempWalkIntf.getName()} "
    //                      //    + s"${argTempWalkIntf.origDefinitionName}"
    //                      //  + s") "
    //                      //)
    //                      //println(
    //                      //  s"bt:("
    //                      //    + s"${argBtTempWalkIntf.component.definitionName} "
    //                      //    + s"${argBtTempWalkIntf.getName()} "
    //                      //    + s"${argBtTempWalkIntf.origDefinitionName}"
    //                      //  + s") "
    //                      //)
    //                      //println("----")
    //                      //println(
    //                      //  emitInterface(argTempWalkIntf).result()
    //                      //)
    //                      //println("----")
    //                      //println(
    //                      //  emitInterface(argBtTempWalkIntf).result()
    //                      //)
    //                      if (
    //                        emitInterface(argTempWalkIntf).result()
    //                        != currNodeWalkData.rootIFMap.getOrElse(argTempWalkIntf, null).result()
    //                      ) {
    //                        println("-- --")
    //                        println(
    //                          "eek! emitInterface(argTempWalkIntf).result() compare not equal!"
    //                        )
    //                        println("----")
    //                        println(
    //                          emitInterface(argTempWalkIntf).result()
    //                        )
    //                        println("----")
    //                        println(
    //                          currNodeWalkData.rootIFMap.getOrElse(argTempWalkIntf, null).result()
    //                        )
    //                        println("-- --")
    //                        assert(false)
    //                      }
    //                      if (
    //                        emitInterface(argTempWalkIntf).result()
    //                        //currNodeWalkData.rootIFMap.getOrElse(argTempWalkIntf, null).result()
    //                        == emitInterface(argBtTempWalkIntf).result()
    //                      ) {
    //                        //println()
    //                        //argFoundKindArr += true
    //                        //argFoundKindArr(0) += 1
    //                        myFoundKind += 1
    //                        //println(
    //                        //  emitInterface(argBtTempWalkIntf).result()
    //                        //)
    //                        //println("--------")
    //                        //argFoundKindSet += false
    //                        //return
    //                      }
    //                    }
    //                    //println("--------")
    //                    //return argBtTempWalkIdx
    //                    if (
    //                      argTempWalkIdx + 1 < rootIFList.size
    //                      && argBtTempWalkIdx + 1 < btRootIFList.size
    //                    ) {
    //                      /*return*/ innerWalk(
    //                        argTempWalkIdx=argTempWalkIdx + 1,
    //                        argBtTempWalkIdx=argBtTempWalkIdx + 1,
    //                      )
    //                    } else {
    //                      //return argBtTempWalkIdx
    //                      //if (rootIFList.size == btRootIFList.size) {
    //                      //}
    //                      //argFoundKindSet += true
    //                    }
    //                  } else {
    //                    //return -1
    //                    println("-- --")
    //                    println(
    //                      s"out of range: "
    //                      + s"arg(${argTempWalkIdx} ${rootIFList.size}) "
    //                      + s"bt(${argBtTempWalkIdx} ${btRootIFList.size})"
    //                    )
    //                    println("----")
    //                    //--------
    //                    println(
    //                      "arg interface:"
    //                    )
    //                    println(
    //                      s"${argTempWalkIntf.getName()} ${argTempWalkIntf.origDefinitionName}"
    //                    )
    //                    //println(
    //                    //  emitInterface(argTempWalkIntf).result()
    //                    //)
    //                    println("----")
    //                    println("bt interfaces:")
    //                    for ((btIntf, btIdx) <- btRootIFList.view.zipWithIndex) {
    //                      println(
    //                        s"${btIntf.getName()} ${btIntf.origDefinitionName}"
    //                      )
    //                      //println(
    //                      //  emitInterface(btIntf).result()
    //                      //)
    //                      //println("----")
    //                    }
    //                    println("-- --")
    //                    //println(
    //                    //  btRootIFList.view.find{btIntf => {
    //                    //    btIntf.
    //                    //  }}
    //                    //)
    //                    //--------

    //                    ////if (
    //                    ////  (
    //                    ////    argTempWalkIdx >= 0
    //                    ////    && argTempWalkIdx < rootIFList.size
    //                    ////  ) || (
    //                    ////    argBtTempWalkIdx >= 0
    //                    ////    && argBtTempWalkIdx < btRootIFList.size
    //                    ////  )
    //                    ////) {
    //                    ////  argFoundKindArr(0) = -1
    //                    ////}
    //                  }
    //                }
    //                val tempArgBtTempWalkIdx = btRootIFList.size - rootIFList.size + argWalkIdx
    //                innerWalk(
    //                  argTempWalkIdx=argWalkIdx,
    //                  argBtTempWalkIdx=(
    //                    tempArgBtTempWalkIdx
    //                    //rootIFList.size.max(btRootIFList.size) + argWalkIdx
    //                    //rootIFList.size - btRootIFList.size + argWalkIdx
    //                  )
    //                )
    //                //val tempSize = (btRootIFList.size - rootIFList.size).abs //rootIFList.size.min(btRootIFList.size)
    //                val tempSize: Int = rootIFList.size - argWalkIdx
    //                //println(
    //                //  s"myFoundKind:${myFoundKind} tempSize:${tempSize}"
    //                //)
    //                if (
    //                  //argFoundKindArr(0) 
    //                  myFoundKind == tempSize
    //                ) {
    //                  println(
    //                    s"myFoundKind:${myFoundKind} tempSize:${tempSize}"
    //                  )
    //                  assert(
    //                    tempArgBtTempWalkIdx >= 0
    //                    && tempArgBtTempWalkIdx < btRootIFList.size
    //                  )
    //                  val tempBtIntf = (
    //                    btRootIFList(
    //                      tempArgBtTempWalkIdx
    //                      //if (
    //                      //  tempArgBtTempWalkIdx >= 0
    //                      //  && tempArgBtTempWalkIdx < btRootIFList.size
    //                      //) (
    //                      //  tempArgBtTempWalkIdx
    //                      //) else if (
    //                      //  tempArgBtTempWalkIdx >= 0
    //                      //) (
    //                      //  btRootIFList.size - 1
    //                      //) else (
    //                      //  0
    //                      //)
    //                    )
    //                  )

    //                  val (isTop, graph) = graphRoot.findInterface(toFind=tempBtIntf)
    //                  //assert(graph != null)

    //                  return (
    //                    //argFoundKindArr, 
    //                    myFoundKind,
    //                    argWalkIntf, graph != null, graph
    //                  )
    //                  //return (argFoundKindArr, argWalkIntf, true, graph)
    //                }
    //              } else {
    //                //println(
    //                //  s"Didn't get to `innerWalk()`: "
    //                //)
    //                //println(
    //                //  s"argWalkIntf("
    //                //    + s"${argWalkIntf.component.definitionName} "
    //                //    + s"${argWalkIntf.getName()} "
    //                //    + s"${argWalkIntf.origDefinitionName}"
    //                //  + s") "
    //                //  + s"argWalkIdx:${argWalkIdx} "
    //                //  + s"riflsz:${rootIFList.size}; "
    //                //)
    //                //println(
    //                //  s"btLast("
    //                //    + s"${bt.rootIFList().last.component.definitionName} "
    //                //    + s"${bt.rootIFList().last.getName()} "
    //                //    + s"${bt.rootIFList().last.origDefinitionName}"
    //                //  + s"); "
    //                //)
    //                //println(
    //                //  s"nodeLast("
    //                //    + s"${node.rootIFList().last.component.definitionName} "
    //                //  +   s"${node.rootIFList().last.getName()} "
    //                //  +   s"${node.rootIFList().last.origDefinitionName}"
    //                //  + s"); "
    //                //)
    //                //println(
    //                //  s"rootIFList.last("
    //                //    + s"${rootIFList.last.component.definitionName} "
    //                //    +  s"${rootIFList.last.getName()} "
    //                //    +  s"${rootIFList.last.origDefinitionName}"
    //                //  + s")"
    //                //)
    //                //println("----")
    //              }
    //            }
    //            return (
    //              //argFoundKindArr, 
    //              -1, 
    //              argWalkIntf, false, null
    //            )
    //          }
    //          case None => {
    //            println(
    //              s"didn't find this odn: "
    //              + s"argWalkIntf(${argWalkIntf.getName} ${argWalkIntf.origDefinitionName}) "
    //              + s"argWalkIdx:${argWalkIdx} "
    //              + s"riflsz:${rootIFList.size}"
    //            )
    //            val newGraphRoot = mkNewGraph(interface=argWalkIntf)
    //            svIntfGraphMap += (
    //              argWalkIntf.origDefinitionName -> newGraphRoot
    //            )
    //            //return walk(
    //            //  argWalkIntf=argWalkIntf,
    //            //  argWalkIdx=argWalkIdx,
    //            //  argFoundKindArr=argFoundKindArr,
    //            //)
    //            return (
    //              //argFoundKindArr, 
    //              -1,
    //              argWalkIntf, true, newGraphRoot
    //            )
    //          }
    //        }
    //      }
    //      //for (someIdx <- rootIFList.size - 1 downto outerIdx)
    //      var lastIdx: Int = outerIdx
    //      val walkIntf: Interface = rootIFList(lastIdx)
    //      //do {
    //        //val myGraph: SvifGraph = null
    //        //var walkIntf: Interface = rootIFList(lastIdx)
    //        //= rootIFList(someIdx) //interface
    //        //val someIdx = lastIdx
    //        //val foundKindArr = mutable.ArrayBuffer[Int]()
    //        //foundKindArr += 0
    //        //val foundKind = 0
    //        val foundNone: Boolean = svIntfGraphMap.get(walkIntf.origDefinitionName) match {
    //          case Some(graphRoot) => {
    //            val (isTop, graph) = graphRoot.findInterface(toFind=walkIntf)
    //            //(graph == null)
    //            false
    //          }
    //          case None => true
    //        }
    //        //do {
    //          val parts = walk(
    //            argWalkIntf=(
    //              //rootIFList(lastIdx)
    //              walkIntf
    //            ),
    //            argWalkIdx=(
    //              //someIdx
    //              lastIdx
    //            ),
    //            //argFirstWalkIdx=(
    //            //  lastIdx
    //            //),
    //            //argFoundKindArr=(
    //            //  foundKindArr
    //            //),
    //          )
    //          //val foundSame = (parts._1(0) == parts._3)
    //          val foundSame = parts._3
    //          val tempGraph = parts._4
    //          //val foundFirstNone = parts._1.contains(true)
    //          //lastIdx = parts._3
    //          ////walkIntf = parts._2
    //          //println(
    //          //  s"new parts._2:(${parts._2.getName()} ${parts._2.origDefinitionName}); "
    //          //  + s"parts._3:${parts._3}"
    //          //)
    //          //walkIntf = rootIFList(lastIdx)
    //          //myGraph = parts._4
    //          if (
    //            //myGraphRoot != null
    //            //!foundNone
    //            //true
    //            //!svInterfaceFound.contains(walkIntf)
    //            true
    //          ) {
    //            //svInterfaceFound += walkIntf
    //            //walkIntf = rootIFList(lastIdx)
    //            println(emitInterface(walkIntf).result())
    //            if (foundSame) {
    //              if (tempGraph != null) {
    //                //assert(parts._4 != null)
    //                //val (tempGraphFoundIsTop, tempGraph) = (
    //                //  svIntfGraphMap.get(walkIntf.origDefinitionName) match {
    //                //    case Some(graphRoot) => {
    //                //      graphRoot.findInterfaceString(
    //                //        emitInterfaceFunc=emitInterface,
    //                //        toFind=emitInterface(walkIntf),
    //                //      )
    //                //      //graphRoot
    //                //    }
    //                //    case None => {
    //                //      println(
    //                //        s"(foundSame == true) eek!"
    //                //      )
    //                //      assert(false)
    //                //      (false, null)
    //                //      //null
    //                //    }
    //                //  }
    //                //)
    //                ////assert(!tempGraphFoundIsTop)
    //                //if (!tempGraph.intfSet.contains(walkIntf)) {
    //                //if (
    //                //  !svInterfaceFound.contains(walkIntf)
    //                //  //&& !foundFirstNone
    //                //) {
    //                  println(
    //                    s"(foundSame == true): (YES) adding this interface: "
    //                    //+ s"foundFirstNone:${foundFirstNone} "
    //                    //+ s"isTop:${tempGraphFoundIsTop} "
    //                    + s"component:${walkIntf.component.definitionName}: "
    //                    + s"walkIntf:(${walkIntf.getName()} ${walkIntf.origDefinitionName}): "
    //                    + s"(tgodn:${tempGraph.origDefinitionName} "
    //                    + s"tgc:${tempGraph.count} tch:${tempGraph.child != null}) "
    //                    //+ s"(mgodn:${myGraph.origDefinitionName} "
    //                    //+ s"mgc:${myGraph.count} mcsz:${myGraph.children.size})"
    //                  )
    //                  //svInterfaceFound += walkIntf
    //                  tempGraph.intfSet += walkIntf
    //                  //if (tempGraph.children.size != 0) {
    //                  //  assert(tempGraph.children.size == 1)
    //                  //  assert(tempGraph.children(0).count == tempGraph.count + 1)
    //                  //}
    //                //} else {
    //                //  println(
    //                //    s"(foundSame == true): (NOT) adding this interface: "
    //                //    + s"foundFirstNone:${foundFirstNone} "
    //                //    + s"isTop:${tempGraphFoundIsTop} "
    //                //    + s"component:${walkIntf.component.definitionName}: "
    //                //    + s"walkIntf:(${walkIntf.getName()} ${walkIntf.origDefinitionName}): "
    //                //    + s"(tgodn:${tempGraph.origDefinitionName} "
    //                //    + s"tgc:${tempGraph.count} tcsz:${tempGraph.children.size})"
    //                //    //+ s"(mgodn:${myGraph.origDefinitionName} "
    //                //    //+ s"mgc:${myGraph.count} mcsz:${myGraph.children.size})"
    //                //  )
    //                //}
    //                //}
    //              }
    //            } else { //if (!foundSame)
    //              //walkIntf = parts._2
    //              //if (myGraph == null) {
    //              //  svInterfaceFound += walkIntf
    //              //  println(
    //              //    s"(myGraph == null): (YES) adding this interface: "
    //              //    //+ s"isTop:${tempGraphFoundIsTop} "
    //              //    + s"component:${walkIntf.component.definitionName}: "
    //              //    + s"walkIntf:(${walkIntf.getName()} ${walkIntf.origDefinitionName}): "
    //              //    //+ s"godn:${graphTop.origDefinitionName} "
    //              //    //+ s"gc:${graphTop.count} cc:${child.count}"
    //              //  )
    //              //  myGraph = mkNewGraph(interface=walkIntf)
    //              //  svIntfGraphMap += (
    //              //    walkIntf.origDefinitionName -> myGraph
    //              //  )
    //              //} else {

    //              //val ((tempGraphFoundIsTop, tempGraph), haveNone)// 
    //              val graphRoot: SvifGraph = (
    //                svIntfGraphMap.get(walkIntf.origDefinitionName) match {
    //                  case Some(graphRoot) => {
    //                    graphRoot
    //                    //(
    //                    //  graphRoot.findInterfaceString(
    //                    //    emitInterfaceFunc=emitInterface,
    //                    //    toFind=emitInterface(walkIntf),
    //                    //  ),
    //                    //  false
    //                    //)
    //                  }
    //                  case None => {
    //                    println(
    //                      s"(foundSame == false) eek!"
    //                    )
    //                    assert(false)
    //                    //svInterfaceFound += walkIntf
    //                    ////println(
    //                    ////  s"(foundSame == false): (adding odn) adding this interface: "
    //                    ////  //+ s"isTop:${tempGraphFoundIsTop} "
    //                    ////  + s"component:${walkIntf.component.definitionName}: "
    //                    ////  + s"walkIntf:(${walkIntf.getName()} ${walkIntf.origDefinitionName}): "
    //                    ////  //+ s"godn:${graphTop.origDefinitionName} "
    //                    ////  //+ s"gc:${graphTop.count} cc:${child.count}"
    //                    ////)
    //                    //myGraph = mkNewGraph(interface=walkIntf)
    //                    //svIntfGraphMap += (
    //                    //  walkIntf.origDefinitionName -> myGraph
    //                    //)
    //                    //--------
    //                    //((false, null), true)
    //                    null
    //                  }
    //                }
    //              )
    //              //val (tempGraphFoundIsTop, tempGraph) = (
    //              //  myGraph.findInterfaceString(
    //              //    emitInterfaceFunc=emitInterface,
    //              //    toFind=emitInterface(walkIntf),
    //              //  )
    //              //)
    //              //val graphTop = tempGraph.getTop()
    //              val graphTop = graphRoot.getTop()
    //              //if (
    //              //  !haveNone
    //              //  //&& !svInterfaceFound.contains(walkIntf)
    //              //  //&& !argFoundSameSet.contains(true)
    //              //  //&& !foundFirstNone
    //              //) {
    //                //if (graphTop == tempGraph) {
    //                  val child = mkNewGraph(
    //                    interface=walkIntf
    //                  )
    //                  graphTop.addChild(newChild=child)
    //                  println(
    //                    //s"(foundSame == false): (graphTop == tempGraph) adding this interface: "
    //                    s"(foundSame == false): (YES) adding this interface: "
    //                    + s"foundContains:${svInterfaceFound.contains(walkIntf)} "
    //                    //+ s"isTop:${tempGraphFoundIsTop} "
    //                    + s"component:${walkIntf.component.definitionName}: "
    //                    + s"walkIntf:(${walkIntf.getName()} ${walkIntf.origDefinitionName}): "
    //                    + s"(godn:${graphTop.origDefinitionName} "
    //                    + s"gc:${graphTop.count} cc:${child.count}) "
    //                    ////+ s"(mgodn:${myGraph.origDefinitionName} "
    //                    ////+ s"mgc:${myGraph.count} mcsz:${myGraph.children.size})"
    //                    //+ s"(tgodn:${tempGraph.origDefinitionName} "
    //                    //+ s"tgc:${tempGraph.count} tcsz:${tempGraph.children.size})"
    //                  )
    //                  assert(child.count == graphTop.count + 1)
    //                //}
    //                //svInterfaceFound += walkIntf
    //              //} 
    //              //else {
    //              //  println(
    //              //    s"(foundSame == false): (NOT) adding this interface: "
    //              //    + s"foundContains:${svInterfaceFound.contains(walkIntf)} "
    //              //    + s"isTop:${tempGraphFoundIsTop} "
    //              //    + s"component:${walkIntf.component.definitionName}: "
    //              //    + s"walkIntf:(${walkIntf.getName()} ${walkIntf.origDefinitionName}): "
    //              //    + s"(godn:${graphTop.origDefinitionName} "
    //              //    + s"gc:${graphTop.count}) "
    //              //    + s"(tgodn:${tempGraph.origDefinitionName} "
    //              //    + s"tgc:${tempGraph.count} tcsz:${tempGraph.children.size})"
    //              //  )
    //              //}
    //              //lastIdx += 1
    //            }
    //          //}
    //          //} else { // if (myGraphRoot == null)
    //          //  //assert(lastIdx == -1)
    //          //  println(
    //          //    s"(myGraph == null): adding this interface: "
    //          //    //+ s"isTop:${tempGraphFoundIsTop} "
    //          //    + s"component:${walkIntf.component.definitionName}: "
    //          //    + s"walkIntf:(${walkIntf.getName()} ${walkIntf.origDefinitionName}): "
    //          //    //+ s"godn:${graphTop.origDefinitionName} "
    //          //    //+ s"gc:${graphTop.count} cc:${child.count}"
    //          //  )
    //          //  svIntfGraphMap += (
    //          //    walkIntf.origDefinitionName -> mkNewGraph(interface=walkIntf)
    //          //  )
    //          //  //assert(false)
    //          }
    //        //} while (myGraphRoot == null)
    //        //if (myGraph != null && !(lastIdx + 1 < rootIFList.size)) {
    //        //  println(
    //        //    s"lastIdx += 1: "
    //        //    + s"lastIdx:${lastIdx} riflsz:${rootIFList.size}"
    //        //  )
    //        //  lastIdx += 1
    //        //}
    //      //} while (lastIdx < rootIFList.size) 
    //      //println("--------")
    //      //--------
    //      //if (!isTop) {
    //      //} else {
    //      //}
    //      //--------
    //      //--------
    //      return true
    //    }
    //    //func(
    //    //  outerIdx=currNodeWalkData.cnt, // 0
    //    //)
    //    //var cnt = rootIFList.size - 1
    //    //val cnt = 
    //    //currNodeWalkData.cnt
    //    while (currNodeWalkData.cnt >= 0) {
    //      if (
    //        func(outerIdx=currNodeWalkData.cnt)
    //      ) {
    //        currNodeWalkData.cnt -= 1
    //        finishedWalk = false
    //      } else {
    //        currNodeWalkData.cnt = -1
    //      }
    //    }
    //  }
    //    //}
    //    //case _ =>
    //  //didFirstWalk = true
    //}
    println(
      s"beginning `lastPasses()`"               
    )

    def lastPasses(graph: SvifGraph, mode: Int): Unit = {
      //globalScope.lock = false
      //val newName = globalScope.allocateName(mkNewName(graph.origDefinitionName, graph.count))
      //globalScope.lock = true
      if (mode == 0) {
        //var prevInterface: Interface = null
        globalScope.lock = false
        val newName = globalScope.allocateName(mkNewName(graph.origDefinitionName, graph.count))
        globalScope.lock = true
        //if (graph.origDefinitionName.contains("WrObjPipePayloadStage")) {
        //  println(
        //    s"newName stuff: "
        //    + s"${newName} "
        //  )
        //}
        def func(interface: Interface) {
          if (interface.origDefinitionName != graph.origDefinitionName) {
            println(
              s"eek! origDefinitionName not same: "
              + s"interface:${interface.origDefinitionName} "
              + s"graph:${graph.origDefinitionName}"
            )
          }
          assert(interface.origDefinitionName == graph.origDefinitionName)
          interface.setDefinitionName(name=newName)
        }
        for (interface <- graph.intfSet.view) {
          func(
            interface=interface,
            //idx=idx,
          )
          //prevInterface = interface
        }
      } else if (mode == 1) {
        svInterface += (
          graph.anyIntf.definitionName -> emitInterface(graph.anyIntf)
        )
      }
      if (
        //graph.children.size != 0
        graph.child != null
      ) {
        //assert(graph.children.size == 1)
        lastPasses(
          graph=(
            //graph.children(0)
            graph.child
          ),
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

package spinal.lib.tools

import spinal.core._
import spinal.lib.IMasterSlave
import java.io.{File, FileWriter}
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

class ElkEdge {
  var source, target, label = ""
  var isBus = 0
  var highlight = 100
}

case class ElkPort(name: String, highlight: Int)

class ElkNode {
  var labelName, typeName = ""
  var inPorts: mutable.Set[ElkPort] = mutable.Set()
  var outPorts: mutable.Set[ElkPort] = mutable.Set()
  var children: mutable.Set[ElkNode] = mutable.Set()
  var isMixed = false
  var highlight = 100
}

case class SignalHandler(signal: BaseType, isPort: Boolean) {

  var name = ""
  var isBus = 0
  var isInPort = true
  var className = ""

  private def judeInOut(dataParent: Data): Boolean = {
    dataParent match {
      case iMasterSlaveParent: IMasterSlave =>
        if (iMasterSlaveParent.isMasterInterface) false
        else true
      case _ =>
        if (dataParent.flatten.head.isOutput) false
        else true
    }

  }

  private def getParameter: Unit = {
    var haveDataParent = false
    val parentList = signal.getRefOwnersChain()
    if (parentList.nonEmpty) {
      breakable {
        for (anyParent <- parentList) {
          anyParent match {
            case dataParent: Data =>
              if (dataParent.getClass.getSimpleName != "") {
                haveDataParent = true
                isBus = 1
                className = dataParent.getClass.getSimpleName
                isInPort = judeInOut(dataParent)
                if (dataParent.getName() != "") name = dataParent.getName()
                else name = "_zz_(" + dataParent.getClass.getSimpleName + ")"
                break()
              }
            case areaParent: Area =>
              haveDataParent = true
              isBus = 2
              name = areaParent.getName()
            case _ =>
          }
        }
      }
    }

    if (!haveDataParent) {
      if (isPort) {
        name = signal.getName()
        if (signal.isOutput) isInPort = false
      } else {
        if (parentList.nonEmpty && parentList.size > 1 && parentList.last.getClass.getSimpleName == "" && !parentList.last
          .isInstanceOf[Data]) {
          val anyParent = signal.getRefOwnersChain().last
          name = anyParent.toString.split("/").last
        } else {
          name = signal.getName()
          if (signal.isOutput) isInPort = false
        }
      }
    }
  }
  getParameter
}

case class ModuleData(module: Component,
                      moduleName: String,
                      clkMap: mutable.HashMap[ClockDomain, Int]) {
  var edges: mutable.Set[ElkEdge] = mutable.Set()
  var topNode = new ElkNode
  val clkResetName: mutable.Set[String] = mutable.Set()
  val legendClkMap = new mutable.HashMap[ClockDomain, Int]

  val moduleAnalyze = new ModuleAnalyzer(module)
  val topInOuts
  : mutable.LinkedHashSet[BaseType] = moduleAnalyze.getInputs ++ moduleAnalyze.getOutputs
  val allRegisters: mutable.LinkedHashSet[BaseType] = moduleAnalyze.getNets(
    net =>
      net.getComponent().getName() == module.getName() && !topInOuts.contains(
        net))
  val allInOuts: mutable.LinkedHashSet[BaseType] =
    moduleAnalyze.getPins(_ => true)
  val allNets: mutable.LinkedHashSet[BaseType] = allRegisters ++ allInOuts
  private val containedNode: mutable.Set[String] = mutable.Set()
  private val allClk: mutable.LinkedHashSet[ClockDomain] =
    moduleAnalyze.getClocks
  init()

  private def init(): Unit = {
    topNode.labelName = moduleName
    if (module.isInstanceOf[BlackBox]) topNode.typeName = "BlackBox"

    if (allClk.size > 1) {
      topNode.highlight = 10
      topNode.isMixed = true
    } else if (allClk.size == 1)
      topNode.highlight = clkMap(allClk.head)

    for (clk <- clkMap) {
      clkResetName.add(clk._1.clock.getName())
      if (clk._1.reset != null)
        clkResetName.add(clk._1.reset.getName())
      if (clk._1.softReset != null)
        clkResetName.add(clk._1.softReset.getName())
    }
  }

  def addNode(newNode: ElkNode): Unit = {
    if (!containedNode.contains(newNode.labelName) && newNode.labelName != "") {
      topNode.children.add(newNode)
      containedNode.add(newNode.labelName)
    }
  }
  def addEdge(newEdge: ElkEdge): Unit = {
    def edgeIsContained(): Boolean = {
      for (thisEdge <- edges) {
        if (thisEdge.source == newEdge.source && thisEdge.target == newEdge.target && thisEdge.label == newEdge.label) {
          return true
        }
      }
      false
    }
    if (!edgeIsContained() && newEdge.source != "" && newEdge.target != "" && newEdge.source != newEdge.target)
      edges.add(newEdge)
  }
}

class NodesHandler(thisModuleData: ModuleData) {

  private def findPortHighlight(thisPort: BaseType): Int =
    thisModuleData.clkMap.getOrElse(thisPort.clockDomain, 1)

  private def handleTopModule(): Unit = {

    /** Adding the topLevel module and its input/output ports */
    for (topInOut <- thisModuleData.topInOuts) {
      val dealtSignal = SignalHandler(topInOut, isPort = true)
      if (!thisModuleData.clkResetName.contains(dealtSignal.name)) {
        if (dealtSignal.isInPort)
          thisModuleData.topNode.inPorts
            .add(ElkPort(dealtSignal.name, findPortHighlight(topInOut)))
        else
          thisModuleData.topNode.outPorts
            .add(ElkPort(dealtSignal.name, findPortHighlight(topInOut)))
        if (findPortHighlight(topInOut) != 1 && !thisModuleData.legendClkMap
          .contains(topInOut.clockDomain))
          thisModuleData.legendClkMap
            .put(topInOut.clockDomain, findPortHighlight(topInOut))
      }
    }
  }

  private def handleInnerModules(): Unit = {

    /** Adding internal sub-modules and their input/output ports */
    val innerModules = thisModuleData.module.children
    for (innerModule <- innerModules) {
      val newNode = new ElkNode
      newNode.labelName = innerModule.getName()
      if (innerModule.isInstanceOf[BlackBox]) newNode.typeName = "BlackBox"
      val clocks = new ModuleAnalyzer(innerModule).getClocks
      if (clocks.nonEmpty) {
        if (clocks.size == 1) {
          newNode.highlight = thisModuleData.clkMap(clocks.head)
          if (!thisModuleData.legendClkMap.contains(clocks.head))
            thisModuleData.legendClkMap.put(clocks.head, newNode.highlight)
        } else
          newNode.highlight = 10
      }
      val innerModuleAnalyzer = new ModuleAnalyzer(innerModule)
      val innerInOuts = innerModuleAnalyzer.getInputs ++ innerModuleAnalyzer.getOutputs
      for (innerInOut <- innerInOuts) {
        val dealtSignal = SignalHandler(innerInOut, isPort = true)
        if (!thisModuleData.clkResetName.contains(dealtSignal.name)) {
          if (dealtSignal.isInPort)
            newNode.inPorts.add(
              ElkPort(dealtSignal.name, findPortHighlight(innerInOut)))
          else
            newNode.outPorts.add(
              ElkPort(dealtSignal.name, findPortHighlight(innerInOut)))
          if (findPortHighlight(innerInOut) != 1 && !thisModuleData.legendClkMap
            .contains(innerInOut.clockDomain))
            thisModuleData.legendClkMap
              .put(innerInOut.clockDomain, findPortHighlight(innerInOut))
        }

      }
      thisModuleData.addNode(newNode)
    }
  }

  private def handleTopLevelRegs(): Unit = {
    for (topLevelRegister <- thisModuleData.allRegisters) {
      val newNode = new ElkNode
      val dealtSignal = SignalHandler(topLevelRegister, isPort = false)
      newNode.labelName = dealtSignal.name
      val clkDomain = topLevelRegister.clockDomain
      if (thisModuleData.clkMap.contains(clkDomain)) {
        newNode.highlight = thisModuleData.clkMap(clkDomain)
        if (!thisModuleData.legendClkMap.contains(clkDomain))
          thisModuleData.legendClkMap
            .put(clkDomain, thisModuleData.clkMap(clkDomain))
      }
      thisModuleData.addNode(newNode)
    }
  }

  def getModuleData: ModuleData = {
    handleTopModule()
    handleInnerModules()
    handleTopLevelRegs()
    thisModuleData
  }
}

class EdgesHandler(thisModuleData: ModuleData) {
  private def signalComponentIsRight(signal: BaseType): Boolean = {
    for (thisNode <- thisModuleData.topNode.children)
      if (thisNode.labelName == signal.getComponent().getName()) return true
    false
  }

  private def getStartName(StartNode: BaseType): (String, Int, Boolean) = {
    var startName = ""
    var sonName = ""
    var startIsBus = 0
    var startIsWrong = false

    if (thisModuleData.allInOuts.contains(StartNode)) {
      val dealtSignal = SignalHandler(StartNode, isPort = true)
      sonName = dealtSignal.name
      startIsBus = dealtSignal.isBus
      if ((thisModuleData.topInOuts.contains(StartNode) && !dealtSignal.isInPort) || (!thisModuleData.topInOuts
        .contains(StartNode) && dealtSignal.isInPort) || thisModuleData.clkResetName
        .contains(dealtSignal.name)) startIsWrong = true
    } else {
      val dealtSignal = SignalHandler(StartNode, isPort = false)
      startIsBus = dealtSignal.isBus
      sonName = dealtSignal.name
    }

    /** Getting the source name */
    if (thisModuleData.allInOuts.contains(StartNode)) {
      if (thisModuleData.topInOuts.contains(StartNode))
        startName = thisModuleData.moduleName + "." + sonName
      else {
        if (signalComponentIsRight(StartNode))
          startName = StartNode.getComponent().getName() + "." + sonName
        else startIsWrong = true
      }
    } else if (thisModuleData.allRegisters.contains(StartNode))
      startName = sonName
    else startIsWrong = true

    def handleStraightConnect(): Unit = {

      /** Handling connections from input directly to output */
      val dataAnalyze = new DataAnalyzer(StartNode)
      val fanOuts = dataAnalyze.getFanOut
      breakable {
        for (fanOut <- fanOuts) {
          if (thisModuleData.topInOuts.contains(StartNode) && thisModuleData.topInOuts.contains(
            fanOut) && SignalHandler(StartNode, isPort = true).isInPort && !SignalHandler(
            fanOut,
            isPort = true).isInPort) {
            val newNode = new ElkNode
            val newEdge = new ElkEdge
            if (thisModuleData.clkMap.contains(StartNode.clockDomain))
              newEdge.highlight = thisModuleData.clkMap(StartNode.clockDomain)
            newNode.labelName = sonName
            newNode.highlight = thisModuleData.topNode.highlight
            if (startIsBus == 1)
              newEdge.label = SignalHandler(StartNode, isPort = true).className
            thisModuleData.addNode(newNode)
            newEdge.isBus = startIsBus
            newEdge.source = startName
            newEdge.target = sonName
            if (!startIsWrong)
              thisModuleData.addEdge(newEdge)
            startName = sonName
            break()
          }
        }
      }
    }
    handleStraightConnect()
    (startName, startIsBus, startIsWrong)
  }

  private def getEndName(endNode: BaseType,
                         startIsBus: Int,
                         startNode: BaseType): (String, Int, Boolean) = {

    /** Getting the sonName of fanOut */
    var endName = ""
    var fanOutSonName = ""
    var endIsBus = 0

    var endIsWrong = false
    if (thisModuleData.clkResetName.contains(endNode.getName()))
      endIsWrong = true
    if (thisModuleData.allInOuts.contains(endNode)) {
      val dealtSignal = SignalHandler(endNode, isPort = true)
      fanOutSonName = dealtSignal.name
      endIsBus = dealtSignal.isBus
      if ((thisModuleData.topInOuts.contains(endNode) && dealtSignal.isInPort) || (!thisModuleData.topInOuts
        .contains(endNode) && !dealtSignal.isInPort) || thisModuleData.clkResetName
        .contains(dealtSignal.name))
        endIsWrong = true
    } else {
      val dealtSignal = SignalHandler(endNode, isPort = false)
      endIsBus = dealtSignal.isBus
      fanOutSonName = dealtSignal.name
    }

    /** Getting the target name */
    if (thisModuleData.allInOuts.contains(endNode)) {
      if (thisModuleData.topInOuts.contains(endNode)) {
        if (!endIsWrong && endNode.isReg) {
          val extraEdge = new ElkEdge
          extraEdge.source = fanOutSonName
          extraEdge.target = thisModuleData.moduleName + "." + fanOutSonName
          extraEdge.isBus = startIsBus & endIsBus
          thisModuleData.addEdge(extraEdge)
          endName = fanOutSonName
          val extraNode = new ElkNode
          extraNode.labelName = fanOutSonName
          extraNode.highlight = thisModuleData.clkMap(endNode.clockDomain)
          thisModuleData.addNode(extraNode)
        } else
          endName = thisModuleData.moduleName + "." + fanOutSonName
      } else {
        if (endNode.getComponent() == startNode.getComponent())
          endIsWrong = true
        if (signalComponentIsRight(endNode))
          endName = endNode
            .getComponent()
            .getName() + "." + fanOutSonName
        else
          endIsWrong = true
      }
    } else if (thisModuleData.allRegisters.contains(endNode)) {
      endName = fanOutSonName
    } else endIsWrong = true
    (endName, endIsBus, endIsWrong)
  }

  private def addNewEdge(): Unit = {
    for (startNode <- thisModuleData.allNets) {
      val (startName, startIsBus, startNodeIsWrong) = getStartName(startNode)
      if (!startNodeIsWrong) {
        val fanOuts = new DataAnalyzer(startNode).getFanOut
        for (endNode <- fanOuts) {
          val (endName, endIsBus, endNodeIsWrong) =
            getEndName(endNode, startIsBus, startNode)
          if (!endNodeIsWrong) {
            val newEdge = new ElkEdge
            newEdge.source = startName
            newEdge.target = endName
            if (startIsBus == 1 && endIsBus == 1)
              newEdge.isBus = 1
            if (thisModuleData.clkMap.contains(startNode.clockDomain))
              newEdge.highlight = thisModuleData.clkMap(startNode.clockDomain)
            if (!thisModuleData.clkResetName.contains(startNode.getName()) && !thisModuleData.clkResetName
              .contains(endNode.getName())) {
              if (newEdge.isBus == 1) {
                val startClassName =
                  SignalHandler(startNode, isPort = true).className
                val endClassName =
                  SignalHandler(endNode, isPort = true).className
                if (startClassName == endClassName)
                  newEdge.label = startClassName
                else
                  newEdge.label = startClassName + " to " + endClassName
              } else if (startIsBus > 1)
                newEdge.label = startNode.getName()
              else if (endIsBus > 1)
                newEdge.label = endNode.getName()
            }
            thisModuleData.addEdge(newEdge)
          }
        }
      }
    }
  }
  def getModuleData: ModuleData = {
    addNewEdge()
    thisModuleData
  }

}

class HTMLGenerator(writer: FileWriter) {
  def genHTMLInit(topLevelName: String, module: Module): Unit = {
    writer.write(
      s"""<!DOCTYPE html>
         |<html>
         |<head>
         |    <meta charset="UTF-8">
         |    <title> RTL diagrams of $topLevelName</title>
         |    <style>
         |.buttons-container {
         | display: flex;
         | justify-content: center;
         | margin-top:30px;
         |}
         |
         |.buttons-container button {
         | background-color: #4CAF50;
         | color: white;
         | border: none;
         | padding:10px20px;
         | margin:010px;
         | cursor: pointer;
         | border-radius:5px;
         |}
         |
         |.center-title {
         |  text-align: center;
         |  font-size:24px;
         |  font-weight: bold;
         |  margin-top:20px;
         | margin-bottom: 20px;
         |}
         |.goTop {
         |	width: 50px;
         |	height: 50px;
         |	background-color: aquamarine;
         |	font-size: 20px;
         |	text-align: center;
         |	line-height: 25px;
         |	color: azure;
         |	position: fixed;
         |	bottom: 50px;
         |	right: 50px;
         |	display: none;
         |}
         |    </style>
         |</head>
         |<body>
         |<button class="goTop" id="gotop">Go Top</button>
         |<script>
         |	var goTop=document.getElementById("gotop")
         |	    window.onscroll=function(){
         |		var jhlheight=document.documentElement.scrollTop||document.body.scrollTop
         |		if(jhlheight>=300){
         |		goTop.style.display="block"
         |		}else{
         |		goTop.style.display="none"
         |   }
         |	}
         |	  goTop.onclick=function(){
         |		window.scrollTo({
         |			top:0,
         |			behavior:"smooth"
         |		})
         |	}
         |</script>
         |<script src="https://cdn.jsdelivr.net/gh/Readon/hdelk@master/js/elk.bundled.js"></script>
         |<script src="https://cdn.jsdelivr.net/gh/Readon/hdelk@master/js/svg.min.js"></script>
         |<script src="https://cdn.jsdelivr.net/gh/Readon/hdelk@master/js/hdelk.js"></script>
         |
         |<h1 class="center-title">choose diagrams</h1>
         |<div class="buttons-container">
         |<a href="#$topLevelName"><button>$topLevelName</button></a>&nbsp;
         |""".stripMargin
    )
    val allInnerCells = module.children
    for (cell <- allInnerCells) {
      writer.write(s"""<a href="#${cell.getName()}"><button>${cell
        .getName()}</button></a>&nbsp;\n""")
    }
    writer.write(s"""</div><br><br><br><br>\n""")
  }

  /** Generating HDElk language for Edge */
  private def genHTMLNodes(thisNode: ElkNode): Unit = {
    writer.write(s"""{id:"${thisNode.labelName}",\n""")
    if (thisNode.typeName != "")
      writer.write(s"""type:"${thisNode.typeName}",\n""")
    if (thisNode.highlight != 100)
      writer.write(s"""highlight:${thisNode.highlight},\n""")
    if (thisNode.inPorts.nonEmpty) {
      writer.write(s"""inPorts: [""")
      for (inPort <- thisNode.inPorts) {
        if (inPort.highlight != 1)
          writer.write(
            s"""{id:"${inPort.name}",highlight:${inPort.highlight}},""")
        else writer.write(s""""${inPort.name}",""")
      }
      writer.write(s"""],\n""")
    }
    if (thisNode.outPorts.nonEmpty) {
      writer.write(s"""outPorts: [""")
      for (outPort <- thisNode.outPorts) {
        if (outPort.highlight != 1)
          writer.write(
            s"""{id:"${outPort.name}",highlight:${outPort.highlight}},""")
        else writer.write(s""""${outPort.name}",""")
      }
      writer.write(s"""],\n""")
    }
    if (thisNode.children.nonEmpty) {
      writer.write(s"""children: [\n""")
      for (thisChildren <- thisNode.children) {
        genHTMLNodes(thisChildren)
        writer.write(s"""},\n""")
      }
      writer.write(s"""],\n""")
    }
  }

  private def genHTMLEdges(edges: mutable.Set[ElkEdge]): Unit = {
    writer.write(s"""edges:[\n""")
    for (edge <- edges) {
      writer.write(
        s"""{ source:"${edge.source}",target:"${edge.target}",bus:${edge.isBus},""")
      if (edge.label != "")
        writer.write(s"""label:"${edge.label}",""")
      if (edge.highlight != 100)
        writer.write(s"""highlight:${edge.highlight}""")
      writer.write(s"""},\n""")
    }
    writer.write(s"""]\n},\n""")
  }

  /** Generating HDElk language for ClockDomains */
  private def genHTMLClockDomains(clkMap: mutable.HashMap[ClockDomain, Int],
                                  isMixed: Boolean): Unit = {
    writer.write(s"""{id:"ClockDomains",\nchildren:[\n""")
    for (element <- clkMap) {
      writer.write(
        s"""{id:"${element._1}",highlight:${element._2},inPorts:[ "In" ],outPorts:[ "Out" ]},\n""")
    }
    if (isMixed)
      writer.write(
        s"""{id:"mixedClk",highlight:10,inPorts:[ "In" ],outPorts:[ "Out" ]},\n""")
    writer.write(s"""]\n}\n""")
  }

  /** Integrating all methods to draw an image */
  def genHTMLOneModule(thisModuleData: ModuleData): Unit = {
    writer.write(
      s"""
         |<div id="${thisModuleData.topNode.labelName}"></div>
         |<h3>${thisModuleData.topNode.labelName}</h3><br><br><br><br>
         |<script type="text/javascript">
         |
         |var mygraph = {
         |children:[
         |""".stripMargin
    )
    genHTMLNodes(thisModuleData.topNode)
    genHTMLEdges(thisModuleData.edges)
    if (thisModuleData.legendClkMap.nonEmpty || thisModuleData.topNode.isMixed)
      genHTMLClockDomains(thisModuleData.legendClkMap,
        thisModuleData.topNode.isMixed)
    writer.write(
      s"""],\n}\nhdelk.layout( mygraph,"${thisModuleData.topNode.labelName}");\n</script>\n""")
  }

}

object HDElkDiagramGen {

  private def drawAllModule(module: Component,
                            topLevelName: String,
                            clkMap: mutable.HashMap[ClockDomain, Int]): Unit = {
    val fileName = topLevelName + ".html"
    val file = new File(fileName)
    val writer = new FileWriter(file)
    val generator = new HTMLGenerator(writer)
    generator.genHTMLInit(topLevelName, module)
    generator.genHTMLOneModule(getOneModuleData(module, topLevelName, clkMap))
    for (innerModule <- module.children)
      generator.genHTMLOneModule(
        getOneModuleData(innerModule, innerModule.getName(), clkMap))
    writer.write(s"""</body>\n</html>""")
    writer.close()
  }

  private def getOneModuleData(
                                module: Component,
                                topLevelName: String,
                                clkMap: mutable.HashMap[ClockDomain, Int]): ModuleData = {
    var thisModuleData = ModuleData(module, topLevelName, clkMap)
    thisModuleData = new NodesHandler(thisModuleData).getModuleData
    thisModuleData = new EdgesHandler(thisModuleData).getModuleData
    thisModuleData
  }

  /** Generating all diagrams */
  def apply[T <: Component](rtl: SpinalReport[T]): Unit = {
    val module = rtl.toplevel
    val topLevelName = rtl.toplevelName
    val clkMap = new mutable.HashMap[ClockDomain, Int]

    val allClk = new ModuleAnalyzer(module).getClocks
    for (thisClk <- allClk) {
      if (!clkMap.contains(thisClk))
        clkMap.put(thisClk, clkMap.size + 2)
    }

    drawAllModule(module, topLevelName, clkMap)
  }
}

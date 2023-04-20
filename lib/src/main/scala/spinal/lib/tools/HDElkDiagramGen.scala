package spinal.lib.tools

import spinal.core._
import spinal.lib.IMasterSlave
import spinal.lib.tools._
import java.io.{File, FileWriter}
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

class ElkEdge {
  var source, target, label = ""
  var isBus, highlight = 100
}

case class ElkPort(name: String, highlight: Int)

class ElkNode {
  var labelName, typeName = ""
  var inPorts: mutable.Set[ElkPort] = mutable.Set()
  var outPorts: mutable.Set[ElkPort] = mutable.Set()
  var children: mutable.Set[ElkNode] = mutable.Set()
  var highlight = 100

  /** Generating HDElk language for Node */
  def drawNodes(writer: FileWriter): Unit = {
    writer.write(s"""{id:"$labelName",\n""")
    if (typeName != "")
      writer.write(s"""type:"$typeName",\n""")
    if (highlight != 100)
      writer.write(s"""highlight:$highlight,\n""")
    if (inPorts.nonEmpty) {
      writer.write(s"""inPorts: [""")
      for (inPort <- inPorts) {
        if (inPort.highlight != 1)
          writer.write(
            s"""{id:"${inPort.name}",highlight:${inPort.highlight}},""")
        else writer.write(s""""${inPort.name}",""")
      }
      writer.write(s"""],\n""")
    }
    if (outPorts.nonEmpty) {
      writer.write(s"""outPorts: [""")
      for (outPort <- outPorts) {
        if (outPort.highlight != 1)
          writer.write(
            s"""{id:"${outPort.name}",highlight:${outPort.highlight}},""")
        else writer.write(s""""${outPort.name}",""")
      }
      writer.write(s"""],\n""")
    }
    if (children.nonEmpty) {
      writer.write(s"""children: [\n""")
      for (thisChildren <- children) {
        thisChildren.drawNodes(writer)
        writer.write(s"""},\n""")
      }
      writer.write(s"""],\n""")
    }
  }
}

case class DealSignal(signal: BaseType, isPort: Boolean) {

  var name = ""
  var isBus = 0
  var isInPort = true
  var className = ""

  private def judeInOut(dataParent: Data): Unit = {
    dataParent match {
      case iMasterSlaveParent: IMasterSlave =>
        if (iMasterSlaveParent.isMasterInterface) isInPort = false
      case _ =>
        if (dataParent.flatten.head.isOutput) isInPort = false
    }
  }

  private var judge = false
  private val parentList = signal.getRefOwnersChain()
  if (parentList.nonEmpty) {
    breakable {
      for (anyParent <- parentList) {
        anyParent match {
          case dataParent: Data =>
            if (dataParent.getClass.getSimpleName != "") {
              judge = true
              isBus = 1
              className = dataParent.getClass.getSimpleName
              judeInOut(dataParent)
              if (dataParent.getName() != "") name = dataParent.getName()
              else name = "_zz_(" + dataParent.getClass.getSimpleName + ")"
              break()
            }
          case _ =>
        }
      }
    }
  }
  if (!judge) {
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

class GenerateOneDiagram(module: Component,
                         moduleName: String,
                         writer: FileWriter,
                         clkMap: mutable.HashMap[String, Int]) {

  private val edges: mutable.Set[ElkEdge] = mutable.Set()
  private val topNode = new ElkNode
  topNode.labelName = moduleName
  if (module.isInstanceOf[BlackBox]) topNode.typeName = "BlackBox"
  private val containedNode: mutable.Set[String] = mutable.Set()
  private val moduleAnalyze = new ModuleAnalyzer(module)
  private val allClk = moduleAnalyze.getClocks
  if (allClk.size > 1) topNode.highlight = 10
  else if (allClk.size == 1)
    topNode.highlight = clkMap(allClk.head.clock.getName())
  private val topInOuts = moduleAnalyze.getInputs ++ moduleAnalyze.getOutputs

  /** Distinguishing system registers */
  private val allRegisters = moduleAnalyze.getRegisters
  private val systemRegisters = moduleAnalyze.getNets(
    net =>
      net.getComponent().getName() == module.getName() && !topInOuts.contains(
        net) && !allRegisters.contains(net))
  private val allInOuts = moduleAnalyze.getPins(_ => true)
  private val everyRegisters = allRegisters ++ systemRegisters

  private def GenAllNodes(): Unit = {
    def findPortHighlight(thisPort: BaseType): Int = {
      (clkMap.get(thisPort.getName()) orElse clkMap.get(
        thisPort.clockDomain.toString())).getOrElse(1)
    }

    /** Adding the toplevel module and its input/output ports */
    for (topInOut <- topInOuts) {
      val dealtSignal = DealSignal(topInOut, isPort = true)
      if (dealtSignal.isInPort)
        topNode.inPorts.add(
          ElkPort(dealtSignal.name, findPortHighlight(topInOut)))
      else
        topNode.outPorts.add(
          ElkPort(dealtSignal.name, findPortHighlight(topInOut)))
    }

    /** Adding internal sub-modules and their input/output ports */
    val innerModules = module.children
    for (innerModule <- innerModules) {
      val newNode = new ElkNode
      newNode.labelName = innerModule.getName()
      if (innerModule.isInstanceOf[BlackBox]) newNode.typeName = "BlackBox"
      val clocks = new ModuleAnalyzer(innerModule).getClocks
      if (clocks.nonEmpty)
        newNode.highlight = clkMap(clocks.head.clock.getName())
      val innerModuleAna = new ModuleAnalyzer(innerModule)
      val innerInOuts = innerModuleAna.getInputs ++ innerModuleAna.getOutputs
      for (innerInOut <- innerInOuts) {
        val dealtSignal = DealSignal(innerInOut, isPort = true)
        if (dealtSignal.isInPort)
          newNode.inPorts.add(
            ElkPort(dealtSignal.name, findPortHighlight(innerInOut)))
        else
          newNode.outPorts.add(
            ElkPort(dealtSignal.name, findPortHighlight(innerInOut)))
      }
      topNode.children.add(newNode)
    }

    /** Adding the registers of the toplevel module */
    for (topLevelRegister <- everyRegisters) {
      val newNode = new ElkNode
      val dealtSignal = DealSignal(topLevelRegister, isPort = false)
      newNode.labelName = dealtSignal.name
      if (systemRegisters.contains(topLevelRegister)) newNode.highlight = 0
      else if (clkMap.contains(topLevelRegister.clockDomain.clock.getName()))
        newNode.highlight = clkMap(topLevelRegister.clockDomain.clock.getName())
      if (!containedNode.contains(newNode.labelName)) {
        topNode.children.add(newNode)
        containedNode.add(newNode.labelName)
      }
    }
  }

  private def GenAllEdges(): Unit = {
    def edgeIsContained(newEdge: ElkEdge): Boolean = {
      for (thisEdge <- edges) {
        if (thisEdge.source == newEdge.source && thisEdge.target == newEdge.target && thisEdge.label == newEdge.label)
          return true
      }
      false
    }

    def componentIsRight(signal: BaseType): Boolean = {
      for (thisNode <- topNode.children)
        if (thisNode.labelName == signal.getComponent().getName()) return true
      false
    }

    /** Getting the sonName of net */
    val allNets = everyRegisters ++ allInOuts
    for (net <- allNets) {
      var sourceName = ""
      var sonName = ""
      var inIsBus = 0
      var netIsWrong = false
      if (allInOuts.contains(net)) {
        val dealtSignal = DealSignal(net, isPort = true)
        sonName = dealtSignal.name
        if ((topInOuts.contains(net) && !dealtSignal.isInPort) || (!topInOuts
          .contains(net) && dealtSignal.isInPort)) netIsWrong = true
        if (!clkMap.contains(net.getName()))
          inIsBus = dealtSignal.isBus
      } else {
        val dealtSignal = DealSignal(net, isPort = false)
        inIsBus = dealtSignal.isBus
        sonName = dealtSignal.name
      }

      /** Getting the source name */
      if (allInOuts.contains(net)) {
        if (topInOuts.contains(net)) sourceName = moduleName + "." + sonName
        else {
          if (componentIsRight(net))
            sourceName = net.getComponent().getName() + "." + sonName
          else netIsWrong = true
        }
      } else if (everyRegisters.contains(net)) sourceName = sonName
      else netIsWrong = true

      /** Handling connections from input directly to output */
      val dataAnalyze = new DataAnalyzer(net)
      val fanOuts = dataAnalyze.getFanOut
      breakable {
        for (fanOut <- fanOuts) {
          if (topInOuts.contains(net) && topInOuts.contains(fanOut) && DealSignal(
            net,
            isPort = true).isInPort && !DealSignal(fanOut, isPort = true).isInPort) {
            val newNode = new ElkNode
            val newEdge = new ElkEdge
            if (clkMap.contains(net.clockDomain.clock.getName()))
              newEdge.highlight = clkMap(net.clockDomain.clock.getName())
            newNode.labelName = sonName
            newNode.highlight = topNode.highlight
            if (inIsBus == 1)
              newEdge.label = DealSignal(net, isPort = true).className
            if (!containedNode.contains(sonName) && !netIsWrong) {
              topNode.children.add(newNode)
              containedNode.add(sonName)
            }
            newEdge.isBus = inIsBus
            newEdge.source = sourceName
            newEdge.target = sonName
            if (!netIsWrong && !edgeIsContained(newEdge))
              edges.add(newEdge)
            sourceName = sonName
            break()
          }
        }
      }

      /** Getting the sonName of fanOut */
      for (fanOut <- fanOuts) {
        val newEdge = new ElkEdge
        var fanOutSonName = ""
        var outIsBus = 0
        var fanOutIsWrong = false
        if (allInOuts.contains(fanOut)) {
          val dealtSignal = DealSignal(fanOut, isPort = true)
          fanOutSonName = dealtSignal.name
          if ((topInOuts.contains(fanOut) && dealtSignal.isInPort) || (!topInOuts
            .contains(fanOut) && !dealtSignal.isInPort))
            fanOutIsWrong = true
          if (!clkMap.contains(fanOut.getName()))
            outIsBus = dealtSignal.isBus
        } else {
          val dealtSignal = DealSignal(fanOut, isPort = false)
          outIsBus = dealtSignal.isBus
          fanOutSonName = dealtSignal.name
        }

        /** Getting the target name */
        if (allInOuts.contains(fanOut)) {
          if (topInOuts.contains(fanOut)) {
            if (!netIsWrong && !fanOutIsWrong && fanOut.isReg) {
              val extraEdge = new ElkEdge
              extraEdge.source = fanOutSonName
              extraEdge.target = moduleName + "." + fanOutSonName
              extraEdge.isBus = inIsBus & outIsBus
              if (!edgeIsContained(extraEdge))
                edges.add(extraEdge)
              newEdge.target = fanOutSonName
            } else
              newEdge.target = moduleName + "." + fanOutSonName
          } else {
            if (fanOut.getComponent() == net.getComponent())
              fanOutIsWrong = true
            if (componentIsRight(fanOut))
              newEdge.target = fanOut
                .getComponent()
                .getName() + "." + fanOutSonName
            else
              fanOutIsWrong = true
          }
        } else if (everyRegisters.contains(fanOut)) {
          newEdge.target = fanOutSonName
        } else fanOutIsWrong = true

        /** Setting and adding Edge */
        if (clkMap.contains(net.getName()))
          newEdge.highlight = clkMap(net.getName())
        else if (systemRegisters.contains(net)) newEdge.highlight = 0
        else if (clkMap.contains(net.clockDomain.clock.getName()))
          newEdge.highlight = clkMap(net.clockDomain.clock.getName())
        newEdge.source = sourceName
        newEdge.isBus = inIsBus & outIsBus
        if (newEdge.isBus == 1) {
          if (DealSignal(net, isPort = true).className == DealSignal(
            fanOut,
            isPort = true).className)
            newEdge.label =
              DealSignal(net, isPort = true).className.getClass.getSimpleName
          else
            newEdge.label = DealSignal(net, isPort = true).className + " to " + DealSignal(
              fanOut,
              isPort = true).className
        } else if (inIsBus == 1) {
          if (!clkMap.contains(net.getName()))
            newEdge.label = net.getName()
        } else if (outIsBus == 1) {
          if (!clkMap.contains(fanOut.getName()))
            newEdge.label = fanOut.getName()
        }
        if (!netIsWrong && !fanOutIsWrong && !edgeIsContained(newEdge) && newEdge.source != "" && newEdge.target != "")
          edges.add(newEdge)
      }
    }
  }

  /** Generating HDElk language for Edge */
  private def drawEdges(): Unit = {
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
  private def drawClockDomains(clkMap: mutable.HashMap[String, Int]): Unit = {
    writer.write(s"""{id:"ClockDomains",\nchildren:[\n""")
    for (element <- clkMap) {
      writer.write(s"""{id:"${element._1}",highlight:${element._2}},\n""")
    }
    writer.write(s"""]\n}\n""")
  }

  /** Integrating all methods to draw an image */
  def beginDraw(): Unit = {
    writer.write(
      s"""
         |<div id="${topNode.labelName}"></div>
         |<h3>${topNode.labelName}</h3><br><br><br><br>
         |<script type="text/javascript">
         |
         |var mygraph = {
         |children:[
         |""".stripMargin
    )
    GenAllNodes()
    GenAllEdges()
    topNode.drawNodes(writer)
    drawEdges()
    if (allClk.nonEmpty) {
      val thisClkMap = new mutable.HashMap[String, Int]
      for (clk <- allClk)
        if (clkMap.contains(clk.clock.getName()))
          thisClkMap.put(clk.clock.getName(), clkMap(clk.clock.getName()))
      drawClockDomains(thisClkMap)
    }
    writer.write(
      s"""],\n}\nhdelk.layout( mygraph,"${topNode.labelName}");\n</script>\n""")
  }
}

object HDElkDiagramGen {

  /** Generating all diagrams */
  def apply[T <: Component](rtl: SpinalReport[T]): Unit = {
    val fileName = rtl.toplevelName + ".html"
    val file = new File(fileName)
    val writer = new FileWriter(file)
    writer.write(
      s"""<!DOCTYPE html>
         |<html>
         |<head>
         |    <meta charset="UTF-8">
         |    <title> RTL diagrams of ${rtl.toplevelName}</title>
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
         |<a href="#${rtl.toplevelName}"><button>${rtl.toplevelName}</button></a>&nbsp;
         |""".stripMargin
    )
    val clkMap = new mutable.HashMap[String, Int]
    val module = rtl.toplevel
    val moduleAnalyze = new ModuleAnalyzer(module)
    val allClk = moduleAnalyze.getClocks
    var clkCounter = 1
    for (thisClk <- allClk) {
      clkCounter = clkCounter + 1
      if (!clkMap.contains(thisClk.clock.getName())) {
        clkMap.put(thisClk.clock.getName(), clkCounter)
        if (thisClk.reset != null)
          clkMap.put(thisClk.reset.getName(), clkCounter)
        if (thisClk.softReset != null)
          clkMap.put(thisClk.softReset.getName(), clkCounter)
      }
    }
    val allInnerCells = module.children
    for (cell <- allInnerCells) {
      writer.write(s"""<a href="#${cell.getName()}"><button>${cell
        .getName()}</button></a>&nbsp;\n""")
    }
    writer.write(s"""</div><br><br><br><br>\n""")
    new GenerateOneDiagram(module, rtl.toplevelName, writer, clkMap)
      .beginDraw()
    for (inner <- module.children)
      new GenerateOneDiagram(inner, inner.getName(), writer, clkMap)
        .beginDraw()
    writer.write(s"""</body>\n</html>""")
    writer.close()
  }
}

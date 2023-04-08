package GenerateDiagrams
import spinal.core._
import spinal.lib.tools._
import java.io.{File, FileWriter}
import scala.collection.mutable
import scala.util.control._

class ElkEdge {
  var source, target, label = ""
  var isBus, highlight = 0
}

class ElkNode {
  var labelName, typeName = ""
  var inPorts: mutable.Set[String] = mutable.Set()
  var outPorts: mutable.Set[String] = mutable.Set()
  var children: mutable.Set[ElkNode] = mutable.Set()
  var highlight = 0
}

class GenerateOneDiagram(module: Component, topLevelName: String, moduleName: String) extends OwnableRef {
  private val builder = new StringBuilder()
  private val edges: mutable.Set[ElkEdge] = mutable.Set()
  private val topNode = new ElkNode
  topNode.labelName = moduleName
  if (module.isInstanceOf[BlackBox]) topNode.typeName = "BlackBox"
  private val clkMap = new mutable.HashMap[String, Int]
  private val clkNamesMap = new mutable.HashMap[String, Int]
  private val containedNode: mutable.Set[String] = mutable.Set()
  /** Initializing data*/


  private def haveParent(thisSon: BaseType): Boolean = {
    var judge = false
    val parentList = thisSon.getRefOwnersChain()
    if (parentList.nonEmpty) {
      for (anyParent <- parentList) {
        anyParent match {
          case dataParent: Data =>
            if (dataParent.getClass.getSimpleName != "") judge = true
          case _ =>
        }
      }
    }
    judge
  }
  private def findParent(thisSon: BaseType): Data = {
    val parentList = thisSon.getRefOwnersChain()
    var returnParent = thisSon.parent
    val loop = new Breaks
    loop.breakable {
      for (anyParent <- parentList) {
        anyParent match {
          case dataParent: Data =>
            if (dataParent.getClass.getSimpleName != "") {
              if (dataParent.getName() != "") {
                returnParent = dataParent
                loop.break()
              }
              else {
                dataParent.setName("_zz_(" + dataParent.getClass.getSimpleName + ")")
                returnParent = dataParent
                loop.break()
              }
            }
          case _ =>
        }
      }
    }
    returnParent
  }
  private def haveRegParent(thisSon: BaseType): Boolean = {
    var judge = false
    val parentList = thisSon.getRefOwnersChain()
    if (parentList.nonEmpty && parentList.size > 1)
      if (parentList.last.getClass.getSimpleName == "" && !parentList.last.isInstanceOf[Data]) judge = true
    judge
  }
  private def findRegParent(thisSon: BaseType): String = {
    val anyParent = thisSon.getRefOwnersChain().last
    val parentName = anyParent.toString.split("/").last
    parentName
  }
/** Searching for the parent of BaseType*/


  private def dealAllSignals(): Unit = {
    val moduleAnalyze = new ModuleAnalyzer(module)
    val allClk = moduleAnalyze.getClocks
    var clkCounter = 1
    for (thisClk <- allClk) {
      if (!clkMap.contains(thisClk.clock.getName())) {
        clkCounter += 1
        clkNamesMap.put(thisClk.clock.getName(), clkCounter)
        clkMap.put(thisClk.clock.getName(), clkCounter)
        if (thisClk.reset != null)
          clkMap.put(thisClk.reset.getName(), clkCounter)
        if (thisClk.softReset != null)
          clkMap.put(thisClk.softReset.getName(), clkCounter)
      }
    }
    if (allClk.size > 1) topNode.highlight = 6
    else if (allClk.size == 1) topNode.highlight = clkMap(allClk.head.toString())
/** Generating the color mapping for clk*/


    val topInOuts = moduleAnalyze.getInputs ++ moduleAnalyze.getOutputs
    val allRegisters = moduleAnalyze.getRegisters
    val systemRegisters = moduleAnalyze.getNets(net => net.getComponent().getName() == module.getName() && !topInOuts.contains(net) && !allRegisters.contains(net))
    val allInOuts = moduleAnalyze.getPins(_ => true)
/** Distinguishing system registers*/


    for (topInOut <- topInOuts) {
      if (haveParent(topInOut)) {
        val rootParent = findParent(topInOut)
        if (rootParent.flatten.head.isInput) topNode.inPorts.add(rootParent.getName())
        else if (rootParent.flatten.head.isOutput) topNode.outPorts.add(rootParent.getName())
        else {
          if (topInOut.isInput) topNode.inPorts.add(rootParent.getName())
          else topNode.outPorts.add(rootParent.getName())
        }
      }
      else {
        if (topInOut.isInput) topNode.inPorts.add(topInOut.getName())
        else topNode.outPorts.add(topInOut.getName())
      }
    }
/** Adding the top-level module and its input/output ports*/


    val innerModules = module.children
    for (innerModule <- innerModules) {
      val newNode = new ElkNode
      newNode.labelName = innerModule.getName()
      if (innerModule.isInstanceOf[BlackBox]) newNode.typeName = "BlackBox"
      val clocks = new ModuleAnalyzer(innerModule).getClocks
      if (clocks.nonEmpty) newNode.highlight = clkMap(clocks.head.toString())
      val innerModuleAna = new ModuleAnalyzer(innerModule)
      val innerInOuts = innerModuleAna.getInputs ++ innerModuleAna.getOutputs
      for (innerInOut <- innerInOuts) {
        if (haveParent(innerInOut)) {
          val rootParent = findParent(innerInOut)
          if (rootParent.flatten.head.isInput) newNode.inPorts.add(rootParent.getName())
          else if (rootParent.flatten.head.isOutput) newNode.outPorts.add(rootParent.getName())
          else {
            if (innerInOut.isInput) newNode.inPorts.add(rootParent.getName())
            else newNode.outPorts.add(rootParent.getName())
          }
        }
        else {
          if (innerInOut.isInput) newNode.inPorts.add(innerInOut.getName())
          else newNode.outPorts.add(innerInOut.getName())
        }
      }
      topNode.children.add(newNode)
    }
/** Adding internal sub-modules and their input/output ports*/


    val everyRegisters = allRegisters ++ systemRegisters
    for (topLevelRegister <- everyRegisters) {
      val newNode = new ElkNode
      if (haveParent(topLevelRegister)) {
        newNode.labelName = findParent(topLevelRegister).getName()
      }
      else if (haveRegParent(topLevelRegister)) newNode.labelName = findRegParent(topLevelRegister)
      else newNode.labelName = topLevelRegister.getName()
      if (clkMap.contains(topLevelRegister.clockDomain.toString()))
        newNode.highlight = clkMap(topLevelRegister.clockDomain.toString())
      if (!containedNode.contains(newNode.labelName)) {
        topNode.children.add(newNode)
        containedNode.add(newNode.labelName)
      }
    }
 /** Adding the registers of the top-level module*/

    val allNets = everyRegisters ++ allInOuts
    for (net <- allNets) {
      var sourceName = ""
      var sonName = ""
      var inIsBus = 0
      var netIsWrong = false
      if (allInOuts.contains(net)) {
        if (haveParent(net)) {
          if (!clkMap.contains(net.getName()))
            inIsBus = 1
          val rootParent = findParent(net)
          if (topInOuts.contains(net)) {
            if (rootParent.flatten.head.isOutput) netIsWrong = true
          }
          else {
            if (rootParent.flatten.head.isInput) netIsWrong = true
          }
          sonName = rootParent.getName()
        }
        else {
          if (topInOuts.contains(net)) {
            if (net.isOutput) netIsWrong = true
          }
          else {
            if (net.isInput) netIsWrong = true
          }
          sonName = net.getName()
        }
      }
      else {
        if (haveParent(net)) {
          inIsBus = 1
          sonName = findParent(net).getName()
        }
        else if (haveRegParent(net)) sonName = findRegParent(net)
        else sonName = net.getName()
      }
/** Getting the sonName of net*/

      if (allInOuts.contains(net)) {
        if (topInOuts.contains(net)) sourceName = moduleName + "." + sonName
        else {
          var judge = false
          for (thisNode <- topNode.children) {
            if (thisNode.labelName == net.getComponent().getName()) {
              judge = true
              sourceName = net.getComponent().getName() + "." + sonName
            }
          }
          if (!judge) netIsWrong = true
        }
      }
      else if (everyRegisters.contains(net)) sourceName = sonName
      else netIsWrong = true
/** Getting the source name*/

      val dataAnalyze = new DataAnalyzer(net)
      val fanOuts = dataAnalyze.getFanOut
      val loop = new Breaks
      loop.breakable {
        for (fanOut <- fanOuts) {
          if (net.getComponent().getName() == module.getName() && fanOut.getComponent().getName() == module.getName() && net.isInput && fanOut.isOutput) {
            val newNode = new ElkNode
            val newEdge = new ElkEdge
            newNode.labelName = sonName
            newNode.highlight = topNode.highlight
            if (inIsBus == 1) newEdge.label = findParent(net).getClass.getSimpleName
            if (!containedNode.contains(sonName) && !netIsWrong) {
              topNode.children.add(newNode)
              containedNode.add(sonName)
            }
            newEdge.isBus = inIsBus
            newEdge.source = sourceName
            newEdge.target = sonName
            var isContained = false
            for (thisEdge <- edges) {
              if (thisEdge.source == newEdge.source && thisEdge.target == newEdge.target && thisEdge.label == newEdge.label)
                isContained = true
            }
            if (!netIsWrong && !isContained)
              edges.add(newEdge)
            sourceName = sonName
            loop.break()
          }
        }
      }
/** Handling connections from input directly to output*/

      for (fanOut <- fanOuts) {
        val newEdge = new ElkEdge
        var fanOutSonName = ""
        var outIsBus = 0
        var fanOutIsWrong = false
        if (allInOuts.contains(fanOut)) {
          if (haveParent(fanOut)) {
            outIsBus = 1
            val rootParent = findParent(fanOut)
            if (topInOuts.contains(fanOut)) {
              if (rootParent.flatten.head.isInput) fanOutIsWrong = true
            }
            else {
              if (rootParent.flatten.head.isOutput) fanOutIsWrong = true
            }
            fanOutSonName = rootParent.getName()
          }
          else {
            if (topInOuts.contains(fanOut)) if (fanOut.isInput) fanOutIsWrong = true
            else if (fanOut.isOutput) fanOutIsWrong = true
            fanOutSonName = fanOut.getName()
          }
        }
        else {
          if (haveParent(fanOut)) {
            outIsBus = 1
            fanOutSonName = findParent(fanOut).getName()
          }
          else if (haveRegParent(fanOut)) fanOutSonName = findRegParent(fanOut)
          else fanOutSonName = fanOut.getName()
        }
/** Getting the subname of fanOut*/


        if (allInOuts.contains(fanOut)) {
          if (topInOuts.contains(fanOut)) {
            newEdge.target = moduleName + "." + fanOutSonName
          }
          else if (fanOut.getComponent() == net.getComponent()) fanOutIsWrong = true
          else {
            var isOk = false
            for (thisNode <- topNode.children) {
              if (thisNode.labelName == fanOut.getComponent().getName()) {
                isOk = true
                newEdge.target = fanOut.getComponent().getName() + "." + fanOutSonName
              }
            }
            if (!isOk)
              fanOutIsWrong = true
          }
        }
        else if (everyRegisters.contains(fanOut)) {
          newEdge.target = fanOutSonName
        }
        else fanOutIsWrong = true
/** Getting the destination name*/


        if (clkMap.contains(net.getName())) newEdge.highlight = clkMap(net.getName())
        else if (clkMap.contains(net.clockDomain.toString())) newEdge.highlight = clkMap(net.clockDomain.toString())
        newEdge.source = sourceName
        newEdge.isBus = inIsBus & outIsBus
        if (newEdge.isBus == 1) {
          if (findParent(net).getClass.getSimpleName == findParent(fanOut).getClass.getSimpleName) newEdge.label = findParent(fanOut).getClass.getSimpleName
          else newEdge.label = findParent(net).getClass.getSimpleName + " to " + findParent(fanOut).getClass.getSimpleName
        }
        var isContained = false
        for (thisEdge <- edges) {
          if (thisEdge.source == newEdge.source && thisEdge.target == newEdge.target && thisEdge.label == newEdge.label)
            isContained = true
        }
        if (!netIsWrong && !fanOutIsWrong && !isContained && newEdge.source != "" && newEdge.target != "")
          edges.add(newEdge)
      }
    }
  }
/** Setting and adding Edge*/

  private def drawNodes(thisNode: ElkNode): Unit = {
    builder ++= s"""{id:"${thisNode.labelName}",\n"""
    if (thisNode.typeName != "") builder ++= s"""type:"${thisNode.typeName}",\n"""
    if (thisNode.highlight != 0) builder ++= s"""highlight:${thisNode.highlight},\n"""
    if (thisNode.inPorts.nonEmpty) {
      builder ++= s"""inPorts: ["""
      for (inPort <- thisNode.inPorts) builder ++= s""""$inPort","""
      builder ++= s"""],\n"""
    }
    if (thisNode.outPorts.nonEmpty) {
      builder ++= s"""outPorts: ["""
      for (outPort <- thisNode.outPorts) builder ++= s""""$outPort","""
      builder ++= s"""],\n"""
    }
    if (thisNode.children.nonEmpty) {
      builder ++= s"""children: [\n"""
      for (thisChildren <- thisNode.children) drawNodes(thisChildren)
      builder ++= s"""],\n"""
    }
    if (thisNode == topNode) drawEdges()
    builder ++= s"""},\n"""
  }
/** Generating HDElk language for Node*/


  private def drawEdges(): Unit = {
    builder ++= s"""edges:[\n"""
    for (edge <- edges) {
      builder ++= s"""{ source:"${edge.source}",target:"${edge.target}",bus:${edge.isBus},"""
      if (edge.label != "")
        builder ++= s"""label:"${edge.label}","""
      if (edge.highlight != 0)
        builder ++= s"""highlight:${edge.highlight}"""
      builder ++= s"""},\n"""
    }
    builder ++= s"""]\n"""
  }
/** Generating HDElk language for Edge*/

  private def drawClockDomains():Unit = {
    builder ++= s"""{id:"ClockDomains",\nchildren:[\n"""
    for (element <- clkNamesMap) {
      builder ++= s"""{id:"${element._1}",highlight:${element._2}},\n"""
    }
    builder ++= s"""]\n}\n"""
  }
/** Generating HDElk language for Clock legend*/
  def beginDraw():Unit = {
    builder ++=
      s"""
         |<div id="${topNode.labelName}"></div>
         |<h3>${topNode.labelName}</h3><br><br><br><br>
         |<script type="text/javascript">
         |
         |var mygraph = {
         |children:[
         |""".stripMargin
    dealAllSignals()
    drawNodes(topNode)
    if (clkMap.nonEmpty)
      drawClockDomains()
    builder ++= s"""],\n}\nhdelk.layout( mygraph,"${topNode.labelName}");\n</script>\n"""
    val fileName = topLevelName + ".html"
    val file = new File(fileName)
    val pw = new FileWriter(file, true)
    pw.write(builder.toString())
    pw.close()
  }
}
/** Integrating all methods to draw an image*/

class GenerateDiagrams(rtl: SpinalReport[Component]) {
  def beginGenerate(): Unit = {
    val module = rtl.toplevel
    val fileName = rtl.toplevelName + ".html"
    val file = new File(fileName)
    val pw = new FileWriter(file)
    val builder = new StringBuilder()
    builder ++=
      s"""<!DOCTYPE html>
         |<html>
         |<head>
         |    <meta charset="UTF-8">
         |    <title>RTL连接图</title>
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
         |<script src="/js/elk.bundled.js"></script>
         |<script src="/js/svg.min.js"></script>
         |<script src="/js/hdelk.js"></script>
         |
         |<h1 class="center-title">choose diagrams</h1>
         |<div class="buttons-container">
         |<a href="#${rtl.toplevelName}"><button>${rtl.toplevelName}</button></a>&nbsp;
         |""".stripMargin

    val allInnerCells = module.children
    for (cell <- allInnerCells) {
      builder ++= s"""<a href="#${cell.getName()}"><button>${cell.getName()}</button></a>&nbsp;\n"""
    }
    builder ++= s"""</div><br><br><br><br>\n"""
    pw.write(builder.toString())
    pw.close()
    new GenerateOneDiagram(module, rtl.toplevelName, rtl.toplevelName).beginDraw()
    for (inner <- module.children)
      new GenerateOneDiagram(inner, rtl.toplevelName, inner.getName()).beginDraw()
    val pa = new FileWriter(file, true)
    pa.write("</body>\n</html>")
    pa.close()
  }
}
/** Generating all diagrams*/
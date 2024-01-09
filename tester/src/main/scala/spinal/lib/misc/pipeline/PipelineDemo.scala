package spinal.lib.misc.pipeline

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.graphic.Rgb
import spinal.lib.misc.pipeline._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TopLevel extends Component {
  val io = new Bundle{
    val up = slave Stream (UInt(16 bits))
    val down = master Stream (UInt(16 bits))
  }

  // Let's define 3 Nodes for our pipeline
  val n0, n1, n2 = Node()

  // Let's connect those nodes by using simples registers
  val s01 = StageLink(n0, n1)
  val s12 = StageLink(n1, n2)

  // Let's define a few stageable things that can go through the pipeline
  val VALUE = Payload(UInt(16 bits))
  val RESULT = Payload(UInt(16 bits))

  // Let's bind io.up to n0
  io.up.ready := n0.ready
  n0.valid := io.up.valid
  n0(VALUE) := io.up.payload

  // Let's do some processing on n1
  n1(RESULT) := n1(VALUE) + 0x1200

  // Let's bind n2 to io.down
  n2.ready := io.down.ready
  io.down.valid := n2.valid
  io.down.payload := n2(RESULT)

  // Let's ask the builder to generate all the required hardware
  Builder(s01, s12)
}

class TopLevel2 extends Component {
  val VALUE = Payload(UInt(16 bits))

  val io = new Bundle{
    val up = slave Stream(VALUE)  //VALUE can also be used as a HardType
    val down = master Stream(VALUE)
  }

  // Let's define 3 Nodes for our pipeline
  val n0, n1, n2 = Node()

  // Let's connect those nodes by using simples registers
  val s01 = StageLink(n0, n1)
  val s12 = StageLink(n1, n2)

  // Let's bind io.up to n0
  n0.arbitrateFrom(io.up)
  n0(VALUE) := io.up.payload

  // Let's do some processing on n1
  val RESULT = n1.insert(n1(VALUE) + 0x1200)

  // Let's bind n2 to io.down
  n2.arbitrateTo(io.down)
  io.down.payload := n2(RESULT)

  // Let's ask the builder to generate all the required hardware
  Builder(s01, s12)
}

class TopLevel2a extends Component {
  val VALUE = Payload(UInt(16 bits))

  val io = new Bundle{
    val up = slave Stream(VALUE)  //VALUE can also be used as a HardType
    val down = master Stream(VALUE)
  }

  // NodesBuilder will be used to register all the nodes created, connect them via stages and generate the hardware
  val builder = new NodesBuilder()

  // Let's define a Node which connect from io.up
  val n0 = new builder.Node{
    arbitrateFrom(io.up)
    VALUE := io.up.payload
  }

  // Let's define a Node which do some processing
  val n1 = new builder.Node{
    val RESULT = insert(VALUE + 0x1200)
  }

  //  Let's define a Node which connect to io.down
  val n2 = new builder.Node {
    arbitrateTo(io.down)
    io.down.payload := n1.RESULT
  }

  // Let's connect those nodes by using simples registers and generate the relatedhardware
  builder.genStagedPipeline()
}


class TopLevel3 extends Component {
  val ADDRESS = Payload(UInt(8 bits))

  val io = new Bundle{
    val up = slave Flow(ADDRESS)  //VALUE can also be used as a HardType
  }

  // Let's define 3 Nodes for our pipeline
  val c0, c1, c2 = CtrlLink()

  // Let's connect those nodes by using simples registers
  val s01 = StageLink(c0.down, c1.up)
  val s12 = StageLink(c1.down, c2.up)

  c0.up.arbitrateFrom(io.up)
  c0(ADDRESS) := io.up.payload
  val ram = Mem.fill(256)(UInt(16 bits))
  val READ_DATA = c1.up.insert(ram.readSync(c0(ADDRESS), c0.down.isFiring, readUnderWrite = writeFirst))
  val WRITE_DATA = c2.insert(c2(READ_DATA)+1)
  ram.write(c2(ADDRESS), c2(WRITE_DATA), c2.down.isFiring)
  ram.generateAsBlackBox()

  for(c <- List(c1, c2)){
    c0.haltWhen(c.up.isValid && c(ADDRESS) === c0(ADDRESS))
  }
//  val bypass = new Area{
//    val data = RegNext(c2(WRITE_DATA))
//
//    def addBypassOn(addressCtrl : Node, dataCtrl: CtrlConnector): Unit = {
//      val hit = RegNext(c2.down.isValid && addressCtrl(ADDRESS) === c2(ADDRESS)) init(False)
//      when(hit) {
//        dataCtrl.bypass(READ_DATA) := data
//      }
//    }
//
//    addBypassOn(c0.down, c1)
//    addBypassOn(c1.down, c2)
//  }



  // Let's ask the builder to generate all the required hardware
  Builder(s01, s12, c0, c1, c2)
}

// This area allows to take a input value and do +1 +1 +1 over 3 stages.
// It can be instantiated in pipeline (reusability)
// I know that's useless, but let's pretend that instead it does a square root XD
class PLus3(INPUT: Payload[UInt], stage1: Node, stage2: Node, stage3: Node) extends Area {
  val ONE = stage1.insert(stage1(INPUT) + 1)
  val TWO = stage2.insert(stage2(ONE) + 1)
  val THREE = stage3.insert(stage3(TWO) + 1)
}

// Let's define a component which takes a stream as input,
// which carries 'lanesCount' values that we want to process in parallel
// and put the result on an output stream
class TopLevel4(lanesCount : Int) extends Component {
  val io = new Bundle{
    val up = slave Stream(Vec.fill(lanesCount)(UInt(16 bits)))
    val down = master Stream(Vec.fill(lanesCount)(UInt(16 bits)))
  }

  // Let's define 3 Nodes for our pipeline
  val n0, n1, n2 = Node()

  // Let's connect those nodes by using simples registers
  val s01 = StageLink(n0, n1)
  val s12 = StageLink(n1, n2)

  // Let's bind io.up to n0
  n0.arbitrateFrom(io.up)
  val LANES_INPUT = io.up.payload.map(n0.insert(_))

  // Let's use our "reusable" Plus3 area to generate each processing lane
  val lanes = for(i <- 0 until lanesCount) yield new PLus3(LANES_INPUT(i), n0, n1, n2)

  // Let's bind n2 to io.down
  n2.arbitrateTo(io.down)
  for(i <- 0 until lanesCount) io.down.payload(i) := n2(lanes(i).THREE)

  // Let's ask the builder to generate all the required hardware
  Builder(s01, s12)
}


class RgbToSomething(addAt : Int,
                     invAt : Int,
                     mulAt : Int,
                     resultAt : Int) extends Component {
  val io = new Bundle {
    val up = slave Stream(spinal.lib.graphic.Rgb(8, 8, 8))
    val down = master Stream (UInt(16 bits))
  }

  // Let's define the Nodes for our pipeline
  val nodes = Array.fill(resultAt+1)(Node())

  // Let's specify which node will be used for what part of the pipeline
  val insertNode = nodes(0)
  val addNode = nodes(addAt)
  val invNode = nodes(invAt)
  val mulNode = nodes(mulAt)
  val resultNode = nodes(resultAt)

  // Define the hardware which will feed the io.up stream into the pipeline
  val inserter = new insertNode.Area {
    arbitrateFrom(io.up)
    val RGB = insert(io.up.payload)
  }

  // sum the r g b values of the color
  val adder = new addNode.Area {
    val SUM = insert(inserter.RGB.r + inserter.RGB.g + inserter.RGB.b)
  }

  // flip all the bit of the RGB sum
  val inverter = new invNode.Area {
    val INV = insert(~adder.SUM)
  }

  // multiplie the inverted bits with 0xEE
  val multiplier = new mulNode.Area {
    val MUL = insert(inverter.INV*0xEE)
  }

  // Connect the end of the pipeline to the io.down stream
  val resulter = new resultNode.Area {
    arbitrateTo(io.down)
    io.down.payload := multiplier.MUL
  }

  // Let's connect those nodes sequencialy by using simples registers
  val connectors = for (i <- 0 to resultAt - 1) yield StageLink(nodes(i), nodes(i + 1))

  // Let's ask the builder to generate all the required hardware
  Builder(connectors)
}


object PipelineDemo1 extends App {
  SimConfig.withFstWave.compile(new TopLevel2a).doSim{ dut =>
    dut.clockDomain.forkStimulus(10)
    dut.io.down.ready #= true
    dut.clockDomain.waitSampling(5)

    // Push one transaction
    dut.io.up.valid #= true
    dut.io.up.payload #= 0x0042
    dut.clockDomain.waitSamplingWhere(dut.io.up.ready.toBoolean)
    dut.io.up.valid #= false

    dut.clockDomain.waitSampling(10)
    simSuccess()
  }
}


object PipelineDemo5 extends App {
  SpinalVerilog(
    new RgbToSomething(
      addAt    = 0,
      invAt    = 1,
      mulAt    = 2,
      resultAt = 3
    )
  )
}



object PipelineDemo6 extends App {

  case class PixelSolverGenerics(fixAmplitude : Int,
                                 fixResolution : Int,
                                 iterationLimit : Int){
    val iterationWidth = log2Up(iterationLimit+1)
    def iterationType = UInt(iterationWidth bits)
    def fixType = SFix(
      peak = fixAmplitude exp,
      resolution = fixResolution exp
    )
  }

  case class PixelTask(g : PixelSolverGenerics) extends Bundle{
    val x,y = g.fixType
  }

  case class PixelResult(g : PixelSolverGenerics) extends Bundle{
    val iteration = g.iterationType
  }

  class DeflectorLink(val up : Node, val down : Node, val deflect : Node) extends Link{
    deflect.up = this
    down.up = this
    up.down = this

    val forceDeflect = False
    override def ups: Seq[Node] = List(up)
    override def downs: Seq[Node] = List(down, deflect)
    override def propagateDown(): Unit = {
      propagateDownAll()
    }
    override def propagateUp(): Unit = {
      propagateUpAll()
    }

    override def build(): Unit = {
      Link.connectDatas(up, down)
      Link.connectDatas(up, deflect)

      val doDeflect = up.valid && (forceDeflect || !down.ready)
      down.valid := up.valid && !doDeflect
      deflect.valid := up.valid && doDeflect
      assert(deflect.ctrl.ready.isEmpty)
    }
  }

  class FeedbackLink(val up: Node, val feedback: Node, val down: Node) extends Link {
    down.up = this
    up.down = this

    val swaps = mutable.LinkedHashMap[Payload[_ <: Data], Payload[_ <: Data]]()

    override def ups: Seq[Node] = List(up)
    override def downs: Seq[Node] = List(down)

    override def nodeSetup(): Unit = {
      for ((to, from) <- swaps) {
        feedback(from)
      }
    }

    override def propagateDown(): Unit = {
      propagateDownAll()
    }

    override def propagateUp(): Unit = {
      propagateUpAll()
    }

    override def build(): Unit = {
      down.valid := up.valid || feedback.valid
      up.ready := !feedback.valid

      Link.connectDatas(up, down)
      when(feedback.valid){
        Link.connectDatasWithSwap(feedback, down, swaps)
      }
    }
  }

  case class PixelSolver(g : PixelSolverGenerics) extends Component {
    val io = new Bundle {
      val cmd = slave Stream (PixelTask(g))
      val rsp = master Stream (PixelResult(g))
    }

    import g._
    val idWidth = 3

    // Let's define all the nodes
    val inject = new Node{
      arbitrateFrom(io.cmd)
      val freeId    = Counter(1 << idWidth,inc = io.cmd.fire)
      val TASK      = insert(io.cmd.payload)
      val ID        = insert(freeId.value)
      val ITERATION = insert(U(0, iterationWidth bits))
      val DONE      = insert(False)
      val X, Y      = insert(SF(0, g.fixAmplitude exp, g.fixResolution exp))
    }

    val feedback = new Node()

    val mul = new Node {
      val XX, YY, XY = Payload(fixType)
      this(XX) := (inject.X * inject.X).truncated
      this(YY) := (inject.Y * inject.Y).truncated
      this(XY) := (inject.X * inject.Y).truncated
    }

    val add = new Node {
      val XN, YN         = Payload(fixType)
      this(XN)          := (mul.XX - mul.YY + inject.TASK.x).truncated
      this(YN)          := (((mul.XY) << 1) + inject.TASK.y).truncated
      val DONE_NEXT      = insert(inject.DONE || mul.XX + mul.YY >= 4.0 || inject.ITERATION === iterationLimit)
      val ITERATION_NEXT = insert(inject.ITERATION + (!DONE_NEXT).asUInt)
    }

    val s2 =  new Node

    val completed = new Node{
      arbitrateTo(io.rsp)
      io.rsp.iteration := inject.ITERATION
    }

    //Let's connect all the nodes
    val arbiter = new FeedbackLink(inject, feedback, mul){
      swaps(inject.TASK)      = inject.TASK
      swaps(inject.ID)        = inject.ID
      swaps(inject.X)         = add.XN
      swaps(inject.Y)         = add.YN
      swaps(inject.DONE)      = add.DONE_NEXT
      swaps(inject.ITERATION) = add.ITERATION_NEXT
    }
    val mulToAdd  = new StageLink(mul, add)
    val addToS2   = new StageLink(add, s2)
    val deflector = new DeflectorLink(s2, completed, feedback){
      val wantedId = Counter(1 << idWidth, inc = completed.isFiring)
      forceDeflect setWhen(!up(add.DONE_NEXT) || wantedId =/= up(inject.ID))
    }

    Builder(arbiter, mulToAdd, addToS2, deflector)
  }

  SpinalVerilog(
    new PixelSolver(
      PixelSolverGenerics(
        fixAmplitude = 7,
        fixResolution = -20,
        iterationLimit = 63
      )
    )
  )

  object PixelSolverChecker {

    import java.awt.event.{WindowAdapter, WindowEvent}
    import java.awt.image.BufferedImage
    import java.awt.{Color, Dimension, Graphics}

    import javax.swing.{JFrame, JPanel, WindowConstants}
    import spinal.core._
    import spinal.core.sim._
    import spinal.lib._
    import spinal.lib.sim.{StreamMonitor, StreamReadyRandomizer}

    import scala.collection.mutable.ArrayBuffer
    import scala.io.Source
    import scala.util.Random

    def apply(cmd: Stream[PixelTask], rsp: Stream[PixelResult], cd: ClockDomain): Unit = {
      SimTimeout(200000000)

      val resX = 64
      val resY = 64
      //Produce cmd stimulus
      fork {
        val startX = -2.0
        val startY = -1.5
        val endX = 0.8
        val endY = 1.5
        val stepX = (endX - startX) / resX
        val stepY = (endY - startY) / resY

        cmd.valid #= false
        cd.waitSampling()

        for (y <- 0 until resY;
             x <- 0 until resX) {
          cd.waitSampling(Random.nextInt(10))
          cmd.valid #= true
          cmd.x.raw #= ((startX + x * stepX) * (1 << 20)).toInt
          cmd.y.raw #= ((startY + y * stepY) * (1 << 20)).toInt
          cd.waitSamplingWhere(cmd.ready.toBoolean)
          cmd.valid #= false
//          cd.waitSampling(1000)
        }
      }

      StreamReadyRandomizer(rsp, cd)
      val resultArray = Array.ofDim[Int](resY, resY)

      //Monitor the rsp stream
      fork {
        val image = new BufferedImage(resX, resY, BufferedImage.TYPE_INT_BGR);
        for (y <- 0 until resY;
             x <- 0 until resX) {
          cd.waitSamplingWhere(rsp.valid.toBoolean && rsp.ready.toBoolean)
          resultArray(y)(x) = rsp.iteration.toInt
          image.setRGB(x, y, rsp.iteration.toInt * 16 * 256)
        }

        val dutLines = ArrayBuffer[String]()
        println("DUT=")
        for (y <- 0 until resY) {
          val l = resultArray(y).map(v => f"$v%2d").mkString(",")
          println(l)
          dutLines += l
        }
//        val ref = "src/test/scala/workshop/mandelbrot/mandelbrot.ref"
        var error = false
//        for ((refLine, i) <- Source.fromFile(ref).getLines.zipWithIndex) {
//          val dutLine = dutLines(i)
//          if (dutLine != refLine) error = true
//        }
//
//        if (error) println("Error, Doesn't match the reference")


        //GUI
        val frame = new JFrame {
          setPreferredSize(new Dimension(resX * 4 + 16, resY * 4 + 48));
          var closed = false
          add(new JPanel {
            this.setPreferredSize(new Dimension(resX, resY))

            override def paintComponent(g: Graphics): Unit = {
              g.drawImage(image, 0, 0, resX * 4, resY * 4, null)
            }
          })

          pack();
          setVisible(true);
          setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
          addWindowListener(new WindowAdapter() {
            override def windowClosing(e: WindowEvent): Unit = {
              closed = true
            }
          });
        }
        simSuccess()
        while (true) {
          if (frame.closed) {
            println("simTime : " + simTime())
            if (error) simFailure() else simSuccess()
            Thread.sleep(10)
          }
        }
      }
    }
  }


  val compiled  = SimConfig.withFstWave.compile(
    PixelSolver(
      g = PixelSolverGenerics(
        fixAmplitude = 7,
        fixResolution = -20,
        iterationLimit = 15
      )
    )
  )

  compiled.doSimUntilVoid(seed = 42) { dut =>
    dut.clockDomain.forkStimulus(10)
    PixelSolverChecker(dut.io.cmd, dut.io.rsp, dut.clockDomain)
  }
}

package spinal.demo.mandelbrot

import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer


class FrameTaskSolver(p: MandelbrotCoreParameters) extends Component {
  val io = new Bundle {
    val frameTask = slave Stream FrameTask(p)
    val pixelResult = master Stream Fragment(PixelResult(p))
  }

  val pixelTaskGenerator = new PixelTaskGenerator(p)
  val pixelTaskDispatcher = new DispatcherInOrder(Fragment(PixelTask(p)), p.pixelTaskSolverCount)
  val pixelTaskSolver = List.fill(p.pixelTaskSolverCount)(new PixelTaskSolver(p))
  val pixelTaskResultArbiter = StreamArbiter.inOrder.build(Fragment(PixelResult(p)), p.pixelTaskSolverCount)

  pixelTaskGenerator.io.frameTask << io.frameTask
  pixelTaskDispatcher.io.input <-/< pixelTaskGenerator.io.pixelTask
  for (solverId <- 0 until p.pixelTaskSolverCount) {
    pixelTaskSolver(solverId).io.pixelTask <-/< pixelTaskDispatcher.io.outputs(solverId)
    pixelTaskResultArbiter.io.inputs(solverId) </< pixelTaskSolver(solverId).io.result
  }
  io.pixelResult <-< pixelTaskResultArbiter.io.output
}

//  The scala-like way to do the FrameTaskSolver stuff is :

//  val pixelTaskGenerator = new PixelTaskGenerator(p)
//  pixelTaskGenerator.io.frameTask << io.frameTask
//
//  val pixelTaskDispatcher = new DispatcherInOrder(Fragment(PixelTask(p)), p.unitCount)
//  pixelTaskDispatcher.io.input <-/< pixelTaskGenerator.io.pixelTask
//
//  val pixelTaskSolver = List.fill(p.pixelTaskSolverCount)(new PixelTaskSolver(p))
//  (pixelTaskSolver, pixelTaskDispatcher.io.outputs).zipped.foreach(_.io.pixelTask <-/< _)
//
//  val pixelTaskResultArbiter = StreamArbiter.inOrder.build(Fragment(PixelResult(p)), p.unitCount)
//  (pixelTaskSolver, pixelTaskResultArbiter.io.inputs).zipped.foreach(_.io.result >/> _)
//
//  pixelTaskResultArbiter.io.output >-> io.pixelResult


class PixelTaskGenerator(p: MandelbrotCoreParameters) extends Component {
  val io = new Bundle {
    val frameTask = slave Stream FrameTask(p)
    val pixelTask = master Stream Fragment(PixelTask(p))
  }

  val positionOnScreen = Reg(UInt2D(log2Up(p.screenResX) bit, log2Up(p.screenResY) bit))
  val positionOnMandelbrot = Reg(SFix2D(io.frameTask.data.fullRangeSFix))
  val setup = RegInit(True)

  val solverTask = Stream(Fragment(PixelTask(p)))

  io.frameTask.ready := !io.frameTask.valid


  when(io.frameTask.ready) {
    setup := True
  }

  when(io.frameTask.valid && solverTask.ready) {
    when(setup) {
      setup := False
      positionOnScreen.x := 0
      positionOnScreen.y := 0
      positionOnMandelbrot := io.frameTask.data.start
    }.otherwise {
      when(positionOnScreen.x !== p.screenResX - 1) {
        positionOnScreen.x := positionOnScreen.x + 1
        positionOnMandelbrot.x := positionOnMandelbrot.x + io.frameTask.data.inc.x
      }.otherwise {
        positionOnScreen.x := 0
        positionOnMandelbrot.x := io.frameTask.data.start.x
        when(positionOnScreen.y !== p.screenResY - 1) {
          positionOnScreen.y := positionOnScreen.y + 1
          positionOnMandelbrot.y := positionOnMandelbrot.y + io.frameTask.data.inc.y
        }.otherwise {
          positionOnMandelbrot.y := io.frameTask.data.start.y
          io.frameTask.ready := True //Asyncronous acknoledge into syncronous space <3
        }
      }
    }
  }

  solverTask.valid := io.frameTask.valid && !setup;
  solverTask.last := io.frameTask.ready
  solverTask.fragment.mandelbrotPosition := positionOnMandelbrot
  solverTask >-> io.pixelTask

}

class PixelTaskSolver(p: MandelbrotCoreParameters) extends Component {
  val io = new Bundle {
    val pixelTask = slave Stream Fragment(PixelTask(p))
    val result = master Stream Fragment(PixelResult(p))
  }

  //It's the context definition used by each stage of the pipeline, Each task are translated to context ("thread")
  class Context extends Bundle {
    val task = PixelTask(p)
    val lastPixel = Bool
    val done = Bool
    val order = UInt(4 bit)
    //Used to reorder result in same oder than input task
    val iteration = UInt(p.iterationWidth bit)
    val z = SFix2D(p.fix)
  }

  //Extended context with x*x   y*y   x*y result
  class Stage2Context extends Context {
    val zXzX = p.fix
    val zYzY = p.fix
    val zXzY = p.fix
  }

  //TODO move it into libs
  case class MultTask(aOffset: Int, bOffset: Int, aWidth: Int, bWidth: Int)
  def sintMul(a: SInt, b: SInt, multOpWidth: Int, keepFrom: Int): SInt = {
    val multTasks = ArrayBuffer[MultTask]()
    val aWidth = widthOf(a)
    val bWidth = widthOf(b)
    for (aOffset <- Range(0, aWidth, multOpWidth)) {
      for (bOffset <- Range(0, bWidth, multOpWidth)) {
        val aPartWidth = Math.min(multOpWidth, aWidth - aOffset)
        val bPartWidth = Math.min(multOpWidth, bWidth - bOffset)
        if (aOffset + aPartWidth + bOffset + bPartWidth > keepFrom)
          multTasks += MultTask(aOffset, bOffset, aPartWidth, bPartWidth)
      }
    }

    var ret = SInt(widthOf(a) + widthOf(b) bit)
    ret := 0
    //for(task <- multTasks){
    val xx = multTasks.map(task => {
      val aPartSigned = task.aOffset + task.aWidth == aWidth
      val bPartSigned = task.bOffset + task.bWidth == bWidth
      val aPart = a(task.aOffset, task.aWidth bit)
      val bPart = b(task.bOffset, task.bWidth bit)
      val mult = (aPartSigned, bPartSigned) match {
        case (false, false) => toSInt(False ## (toUInt(aPart) * toUInt(bPart)))
        case (false, true) => toSInt(False ## aPart) * bPart
        case (true, false) => aPart * toSInt(False ## bPart)
        case (true, true) => aPart * bPart
      }
      ret = ret + (mult << (task.aOffset + task.bOffset))
      mult << (task.aOffset + task.bOffset)
    })
    //    def reduceIt(l : SInt,r : SInt) : SInt = {
    //      RegNext(l + r)
    //    }
    xx.reduceBalancedSpinal((l, r) => l + r, (s, l) => RegNext(s))
  }

  def fixMul(a: SFix, b: SFix): SFix = {
    val ret = SFix(a.exp + b.exp, a.bitCount + b.bitCount bit)
    ret.raw := sintMul(a.raw, b.raw, 17, p.fixWidth) //p.fixWidth
    //Delay(ret,3)
    ret
  }


  val insertTaskOrder = Counter(16, io.pixelTask.fire)

  //Task to insert into the pipeline
  val taskToInsert: Stream[Context] = io.pixelTask.translateInto(Stream(new Context))((to, from) => {
    to.task := from.fragment
    to.lastPixel := from.last
    to.done := False
    to.order := insertTaskOrder;
    to.iteration := 0
    to.z := from.fragment.mandelbrotPosition
  })
  val loopBack = RegFlow(new Context)
  //Loop back from end of pipeline
  val stage0 = StreamFlowArbiter(taskToInsert, loopBack) //First stage


  //Stage1 is a routing stage
  val stage1 = Delay(stage0, 2)


  //Stage2 get multiplication result of x*x  y*y and x*y
  val stage2 = RegFlow(new Stage2Context)

  stage2.data.zXzX := fixMul(stage1.data.z.x, stage1.data.z.x)
  stage2.data.zYzY := fixMul(stage1.data.z.y, stage1.data.z.y)
  stage2.data.zXzY := fixMul(stage1.data.z.x, stage1.data.z.y)
  stage2 assignSomeByName Delay(stage1, latencyAnalysis(stage1.data.z.x.raw, stage2.data.zXzX.raw) - 1)


  //Stage3 calculate next position of the iteration (zX,zY)
  //       Increment the iteration count if was not done
  //       calculate if the "Thread" has rush a end condition (iteration > N, x*x+y*y + 4)
  val stage3 = RegFlow(new Context)
  stage3 assignAllByName stage2

  stage3.data.z.x := stage2.data.zXzX - stage2.data.zYzY + stage2.data.task.mandelbrotPosition.x
  stage3.data.z.y := (stage2.data.zXzY << 1) + stage2.data.task.mandelbrotPosition.y
  when(!stage2.data.done) {
    stage3.data.iteration := stage2.data.iteration + 1
  }
  when(stage2.data.iteration >= p.iterationLimit | stage2.data.zXzX + stage2.data.zYzY >= 4.0) {
    stage3.data.done := True
  }


  //End Stage put to the output the result if it's finished and in right order
  //          else put it into the feedback to redo iteration or to waiting
  val result = Stream(Fragment(PixelResult(p)))
  val resultOrder = Counter(16, result.fire)
  val readyForResult = stage3.data.done && resultOrder === stage3.data.order


  result.valid := stage3.valid && readyForResult
  result.last := stage3.data.lastPixel
  result.fragment.iteration := stage3.data.iteration - 1


  result >-> io.result

  loopBack.valid := stage3.valid && ((!readyForResult) || (!result.ready))
  loopBack.data := stage3.data

}


//object Main {
//  def main(args: Array[String]) {
//    SpinalVhdl(new FrameTaskSolver(new MandelbrotCoreParameters(256, 4, 64, 64, 7, 36)))
//  }
//}


//Self pipelined fixed point multiplication, should be better with ip
//  def fixMul(a: SInt, b: SInt): SInt = {
//    val width = a.getWidth
//    if (width <= 18) {
//      val reg = RegNext(a * b)
//      val reg2 = RegNext(reg(width * 2 - p.fixExp - 1, width - p.fixExp).toBits.toSInt)
//      val reg3 = RegNext(reg2)
//      return reg3
//    } else {
//      val lowWidth = 18
//      val highWidth = width - lowWidth
//
//      val step0_a1b0 = RegNext(a(width - 1, lowWidth) * toSInt(False ## b(lowWidth - 1, 0)))
//      val step0_a0b1 = RegNext(toSInt(False ## a(lowWidth - 1, 0)) * b(width - 1, lowWidth))
//      val step0_a1b1 = RegNext(a(width - 1, lowWidth) * b(width - 1, lowWidth))
//
//
//      val step1_a1b0 = RegNext(step0_a1b0)
//      val step1_a0b1 = RegNext(step0_a0b1)
//      val step1_a1b1 = RegNext(step0_a1b1)
//
//      val step2_a0b1_a1b0 = RegNext(step1_a0b1 + step1_a1b0)
//      val step2_a1b1 = RegNext(step1_a1b1)
//
//      val result = ((step2_a0b1_a1b0) << lowWidth) + (step2_a1b1 << (lowWidth * 2))
//      return result(width * 2 - p.fixExp - 1, width - p.fixExp)
//    }
//  }

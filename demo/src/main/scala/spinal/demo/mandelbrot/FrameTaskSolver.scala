package spinal.demo.mandelbrot

import spinal.core._
import spinal.lib._
import spinal.lib.math.SIntMath


class FrameTaskSolver(p: MandelbrotCoreParameters) extends Component {
  val io = new Bundle {
    val frameTask = slave Stream FrameTask(p)
    val pixelResult = master Stream Fragment(PixelResult(p))
  }

  val pixelTaskGenerator = new PixelTaskGenerator(p)
  val pixelTaskDispatcher = new StreamDispatcherSequencial(Fragment(PixelTask(p)), p.pixelTaskSolverCount)
  val pixelTaskSolver = List.fill(p.pixelTaskSolverCount)(new PixelTaskSolver(p))
  val pixelResultArbiter = StreamArbiterFactory.sequentialOrder.build(Fragment(PixelResult(p)), p.pixelTaskSolverCount)

  pixelTaskGenerator.io.frameTask << io.frameTask
  pixelTaskDispatcher.io.input <-/< pixelTaskGenerator.io.pixelTask
  for (solverId <- 0 until p.pixelTaskSolverCount) {
    pixelTaskSolver(solverId).io.pixelTask << pixelTaskDispatcher.io.outputs(solverId)
    pixelResultArbiter.io.inputs(solverId) << pixelTaskSolver(solverId).io.pixelResult
  }
  io.pixelResult <-< pixelResultArbiter.io.output
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
  val positionOnMandelbrot = Reg(SFix2D(io.frameTask.fullRangeSFix))
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
      positionOnMandelbrot := io.frameTask.start
    }otherwise {
      when(positionOnScreen.x =/= p.screenResX - 1) {
        positionOnScreen.x := positionOnScreen.x + 1
        positionOnMandelbrot.x := positionOnMandelbrot.x + io.frameTask.inc.x
      }otherwise {
        positionOnScreen.x := 0
        positionOnMandelbrot.x := io.frameTask.start.x
        when(positionOnScreen.y =/= p.screenResY - 1) {
          positionOnScreen.y := positionOnScreen.y + 1
          positionOnMandelbrot.y := positionOnMandelbrot.y + io.frameTask.inc.y
        }otherwise {
          positionOnMandelbrot.y := io.frameTask.start.y
          io.frameTask.ready := True //Asynchronous acknoledge into synchronous space <3
        }
      }
    }
  }

  solverTask.valid := io.frameTask.valid && !setup;
  solverTask.last := io.frameTask.ready
  solverTask.fragment.mandelbrotPosition := positionOnMandelbrot.truncated
  solverTask >-> io.pixelTask

}

class PixelTaskSolver(p: MandelbrotCoreParameters) extends Component {
  val io = new Bundle {
    val pixelTask = slave Stream Fragment(PixelTask(p))
    val pixelResult = master Stream Fragment(PixelResult(p))
  }

  //It's the context definition used by each stage of the pipeline, Each task are translated to context ("thread")
  class Context extends Bundle {
    val task = PixelTask(p)
    val lastPixel = Bool
    val done = Bool
    val order = UInt(5 bit)
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

  val insertTaskOrder = Counter(32, io.pixelTask.fire)

  //Task to insert into the pipeline
  val taskToInsert: Stream[Context] = io.pixelTask.translateInto(Stream(new Context))((to, from) => {
    to.task := from.fragment
    to.lastPixel := from.last
    to.done := False
    to.order := insertTaskOrder
    to.iteration := 0
    to.z := from.fragment.mandelbrotPosition
  })
  val loopBack = RegFlow(new Context)
  //Loop back from end of pipeline
  val stage0 = StreamFlowArbiter(taskToInsert, loopBack) //First stage


  //Stage1 is a routing stage
  val stage1 = DelayWithInit(stage0, 2)((reg) => reg.valid.init(False))



  //Stage2 get multiplication result of x*x  y*y and x*y
  val stage2 = RegFlow(new Stage2Context)

  def fixMul(a: SFix, b: SFix): SFix = {
    val ret = SFix(a.maxExp + b.maxExp + 1 exp, a.bitCount + b.bitCount bit)
    // SIntMath.mul is a pipelined implementation of the signed multiplication operator
    //(leftSigned, rightSigned, number of bit per multiplicator, trunk bits bellow this limit, ...)
    ret.raw := SIntMath.mul(a.raw, b.raw, 17, p.fixWidth - p.fixExp, 1, (stage, level) => RegNext(stage))
    ret

   // return a*b
  }


  stage2.zXzX := fixMul(stage1.z.x, stage1.z.x).truncated
  stage2.zYzY := fixMul(stage1.z.y, stage1.z.y).truncated
  stage2.zXzY := fixMul(stage1.z.x, stage1.z.y).truncated
  val stage1ToStage2Latency = LatencyAnalysis(stage1.z.x.raw, stage2.zXzX.raw) - 1
  stage2 assignSomeByName DelayWithInit(stage1, stage1ToStage2Latency)((reg) => reg.valid.init(False))

  //Stage3 calculate next position of the iteration (zX,zY)
  //       Increment the iteration count if was not done
  //       calculate if the "Thread" has rush a end condition (iteration > N, x*x+y*y + 4)
  val stage3 = RegFlow(new Context)
  stage3 assignAllByName stage2
  stage3.allowOverride
  stage3.z.x := stage2.zXzX - stage2.zYzY + stage2.task.mandelbrotPosition.x
  stage3.z.y := ((stage2.zXzY << 1) + stage2.task.mandelbrotPosition.y).truncated
  when(!stage2.done) {
    stage3.iteration := stage2.iteration + 1
  }
  when(stage2.iteration >= p.iterationLimit | stage2.zXzX + stage2.zYzY >= 4.0) {
    stage3.done := True
  }


  //End Stage put to the output the result if it's finished and in right order
  //          else put it into the feedback to redo iteration or to waiting
  val result = Stream(Fragment(PixelResult(p)))
  val resultOrder = Counter(32, result.fire)
  val readyForResult = stage3.done && resultOrder === stage3.order


  result.valid := stage3.valid && readyForResult
  result.last := stage3.lastPixel
  result.fragment.iteration := stage3.iteration - 1


  result >-> io.pixelResult

  loopBack.valid := stage3.valid && ((!readyForResult) || (!result.ready))
  loopBack.payload := stage3.payload
}

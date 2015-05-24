package spinal.demo.mandelbrot

import spinal.core._
import spinal.lib._


case class UInt2D(bitCount: BitCount) extends Bundle{
  val x = UInt(bitCount)
  val y = UInt(bitCount)
}

object SFix2D{
  def apply(copy : SFix) : SFix2D = SFix2D(copy.exp,copy.bitCount)
}

case class SFix2D(exp : Int,bitCount : Int) extends Bundle{
  val x = SFix(exp,bitCount)
  val y = SFix(exp,bitCount)
}

object FrameTaskSolver {
  class Parameter(val unitCount: Int, val screenResX: Int, val screenResY: Int, val fixExp: Int, val fixWidth: Int) {
    def fix = SFix(fixExp,fixWidth)
    def frameTask = FrameTask(4,32)

    def resultBits = UInt(8 bit)
    def pixelResult = Handshake(resultBits)
    def PixelTaskStream = Handshake(new PixelTask(this))
  }
}

case class FrameTask(exp : Int,bitCount : Int) extends Bundle {
  val start = SFix2D(exp,bitCount)
  val inc = SFix2D(exp-8,bitCount+8)

  def globalSFix = SFix(exp,bitCount + 16)
}

case class PixelTask(p: FrameTaskSolver.Parameter) extends Bundle {
  val mandelbrotPosition = SFix2D(p.fix)
}

class FrameTaskSolver(p: FrameTaskSolver.Parameter) extends Component {
  val io = new Bundle {
    val frameTask = slave Handshake (p.frameTask)
    val pixelResult = master(p.pixelResult)
  }

//  val taskGenerator = new TaskGenerator(p)
//  val taskDispatcher = new DispatcherInOrder(new Task(p), p.unitCount)
//  val result_arbiter = HandshakeArbiter.inOrder.build(p.resultBits, p.unitCount)
//
//  taskGenerator.io.job << io.job
//  taskDispatcher.io.input <-/< taskGenerator.io.solverTask
//  for((taskDispatcherOutput,resultArbiterInput) <- (taskDispatcher.io.output,result_arbiter.io.inputs).zipped){
//    val solver = new SolverPipelined(p)
//    solver.io.task <-/< taskDispatcherOutput
//    solver.io.result >/> resultArbiterInput
//  }
//  result_arbiter.io.output >-> io.result

  val taskGenerator = new PixelTaskGenerator(p)
  taskGenerator.io.frameTask << io.frameTask

  val taskDispatcher = new DispatcherInOrder(new PixelTask(p), p.unitCount)
  taskDispatcher.io.input <-/< taskGenerator.io.pixelTask

  val pixelSolver = List.fill(p.unitCount)(new PixelTaskSolver(p))
  (pixelSolver, taskDispatcher.io.outputs).zipped.map(_.io.pixelTask <-/< _)

  val result_arbiter = HandshakeArbiter.inOrder.build(p.resultBits, p.unitCount)
  (pixelSolver, result_arbiter.io.inputs).zipped.map(_.io.result >/> _)

  result_arbiter.io.output >-> io.pixelResult
}


class PixelTaskGenerator(p: FrameTaskSolver.Parameter) extends Component {
  val io = new Bundle {
    val frameTask = slave Handshake (p.frameTask)
    val pixelTask = master(p.PixelTaskStream)
  }

  val screenPosition = Reg(UInt2D(11 bit))
  val mandelbrotPosition = Reg(SFix2D(io.frameTask.data.globalSFix))
  val setup = RegInit(True)

  val solverTask = Handshake(PixelTask(p))

  io.frameTask.ready := !io.frameTask.valid


  when(io.frameTask.ready) {
    setup := True
  }

  when(io.frameTask.valid && solverTask.ready) {
    when(setup) {
      setup := False
      screenPosition.x := 0
      screenPosition.y := 0
      mandelbrotPosition := io.frameTask.data.start
    }.otherwise {
      when(screenPosition.x !== p.screenResX - 1) {
        screenPosition.x := screenPosition.x + 1
        mandelbrotPosition.x := mandelbrotPosition.x + io.frameTask.data.inc.x
      }.otherwise {
        screenPosition.x := 0
        mandelbrotPosition.x := io.frameTask.data.start.x
        when(screenPosition.x !== p.screenResY - 1) {
          screenPosition.y := screenPosition.y + 1
          mandelbrotPosition.y := mandelbrotPosition.y + io.frameTask.data.inc.y
        }.otherwise {
          mandelbrotPosition.y := io.frameTask.data.start.y
          io.frameTask.ready := True //Asyncronous acknoledge into syncronous space <3
        }
      }
    }
  }

  solverTask.valid := io.frameTask.valid && !setup;
  solverTask.data.mandelbrotPosition :=  mandelbrotPosition

  solverTask >-> io.pixelTask

}

class PixelTaskSolver(p: FrameTaskSolver.Parameter) extends Component {
  val io = new Bundle {
    val pixelTask = slave(p.PixelTaskStream)
    val result = master(p.pixelResult.asMaster)
  }

  //It's the context definition used by each stage of the pipeline, Each task are translated to context ("thread")
  abstract class ContextBase(p: FrameTaskSolver.Parameter) extends Bundle {
    val task = new PixelTask(p)
    val done = Bool
    val order = UInt(4 bit)    //Used to reorder result in same oder than input task
    val iteration = UInt(9 bit)
    val z = SFix2D(p.fix)
  }

  case class Context(p: FrameTaskSolver.Parameter) extends ContextBase(p) {

  }

  //Extended context with x*x   y*y   x*y result
  case class Stage3Context(p: FrameTaskSolver.Parameter) extends ContextBase(p) {
    val zXzX = p.fix
    val zYzY = p.fix
    val zXzY = p.fix
  }


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
  def fixMul(a: SFix, b: SFix): SFix = {
    Delay(a*b,3)
  }


  val insertTaskOrder = Reg(UInt(4 bit)) init (0)
  val resultOrder = Reg(UInt(4 bit)) init (0)


  val stage0 = RegFlow(new Context(p))
  //First stage
  val loopBack = RegFlow(new Context(p))
  //Loop back from end of pipeline
  val insertTask = new Handshake(new Context(p)) //Task to insert into the pipeline


  //Hardcoded way to translate the input Task to Context
  val insertTaskBits = new Context(p)
  insertTaskBits.task := io.pixelTask.data
  insertTaskBits.done := Bool(false)
  insertTaskBits.order := insertTaskOrder;
  insertTaskBits.iteration := 0
  insertTaskBits.z := io.pixelTask.data.mandelbrotPosition

  (io.pixelTask ~ insertTaskBits) >> insertTask
  //((io.task ~ insertTaskBits) & (insertTaskOrder === resultOrder)) >> insertTask //same than precedent line but limit the number of "thread" into the pipeline to one
  //End of hardcoded way to translate the input Task to Context

  when(io.pixelTask.fire) {
    //When a transaction is done on io.task =>
    insertTaskOrder := insertTaskOrder + 1
  }

  //begin of the solver pipeline => insert contexts from loopback or insertTask
  stage0.valid := loopBack.valid || insertTask.valid
  insertTask.ready := !loopBack.valid
  stage0.data := Mux(loopBack.valid, loopBack.data, insertTask.data)

  //Stage1 is a routing stage
  val stage1b = RegFlow(new Context(p))
  stage1b := stage0
  val stage1 = RegFlow(new Context(p))
  stage1 := stage1b

  //Stage2 is when x*x  y*y and x*y are done with function fixMul
  val stage2b = RegFlow(new Context(p))
  stage2b := stage1
  val stage2bb = RegFlow(new Context(p))
  stage2bb := stage2b
  val stage2 = RegFlow(new Context(p))
  stage2 := stage2bb

  //Stage3 get multiplication result
  val stage3 = RegFlow(new Stage3Context(p))
  stage3 assignSomeByName stage2

  stage3.data.zXzX := fixMul(stage1.data.z.x, stage1.data.z.x)
  stage3.data.zYzY := fixMul(stage1.data.z.y, stage1.data.z.y)
  stage3.data.zXzY := fixMul(stage1.data.z.x, stage1.data.z.y)

  //Stage4 calculate next position of the iteration (zX,zY)
  //       Increment the iteration count if was not done
  //       calculate if the "Thread" has rush a end condition (iteration > N, x*x+y*y + 4)
  val stage4 = RegFlow(new Context(p))
  stage4 assignSomeByName stage3

  stage4.data.z.x := stage3.data.zXzX - stage3.data.zYzY + stage3.data.task.mandelbrotPosition.x
  stage4.data.z.y := (stage3.data.zXzY << 1) + stage3.data.task.mandelbrotPosition.y
  when(!stage3.data.done) {
    stage4.data.iteration := stage3.data.iteration + 1
  }
  when(stage3.data.iteration >= 62 | stage3.data.zXzX + stage3.data.zYzY >= 4.0) {
    stage4.data.done := True
  }

  //End Stage put to the output the result if it's finished and in right order
  //          else put it into the feedback to redo iteration or to waiting
  val readyForresult = stage4.data.done && resultOrder === stage4.data.order

  val result = p.pixelResult
  result.valid := stage4.valid && readyForresult
  result.data := stage4.data.iteration

  when(result.fire) {
    resultOrder := resultOrder + 1
  }

  result >-> io.result

  loopBack.valid := stage4.valid && ((!readyForresult) || (!result.ready))
  loopBack.data := stage4.data

}


object Main {
  def main(args: Array[String]) {
    SpinalVhdl(new FrameTaskSolver(new FrameTaskSolver.Parameter(4, 64, 64, 7, 36)))
  }
}





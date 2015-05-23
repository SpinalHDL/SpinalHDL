//package spinal.demo.mandelbrot
//
//import spinal.core._
//import spinal.lib._
//
//
//case class SFix2D(exp : Int,bitCount : Int){
//  val x = SFix(exp,bitCount)
//  val y = SFix(exp,bitCount)
//}
//
//object Mandelbrot {
//  class Parameter(val unitCount: Int, val screenResX: Int, val screenResY: Int, val fixExp: Int, val fixWidth: Int) {
//    def fix = SInt(fixWidth bit)
//    def job = Job(4,)
//    def toFix(value: Float) = U((value * (1l << (fixWidth - fixExp))).toLong)
//
//    def resultBits = UInt(8 bit)
//    def resultStream = Handshake(resultBits)
//    def solverTaskStream = Handshake(new Task(this))
//  }
//}
//
//case class Job(exp : Int,bitCount : Int) extends Bundle {
//  val start = SFix2D(exp,bitCount)
//  val inc = SFix2D(exp-8,bitCount+8)
//}
//
//case class Task(p: Mandelbrot.Parameter) extends Bundle {
//  val x = p.fix
//  val y = p.fix
//}
//
//class Mandelbrot(p: Mandelbrot.Parameter) extends Component {
//  val io = new Bundle {
//    val job = slave Handshake (Job())
//    val result = master(p.resultStream.asMaster)
//  }
//
//  val taskGenerator = new TaskGenerator(p)
//  taskGenerator.io.job << io.job
//
//  val taskDispatcher = new DispatcherInOrder(new Task(p), p.unitCount)
//  taskDispatcher.io.input <-/< taskGenerator.io.solverTask
//
//  val solvers = List.fill(p.unitCount)(new SolverPipelined(p))
//  (solvers, taskDispatcher.io.output).zipped.map(_.io.task <-/< _)
//
//  val result_arbiter = HandshakeArbiter.inOrder.build(p.resultBits, p.unitCount)
//  (solvers, result_arbiter.io.inputs).zipped.map(_.io.result >/> _)
//
//  result_arbiter.io.output >-> io.result
//}
//
//
//class TaskGenerator(p: Mandelbrot.Parameter) extends Component {
//  val io = new Bundle {
//    val job = slave Handshake (Job())
//    val solverTask = master(p.solverTaskStream)
//  }
//
//  val screenX = Reg(UInt(11 bit))
//  val screenY = Reg(UInt(11 bit))
//  val mandelbrotX = Reg(SInt(32 + 10 bit))
//  val mandelbrotY = Reg(SInt(32 + 10 bit))
//  val setup = RegInit(True)
//
//  val solverTask = Handshake(Task(p))
//
//  io.job.ready := !io.job.valid
//
//
//  when(io.job.ready) {
//    setup := True
//  }
//
//  when(io.job.valid && solverTask.ready) {
//    when(setup) {
//      setup := False
//      screenX := 0
//      screenY := 0
//      mandelbrotX := io.job.data.xStart << 10
//      mandelbrotY := io.job.data.yStart << 10
//    }.otherwise {
//      when(screenX !== p.screenResX - 1) {
//        screenX := screenX + 1
//        mandelbrotX := mandelbrotX + io.job.data.xInc
//      }.otherwise {
//        screenX := 0
//        mandelbrotX := io.job.data.xStart << 10
//        when(screenY !== p.screenResY - 1) {
//          screenY := screenY + 1
//          mandelbrotY := mandelbrotY + io.job.data.yInc
//        }.otherwise {
//          mandelbrotY := io.job.data.yStart << 10
//          io.job.ready := True //Asyncronous acknoledge into syncronous space <3
//        }
//      }
//    }
//  }
//
//  solverTask.valid := io.job.valid && !setup;
//  solverTask.data.x := mandelbrotX >> (10 - (p.fixWidth - p.fixExp - 28));
//  solverTask.data.y := mandelbrotY >> (10 - (p.fixWidth - p.fixExp - 28));
//
//  solverTask >-> io.solverTask
//
//}
//
//class SolverPipelined(p: Mandelbrot.Parameter) extends Component {
//  val io = new Bundle {
//    val task = slave(p.solverTaskStream)
//    val result = master(p.resultStream.asMaster)
//  }
//
//  //It's the context definition used by each stage of the pipeline, Each task are translated to context ("thread")
//  abstract class ContextBase(p: Mandelbrot.Parameter) extends Bundle {
//    val task = new Task(p)
//    val done = Bool
//    val order = UInt(4 bit)
//    //Used to reorder result in same oder than input task
//    val iteration = UInt(9 bit)
//    val zX = p.fix
//    val zY = p.fix
//  }
//
//  case class Context(p: Mandelbrot.Parameter) extends ContextBase(p) {
//
//  }
//
//  //Extended context with x*x   y*y   x*y result
//  case class Stage3Context(p: Mandelbrot.Parameter) extends ContextBase(p) {
//    val zXzX = p.fix
//    val zYzY = p.fix
//    val zXzY = p.fix
//  }
//
//
//  //Self pipelined fixed point multiplication, should be better with ip
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
//
//
//  val insertTaskOrder = Reg(UInt(4 bit)) init (0)
//  val resultOrder = Reg(UInt(4 bit)) init (0)
//
//
//  val stage0 = RegFlow(new Context(p))
//  //First stage
//  val loopBack = RegFlow(new Context(p))
//  //Loop back from end of pipeline
//  val insertTask = new Handshake(new Context(p)) //Task to insert into the pipeline
//
//  //Functional way to translate the input Task to Context
//  /* def taskToContext(task : Task) : Context = {
//       val context = new Context(p)
//     context.task := task
//     context.done := Bool(false)
//     context.order := insertTaskOrder;
//     context.iteration := UInt(0)
//     context.zX := task.x
//     context.zY := task.y
//     return context
//   }
//   (io.task ~~ taskToContext) >> insertTask*/
//  //end of functional way to do that
//
//  //Hardcoded way to translate the input Task to Context
//  val insertTaskBits = new Context(p)
//  insertTaskBits.task := io.task.data
//  insertTaskBits.done := Bool(false)
//  insertTaskBits.order := insertTaskOrder;
//  insertTaskBits.iteration := 0
//  insertTaskBits.zX := io.task.data.x
//  insertTaskBits.zY := io.task.data.y
//
//  (io.task ~ insertTaskBits) >> insertTask
//  //((io.task ~ insertTaskBits) & (insertTaskOrder === resultOrder)) >> insertTask //same than precedent line but limit the number of "thread" into the pipeline to one
//  //End of hardcoded way to translate the input Task to Context
//
//  when(io.task.fire) {
//    //When a transaction is done on io.task =>
//    insertTaskOrder := insertTaskOrder + 1
//  }
//
//  //begin of the solver pipeline => insert contexts from loopback or insertTask
//  stage0.valid := loopBack.valid || insertTask.valid
//  insertTask.ready := !loopBack.valid
//  stage0.data := Mux(loopBack.valid, loopBack.data, insertTask.data)
//
//  //Stage1 is a routing stage
//  val stage1b = RegFlow(new Context(p))
//  stage1b := stage0
//  val stage1 = RegFlow(new Context(p))
//  stage1 := stage1b
//
//  //Stage2 is when x*x  y*y and x*y are done with function fixMul
//  val stage2b = RegFlow(new Context(p))
//  stage2b := stage1
//  val stage2bb = RegFlow(new Context(p))
//  stage2bb := stage2b
//  val stage2 = RegFlow(new Context(p))
//  stage2 := stage2bb
//
//  //Stage3 get multiplication result
//  val stage3 = RegFlow(new Stage3Context(p))
//  stage3 assignSomeByName stage2
//
//  stage3.data.zXzX := fixMul(stage1.data.zX, stage1.data.zX)
//  stage3.data.zYzY := fixMul(stage1.data.zY, stage1.data.zY)
//  stage3.data.zXzY := fixMul(stage1.data.zX, stage1.data.zY)
//
//  //Stage4 calculate next position of the iteration (zX,zY)
//  //       Increment the iteration count if was not done
//  //       calculate if the "Thread" has rush a end condition (iteration > N, x*x+y*y + 4)
//  val stage4 = RegFlow(new Context(p))
//  stage4 assignSomeByName stage3
//
//  stage4.data.zX := stage3.data.zXzX - stage3.data.zYzY + stage3.data.task.x
//  stage4.data.zY := (stage3.data.zXzY << 1) + stage3.data.task.y
//  when(!stage3.data.done) {
//    stage4.data.iteration := stage3.data.iteration + 1
//  }
//  when(stage3.data.iteration >= 62 | stage3.data.zXzX + stage3.data.zYzY >= BigInt(4l << (p.fixWidth - p.fixExp))) {
//    stage4.data.done := Bool(true)
//  }
//
//  //End Stage put to the output the result if it's finished and in right order
//  //          else put it into the feedback to redo iteration or to waiting
//  val readyForresult = stage4.data.done && resultOrder === stage4.data.order
//
//  val result = p.resultStream
//  result.valid := stage4.valid && readyForresult
//  result.data := stage4.data.iteration
//
//  when(result.fire) {
//    resultOrder := resultOrder + 1
//  }
//
//  result >-> io.result
//
//  loopBack.valid := stage4.valid && ((!readyForresult) || (!result.ready))
//  loopBack.data := stage4.data
//
//}
//
//
//object Main {
//  def main(args: Array[String]) {
//    SpinalVhdl(new Mandelbrot(new Mandelbrot.Parameter(4, 64, 64, 7, 36)))
//  }
//}
//
//
//
//

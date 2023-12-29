package spinal.lib.misc.test

import org.apache.commons.io.FileUtils
import org.apache.commons.io.output.TeeOutputStream

import java.io.{File, OutputStream, PrintStream, PrintWriter}
import java.util.concurrent.ForkJoinPool
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}


class AsyncJob(toStdout: Boolean, val logsPath : File)(body: => Unit)(implicit @deprecatedName('execctx) executor: ExecutionContext) {
  FileUtils.forceMkdir(logsPath)
  val logsFile = new File(logsPath, "stdout.log")
  val file = new PrintStream(logsFile)
  val originalOutput = Console.out
  var failed = false
  val stdout = if (toStdout) new TeeOutputStream(Console.out, file) else file
  val future = Future {
    Console.withOut(stdout) {
      Console.withErr(stdout)(
        try {
          body
        } catch {
          case e: Throwable => {
            failed = true
            onFail()
            println(e.getMessage)
            println(e.getStackTrace.map(_.toString).mkString("\n"))
            Console.out.flush()
            Console.err.flush()
            Thread.sleep(50)
          }
        }
      )
    }
//    if (failed) println(s"Failure")
  }

  def onFail(): Unit = {

  }

  def join(): Unit = {
    Await.result(future, Duration.Inf)
    file.flush()
    file.close()
  }
}

class MultithreadedTester(threadCount : Int = 0, workspace : File = new File("logs")){
  val finalThreadCount = if(threadCount > 0) threadCount else {
    new oshi.SystemInfo().getHardware.getProcessor.getLogicalProcessorCount
  }
  implicit val ec = ExecutionContext.fromExecutorService(
    new ForkJoinPool(finalThreadCount, ForkJoinPool.defaultForkJoinWorkerThreadFactory, null, true)
  )

  val jobs = ArrayBuffer[AsyncJob]()


  def test(testName: String, toStdout : Boolean = false)(testFun: => Unit) {
    jobs += new AsyncJob(toStdout, new File(workspace, testName))(testFun)
  }

  def await(): Unit = {
    jobs.foreach(_.join())
    jobs.foreach(j => if(j.failed) throw new Exception(s"Some jobs failed, see ${workspace.getAbsolutePath}"))
    jobs.clear()
  }
}

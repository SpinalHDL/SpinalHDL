package spinal.lib.misc.test

import org.apache.commons.io.FileUtils

import java.io.{File, OutputStream, PrintStream, PrintWriter}
import java.util.concurrent.ForkJoinPool
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}


class MultithreadedTester(threadCount : Int = 0, workspace : File = new File("logs")){
  val finalThreadCount = if(threadCount > 0) threadCount else {
    new oshi.SystemInfo().getHardware.getProcessor.getLogicalProcessorCount
  }
  implicit val ec = ExecutionContext.fromExecutorService(
    new ForkJoinPool(finalThreadCount, ForkJoinPool.defaultForkJoinWorkerThreadFactory, null, true)
  )

  val jobs = ArrayBuffer[Job]()
  class Job(testName : String) (body : => Unit){
    val logsPath = new File(workspace, testName)
    FileUtils.forceMkdir(logsPath)
    val file = new PrintStream(new File(logsPath,"stdout.log"))
    val originalOutput = Console.out
    var failed = false
    val future = Future{
      Console.withOut(file){
        Console.withErr(file)(
          try {
            body
          } catch {
            case e: Throwable => {
              failed = true
              println(e.getMessage)
              println(e.getStackTrace.map(_.toString).mkString("\n"))
              Console.out.flush()
              Console.err.flush()
              Thread.sleep(50)
            }
          }
        )
      }
      if(failed){
        println(s"$testName failed")
      }
    }

    def join(): Unit = {
      Await.result(future, Duration.Inf)
      file.flush()
      file.close()
    }
  }

  def test(testName: String)(testFun: => Unit) {
    jobs += new Job(testName)(testFun)
  }

  def await(): Unit = {
    jobs.foreach(_.join())
    jobs.foreach(j => if(j.failed) throw new Exception(s"Some jobs failed, see ${workspace.getAbsolutePath}"))
    jobs.clear()
  }
}

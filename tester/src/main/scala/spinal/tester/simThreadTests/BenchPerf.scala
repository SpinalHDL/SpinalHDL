package spinal.tester.simThreadTests

import java.util.concurrent.locks.LockSupport
import java.util.concurrent.{CyclicBarrier, Semaphore}

import net.openhft.affinity.Affinity


class NativeThread{
  def info(str : String) = {}
//  def info(str : String) = println(str)
  var i = 0



  val barrier = new CyclicBarrier(2)
  var sync = false
  val thread = new Thread (){
    override def run(): Unit = {
      barrier.await()
      while(true){
        i += 1
        NativeThread.this.suspend()
      }
    }
  }.start()

  def resume(): Unit ={
    barrier.await()
    barrier.await()
  }

  def suspend(): Unit ={
    barrier.await()
    barrier.await()
  }


//  val master = Thread.currentThread()
//
//  val thread = new Thread (){
//    override def run(): Unit = {
//      Affinity.setAffinity(1)
//      LockSupport.park()
//      while(true){1
//        i += 1
//        NativeThread.this.suspend()
//      }
//    }
//  }
//  thread.start()
//
//  def resume(): Unit ={
//    LockSupport.unpark(thread)
//    LockSupport.park()
//  }
//
//  def suspend(): Unit ={
//    LockSupport.unpark(master)
//    LockSupport.park()
//  }





  //  val startLock = new Object()
//  var endLock = false


//  val thread = new Runnable(){
//    override def run(): Unit = {
//      startLock.wait()
//      while(true){
//        i += 1
//        suspend()
//      }
//    }
//  }
//
//  def resume(): Unit ={
//    endLock = true
//    startLock.notifyAll()
//    while(endLock){}
//  }
//
//  def suspend(): Unit ={
//    endLock = true
//    startLock.wait()
//  }
}


object BenchPerf extends App{
  Affinity.setAffinity(1)
  for(repeat <- 0 until 1000) {
    val t1 = new NativeThread
    var i = 0
    val iteration = 100000
    val startAt = System.nanoTime()
    while (i < iteration) {
      t1.resume()
      i += 1
    }
    val endAt = System.nanoTime()
    val delta = (endAt - startAt)
    println(s"done in ${delta / iteration} ns/iteration")
  }
  System.exit(0)
}


object TestAffinity extends App{
  for(repeat <- 0 to 3) {
    val t = new Thread(){
      override def run(): Unit = {
        Affinity.setAffinity(repeat%3*2)
        while(true){}
      }
    }

    t.start()
  }
  Thread.sleep(100000)
}


object TestStates extends App{
  val t = new Thread(){
    override def run(): Unit = {
      val startAt = System.currentTimeMillis()
      while(System.currentTimeMillis() - startAt < 1000){}
      Thread.sleep(1000)
      println("t end")
    }
  }
  println(t.isDaemon)
  t.start()
  while(true) {
//    println(t.interrupt()isAlive)
//    Thread.sleep(400)
  }
}



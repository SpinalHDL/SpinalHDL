package spinal.tester

import spinal.core._
import spinal.lib._


object PlayDebug{



  def main(args: Array[String]): Unit = {
    try{
      println("throw")
      throw new Exception("asd")
    }/* catch {
      case e : Exception => {
        println("exception")
        throw new Exception("Rawrrr")
      }
    } */finally {
      println("finaly")
    }
  }
}

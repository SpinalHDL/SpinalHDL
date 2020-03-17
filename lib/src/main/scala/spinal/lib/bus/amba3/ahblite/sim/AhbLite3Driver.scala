package spinal.lib.bus.amba3.ahblite.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba3.ahblite.AhbLite3

import scala.util.Random

class AhbLite3Driver(bus: AhbLite3, clockdomain: ClockDomain){


  def drive(transactions: Seq[AhbLite3Transaction]): Unit ={

      if (transactions.length == 1) {

        bus.HSEL    #= true
        bus.HADDR   #= transactions.head.haddr
        bus.HTRANS  #= transactions.head.htrans
        bus.HBURST  #= transactions.head.hburst
        bus.HWRITE  #= transactions.head.hwrite

        clockdomain.waitActiveEdge(1)

        bus.HSEL    #= false
        bus.HTRANS  #= 0
        bus.HADDR   #= 0
        bus.HBURST  #= 0
        if (transactions.head.hwrite)  bus.HWDATA  #= transactions.head.hwdata

        clockdomain.waitSamplingWhere(bus.HREADYOUT.toBoolean)

      } else {

        for ((trans, index) <- transactions.zipWithIndex) {
          bus.HSEL   #= true
          bus.HADDR  #= trans.haddr
          bus.HTRANS #= trans.htrans
          bus.HBURST #= trans.hburst
          bus.HWRITE #= trans.hwrite

          if(index > 0){
            clockdomain.waitActiveEdgeWhere(bus.HREADYOUT.toBoolean)
          }else{
            clockdomain.waitActiveEdge(1)
          }
          if (trans.hwrite)  bus.HWDATA #= trans.hwdata
        }

        bus.HSEL   #= false
        bus.HTRANS #= 0
        bus.HADDR  #= 0
        bus.HBURST #= 0

        clockdomain.waitActiveEdgeWhere(bus.HREADYOUT.toBoolean)

        bus.HWDATA #= 0
      }
    }



  /**
    * Basic response of a slave
    */
  def slaveSink(): Unit = {
    val dummy = fork{
      while(true){
        bus.HREADYOUT #= true
        clockdomain.waitActiveEdgeWhere(bus.HSEL.toBoolean && bus.HTRANS.toInt != 0 )
        if (!bus.HWRITE.toBoolean){
          bus.HRDATA.randomize()
        }
      }
    }
  }


  /**
    * Generate randomly some Wait
    */
  def slaveRandomWait(): Unit = {
    val dummy = fork{
      while(true){
        bus.HREADYOUT #= true
        clockdomain.waitActiveEdgeWhere(bus.HSEL.toBoolean & bus.HTRANS.toInt == 2)
        val waitCycle = Random.nextInt(3)
        for(_ <- 0 until waitCycle){
          bus.HREADYOUT #= false
          clockdomain.waitActiveEdge()
        }
        if (!bus.HWRITE.toBoolean){
          bus.HRDATA.randomize()
        }
      }
    }
  }

  /**
    * Generate randomly error
    */
  def slaveRandomError(): Unit = {
    val dummy = fork{
      while(true){
        bus.HREADYOUT #= true
        bus.HRESP     #= false
        clockdomain.waitActiveEdgeWhere(bus.HSEL.toBoolean & bus.HTRANS.toInt == 2)
        bus.HREADYOUT #= false
        bus.HRESP     #= true  // error
        clockdomain.waitActiveEdge()
        bus.HRESP     #= true
        bus.HREADYOUT #= true
        clockdomain.waitActiveEdge()
      }
    }
  }


}

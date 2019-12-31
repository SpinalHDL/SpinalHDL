package spinal.lib.memory.sdram.sdr.sim

import java.nio.file.{Files, Paths}

import spinal.core.sim._
import spinal.core._
import spinal.lib.memory.sdram.SdramLayout
import spinal.lib.memory.sdram.sdr.SdramInterface


case class SdramModel(io : SdramInterface,
                      layout : SdramLayout,
                      clockDomain : ClockDomain){
  var CAS = 0
  var burstLength = 0
  val banks = Array.fill(layout.bankCount)(new Bank)

  var ckeLast = false
  val readShifter = new Array[Byte](layout.bankCount*3)

  def write(address : Int, data : Byte): Unit = {
    val byteId = address & (layout.bytePerWord-1)
    val word = address / layout.bytePerWord
    val row = word >> layout.columnWidth + layout.bankWidth
    val bank = (word >> layout.columnWidth) & (layout.bankCount-1)
    val col = word & (layout.columnSize-1)
    banks(bank).data((col + row * layout.columnSize) * layout.bytePerWord + byteId) = data
  }

  def loadBin(address : Int, path : String): Unit ={
    val bin = Files.readAllBytes(Paths.get(path))
    for((v,i) <- bin.zipWithIndex) write(address = i + address, data = v)
  }

  class Bank{
    val data = new Array[Byte](layout.capacity.toInt/layout.bankCount)

    var opened = false
    var openedRow = 0

    def activate(row : Int) : Unit = {
      if(opened)
        report("SDRAM error open unclosed bank")
      openedRow = row
      opened = true
    }

    def precharge() : Unit = {
      opened = false
    }

    def write(column : Int, byteId : Int, data : Byte) : Unit = {
      if(!opened)
        report("SDRAM : write in closed bank")
      val addr = byteId + (column + openedRow * layout.columnSize) * layout.bytePerWord
      //printf("SDRAM : Write A=%08x D=%02x\n",addr,data);
      this.data(addr) = data
    }

    def read(column : Int, byteId : Int) : Byte = {
      if(!opened)
        report("SDRAM : read in closed bank")
      val addr = byteId + (column + openedRow * layout.columnSize) * layout.bytePerWord
//      printf("SDRAM : Read A=%08x D=%02x\n",addr,data(addr));
      return data(addr);
    }
  }
  def report(msg : String) = println(simTime() + " " + msg)
  clockDomain.onSamplings{
    if(!io.CSn.toBoolean && ckeLast){
      val code = (if(io.RASn.toBoolean) 0x4 else 0) | (if(io.CASn.toBoolean) 0x2 else 0) | (if(io.WEn.toBoolean) 0x1 else 0)
      val ba = io.BA.toInt
      val addr = io.ADDR.toInt
      code match {
        case 0 =>  //Mode register set
          if(ba == 0 && (addr & 0x400) == 0){
            CAS = ((addr) >> 4) & 0x7
            burstLength = ((addr) >> 0) & 0x7
            if((addr & 0x388) != 0)
              report("SDRAM : ???")
            printf("SDRAM : MODE REGISTER DEFINITION CAS=%d burstLength=%d\n",CAS,burstLength)
          }
        case 2 =>  //Bank precharge
          if((addr & 0x400) != 0){ //all
            for(bankId <- 0 until layout.bankCount) banks(bankId).precharge()
          } else { //single
            banks(ba).precharge()
          }
        case 3 =>  //Bank activate
          banks(ba).activate(addr);
        case 4 =>  //Write
          if((addr & 0x400) != 0)
            report("SDRAM : Write autoprecharge not supported")

          if(io.DQ.writeEnable.toLong == 0)
            report("SDRAM : Write Wrong DQ direction")

          val dqWrite = io.DQ.write.toLong
          val dqm = io.DQM.toInt
          for(byteId <- 0 until layout.bytePerWord){
            if(((dqm >> byteId) & 1) == 0)
              banks(ba).write(addr, byteId ,(dqWrite >> byteId*8).toByte);
          }

        case 5 =>  //Read
          if((addr & 0x400) != 0)
            report("SDRAM : READ autoprecharge not supported")

          if(io.DQ.writeEnable.toLong != 0)
            report("SDRAM : READ Wrong DQ direction")

          //if(io.DQM !=  config->byteCount-1)
          //println("SDRAM : READ wrong DQM")

          for(byteId <- 0 until layout.bytePerWord){
            readShifter(byteId) = banks(ba).read(addr, byteId);
          }
        case 1 =>  // Self refresh
        case 7 =>  // NOP
        case _ =>
          report("SDRAM : unknown code")
      }
    }
    ckeLast = io.CKE.toBoolean;

    if(CAS >= 2 && CAS <=3){
      var readData = 0l
      for(byteId <- 0 until layout.bankCount){
        readData |= (readShifter(byteId + (CAS-1)*layout.bytePerWord) & 0xFF).toLong << byteId*8;
      }
      io.DQ.read #= readData
      for(latency <-  CAS-1 downto 1){  //missing CKE
        for(byteId <- 0 until layout.bytePerWord){
          readShifter(byteId+latency*layout.bytePerWord) = readShifter(byteId+(latency-1)*layout.bytePerWord);
        }
      }
    }
  }
}
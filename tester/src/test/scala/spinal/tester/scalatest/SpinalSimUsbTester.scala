package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.com.eth._
import spinal.lib.com.usb.phy.{UsbHubLsFs, UsbLsFsPhy}
import spinal.lib.sim.{StreamDriver, StreamMonitor, StreamReadyRandomizer}

import scala.collection.mutable
import scala.util.Random

class SpinalSimUsbTester extends FunSuite{
//  implicit class UsbLsFsCtrlPimper(self : UsbHubLsFs.Ctrl)(implicit cd : ClockDomain){
//    def init(): Unit = {
//      self.tx.kind #= UsbHubLsFs.TxKind.NONE
//      cd.waitSampling(10)
//    }
//
//    def issue(cmd : UsbHubLsFs.TxKind.E): Unit ={
//      self.tx.kind #= cmd
//      cd.waitSamplingWhere(self.tx.ready.toBoolean)
//      self.tx.kind #= UsbHubLsFs.TxKind.NONE
//    }
//
//    def reset(): Unit ={
//      issue(UsbHubLsFs.TxKind.RESET)
//    }
//    def suspend(): Unit ={
//      issue(UsbHubLsFs.TxKind.SUSPEND)
//    }
//    def resume(): Unit ={
//      issue(UsbHubLsFs.TxKind.RESUME)
//    }
//    def packet(bytes : Seq[Int]): Unit ={
//      for((byte, i) <- bytes.zipWithIndex) {
//        self.tx.data #= byte
//        self.tx.last #= i == bytes.length-1
//        issue(UsbHubLsFs.TxKind.PACKET)
//      }
//    }
//
//  }

//  test("UsbLsFsPhy"){
//    SimConfig.withFstWave.compile(UsbLsFsPhy(4)).doSim{dut =>
//      implicit val cd = dut.clockDomain
//      cd.forkStimulus(20833)
//
//      dut.io.ctrl.fullSpeed #= true
//      dut.io.ctrl.init()
//      dut.io.ctrl.reset()
//      cd.waitSampling(100)
//      dut.io.ctrl.suspend()
//      cd.waitSampling(100)
//      dut.io.ctrl.resume()
//      cd.waitSampling(100)
//      dut.io.ctrl.packet(List(0x00, 0xAA, 0xAA, 0x55, 0x55, 0x00, 0xFF, 0xFF))
//      dut.io.ctrl.packet(List(0x00, 0xAA, 0xAA, 0x55, 0x55, 0x00, 0xFF, 0xFF))
//      cd.waitSampling(100)
//    }
//  }
}

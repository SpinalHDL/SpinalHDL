//package spinal.lib.com.usb.phy
//
//import spinal.core._
//import spinal.lib._
//import spinal.lib.com.usb.phy.UsbLsFs.RxKind
//import spinal.lib.io.TriState
//
//
//
//case class UsbPhyFsNativeIo() extends Bundle with IMasterSlave {
//  val dp,dm = TriState(Bool)
//
//  override def asMaster(): Unit = {
//    master(dp,dm)
//  }
//}
//
//
//
//case class UsbLsFsPhyAbstractIo() extends Bundle with IMasterSlave {
//  val tx = new Bundle {
//    val enable = Bool()
//    val data = Bool()
//    val se0 = Bool()
//  }
//  val rx = new Bundle {
//    val dp = Bool()
//    val dm = Bool()
//    val rcv = Bool()
//  }
//
//  override def asMaster(): Unit = {
//    out(tx)
//    in(rx)
//  }
//}
//
//case class UsbLsFsPhyFilter(fsRatio : Int) extends Component {
//  val io = new Bundle {
//    val fullSpeed = in Bool()
//
//    val usb = new Bundle {
//      val dp = in Bool()
//      val dm = in Bool()
//    }
//
//    val filtred = new Bundle {
//      val dp, dm, d = out Bool()
//      val sample = out Bool()
//    }
//  }
//
//  val frontend = new Area{
//    val valid = io.usb.dp =/= io.usb.dm
//    val dp = io.usb.dp
//    val dm = io.usb.dm
//    val value = dp
//    val valueOld = RegNextWhen(value, valid)
//    val edge = value ^ valueOld
//  }
//
//  val timer = new Area{
//    val clear = False
//    val counter = Reg(UInt(log2Up(fsRatio*8) bits))
//    counter := counter + 1
//    when(counter === fsRatio-1 || clear){
//      counter := 0
//    }
//
//    val sampleAt = io.fullSpeed ? U(fsRatio/2) | U(fsRatio*8/2)
//    val sampleDo = counter === sampleAt
//
//    when(frontend.valid & frontend.edge){
//      clear := True
//    }
//  }
//
//  io.filtred.dp := frontend.dp
//  io.filtred.dm := frontend.dm
//  io.filtred.d := frontend.value
//  io.filtred.sample := timer.sampleDo
//}
//
//case class UsbLsFsPhy(fsRatio : Int) extends Component{
//  val io = new Bundle{
//    val ctrl = slave(UsbLsFs.Ctrl())
//    val usb = master(UsbLsFsPhyAbstractIo())
//  }
//
//
//  val lsRatio = fsRatio*8
//
//  class Timer(counterTimeMax : Double) extends  Area {
//    val counter = Reg(UInt((log2Up(12e6 * fsRatio * counterTimeMax toInt) + 2) bits))
//    val clear = False
//    counter := counter + 1
//    when(clear) {
//      counter := 0
//    }
//
//    def trigger(t: Double): Bool = {
//      val at = (12e6 * fsRatio * t toInt) - 1
//      counter === at
//    }
//
//    def cycles(c: Int): Bool = {
//      val ls = c * lsRatio - 1
//      val fs = c * fsRatio - 1
//      counter === (io.ctrl.fullSpeed ? U(fs) | U(ls))
//    }
//  }
//
//  val j = CombInit(io.ctrl.fullSpeed)
//  val k = !io.ctrl.fullSpeed
//
//  val tx = new Area {
//    val timer = new Timer(counterTimeMax = 20e-3 * 1.1) {
//      val reset = trigger(10e-3 * 1.1)
//      val suspend = trigger(3e-3 * 1.1)
//      val resume = trigger(20e-3 * 1.1)
//      val oneCycle = cycles(1)
//      val twoCycle = cycles(2)
//    }
//
//    val state = Reg(UInt(2 bits))
//
//
//
//    io.usb.tx.enable := False
//    io.usb.tx.se0 := False
//    io.usb.tx.data := False
//    io.ctrl.tx.ready := False
//
//    val eop = new Area {
//      val valid = False
//      val done = False
//      val state = Reg(UInt(2 bits))
//
//      val clear = False
//      when(clear) {
//        state := 0
//      }
//      when(valid) {
//        switch(state) {
//          is(0) {
//            io.usb.tx.enable := True
//            io.usb.tx.se0 := True
//            when(timer.twoCycle) {
//              timer.clear := True
//              state := 1
//            }
//          }
//          is(1) {
//            io.usb.tx.enable := True
//            io.usb.tx.data := False
//            when(timer.oneCycle) {
//              timer.clear := True
//              state := 2
//            }
//          }
//          is(2) {
//            when(timer.oneCycle) {
//              timer.clear := True
//              state := 0
//              done := True
//            }
//          }
//        }
//      }
//    }
//
//    val encoder = new Area {
//      val valid = False
//      val done = False
//      val data = Bool().assignDontCare()
//      val counter = Reg(UInt(3 bits))
//      val toggle = Reg(Bool)
//
//      val clear = False
//      when(clear) {
//        counter := 0
//        toggle := False
//      }
//
//      when(valid) {
//        io.usb.tx.enable := True
//        when(counter === 5) {
//          io.usb.tx.data := !toggle ^ j
//          when(timer.oneCycle) {
//            counter := 0; toggle := !toggle; timer.clear := True
//          }
//        } otherwise {
//          when(data) {
//            io.usb.tx.data := toggle ^ j
//            when(timer.oneCycle) {
//              counter := counter + 1; done := True; timer.clear := True
//            }
//          } otherwise {
//            io.usb.tx.data := !toggle ^ j
//            when(timer.oneCycle) {
//              counter := 0; done := True; timer.clear := True; toggle := !toggle
//            }
//          }
//        }
//      }
//    }
//
//    val bitCounter = Reg(UInt(3 bits))
//
//    switch(io.ctrl.tx.kind) {
//      default { //UsbLsFs.TxKind.NONE
//        timer.clear := True
//        encoder.clear := True
//        eop.clear := True
//        state := 0
//        bitCounter := 0
//      }
//      is(UsbLsFs.TxKind.RESET) {
//        io.usb.tx.enable := True
//        io.usb.tx.se0 := True
//        when(timer.reset) {
//          io.ctrl.tx.ready := True
//          timer.clear := True
//        }
//      }
//      is(UsbLsFs.TxKind.SUSPEND) {
//        when(timer.suspend) {
//          io.ctrl.tx.ready := True
//          timer.clear := True
//        }
//      }
//      is(UsbLsFs.TxKind.RESUME) {
//        io.usb.tx.enable := True
//        switch(state) {
//          is(0) {
//            io.usb.tx.data := k
//            when(timer.resume) {
//              state := 1; timer.clear := True
//            }
//          }
//          is(1) {
//            eop.valid := True
//            when(eop.done) {
//              state := 0
//              io.ctrl.tx.ready := True
//            }
//          }
//        }
//      }
//      is(UsbLsFs.TxKind.PACKET) {
//        switch(state) {
//          is(0) {
//            when(io.ctrl.tx.data === 0xA5) {
//              state := 3
//            } otherwise {
//              // sync
//              encoder.valid := True
//              encoder.data := bitCounter === 7
//              when(encoder.done) {
//                bitCounter := bitCounter + 1
//                when(bitCounter === 7) {
//                  state := 1
//                }
//              }
//            }
//          }
//
//          is(1) {
//            // payload
//            encoder.valid := True
//            encoder.data := io.ctrl.tx.data(bitCounter)
//            when(encoder.done) {
//              bitCounter := bitCounter + 1
//              when(bitCounter === 7) {
//                when(!io.ctrl.tx.last) {
//                  io.ctrl.tx.ready := True
//                } otherwise {
//                  state := 2
//                }
//              }
//            }
//          }
//
//          is(2) {
//            // eop
//            eop.valid := True
//            when(eop.done) {
//              state := 0
//              io.ctrl.tx.ready := True
//              encoder.clear := True
//            }
//          }
//
//          is(3) {
//            // low speed SOF
//            when(!io.ctrl.tx.last) {
//              io.ctrl.tx.ready := True
//            } otherwise {
//              eop.valid := True
//              when(eop.done) {
//                state := 0
//                io.ctrl.tx.ready := True
//                encoder.clear := True
//              }
//            }
//          }
//        }
//      }
//    }
//  }
//
//  val rx = new Area{
//    val timer = new Timer(counterTimeMax = 20e-3 * 1.1) {
//      val reset = trigger(10e-3 * 0.9)
//      val suspend = trigger(3e-3 * 0.9)
//      val resume = trigger(20e-3 * 0.9)
//      val oneCycle = cycles(1)
//      val twoCycle = cycles(2)
//    }
//
//
//    val filter = UsbLsFsPhyFilter(fsRatio)
//    filter.io.usb.dp := io.usb.rx.dp
//    filter.io.usb.dm := io.usb.rx.dm
//
//    val decoder = new Area{
//      val state = Reg(Bool) init(False)
//
//      val output = Flow(Bool)
//      output.valid := False
//      output.payload.assignDontCare()
//
//      when(filter.io.filtred.sample){
//        output.valid := True
//        when(state ^ filter.io.filtred.d ^ j){
//          output.payload := False
//          state := !state
//        } otherwise {
//          output.payload := True
//        }
//      }
//    }
//
//    val destuffer = new Area{
//      val counter = Reg(UInt(3 bits)) init(0)
//      val unstuffNext = counter === 6
//
//      val output = decoder.output.throwWhen(unstuffNext).stage()
//
//      when(decoder.output.valid){
//        counter := counter + 1
//        when(!decoder.output.payload || unstuffNext){
//          counter := 0
//        }
//      }
//    }
//
//    val history = new Area{
//      val value = History(destuffer.output.payload, 0 to 7, when = destuffer.output.valid).asBits
//      val updated = CombInit(destuffer.output.valid)
//      val sync = new Area {
//        val pattern = 0x80
//        val hit = updated && value === pattern
//      }
//    }
//
////    val resume = new Area{
////      val minThreshold = io.ctrl.fullSpeed ? U(fsRatio*3) | U(fsRatio*8*3)
////      val counter = Reg(UInt(log2Up(fsRatio*8*3+1) bits)) init(0)
////      val maxHit = counter === maxThreshold
////      val hit = False
////
////      when(!filter.io.filtred.dp && !filter.io.filtred.dm){
////        when(!maxHit) {
////          counter := counter + 1
////        }
////      } otherwise {
////        counter := 0
////      }
////      when(filter.io.filtred.dp === io.ctrl.fullSpeed && filter.io.filtred.dm === !io.ctrl.fullSpeed){
////        when(counter >= minThreshold && !maxHit){
////          hit := True
////        }
////      }
////    }
//
//    val eop = new Area{
//      val minThreshold = io.ctrl.fullSpeed ? U(fsRatio*3) | U(fsRatio*8*3)
//      val maxThreshold = io.ctrl.fullSpeed ? U(fsRatio*2*2/3) | U(fsRatio*8*2*2/3)
//      val counter = Reg(UInt(log2Up(fsRatio*8*3+1) bits)) init(0)
//      val maxHit = counter === maxThreshold
//      val hit = False
//
//      when(!filter.io.filtred.dp && !filter.io.filtred.dm){
//        when(!maxHit) {
//          counter := counter + 1
//        }
//      } otherwise {
//        counter := 0
//      }
//      when(filter.io.filtred.dp === io.ctrl.fullSpeed && filter.io.filtred.dm === !io.ctrl.fullSpeed){
//        when(counter >= minThreshold && !maxHit){
//          hit := True
//        }
//      }
//    }
//
//    val packet = new Area{
//
//      val State = new SpinalEnum{
//        val IDLE, SYNC, PACKET, EOP = newElement()
//      }
//
//      val state = RegInit(State.IDLE)
//      val counter = Reg(UInt(3 bits))
//
//
//      io.ctrl.rx.kind := RxKind.NONE
//      io.ctrl.rx.skip := True
//      io.ctrl.rx.data := history.value
//
//      switch(state){
//        is(State.IDLE){
//          when(history.sync.hit){
//            state := State.PACKET
//            counter := 0
//          }
//        }
//        is(State.PACKET){
//          io.ctrl.rx.kind := RxKind.PACKET
//          when(destuffer.output.valid){
//            counter := counter + 1
//            when(counter === 7){
//              io.ctrl.rx.skip := False
//            }
//          }
//          when(eop.hit){
//            state := State.IDLE
//          }
//        }
//      }
//    }
//  }
//}

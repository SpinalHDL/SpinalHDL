package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._

object BmbExclusiveMonitor{
  def outputParameter(inputParameter : BmbParameter) = inputParameter.copy(canExclusive = false, contextWidth = inputParameter.contextWidth + 1)
}

object BmbExclusiveMonitorState extends SpinalEnum{
  val IDLE, FENCE_START, FENCE_BUSY, EMIT = newElement()
}

//Only LENGTH aligned access for now
//Ensure exclusive ordering across sources by waiting on conflicts
case class BmbExclusiveMonitor(inputParameter : BmbParameter,
                               pendingWriteMax : Int) extends Component{
  import BmbExclusiveMonitorState._
  assert(inputParameter.alignment == BmbParameter.BurstAlignement.LENGTH)
  val io = new Bundle {
    val input = slave(Bmb(inputParameter))
    val output = master(Bmb(BmbExclusiveMonitor.outputParameter(inputParameter)))
  }

  val sourceCount = 1 << inputParameter.sourceWidth

  //Before we create a reservation for a exclusive read, we have to ensure that no pending write could conflict that
  //future reservation (due to potential reordering), so we implement a fence logic to track pending transaction
  val fence = new Area{
    val start = False
    val done = True
    val busy = RegInit(False) clearWhen(done) setWhen(start)
  }

  val exclusiveWriteCancel = False
  val sources = for(sourceId <- 0 until sourceCount) yield new Area{
    val valid = RegInit(False) //Validity of the reservation for the given source
    val exclusiveWritePending = RegInit(False)  //While a exclusive write is ongoing, this is used to block all conflicting writes to ensure atomicity
    val state = RegInit(IDLE)
    val address = Reg(UInt(inputParameter.addressWidth bits))
    val length = Reg(UInt(inputParameter.lengthWidth bits))
    val context = Reg(Bits(inputParameter.contextWidth bits))
    val addressHit = address >> inputParameter.lengthWidth === io.input.cmd.address >> inputParameter.lengthWidth
    val sourceHit = io.input.cmd.source === sourceId
    val haltSource = state =/= IDLE


    when(io.output.rsp.fire && io.output.rsp.source === sourceId && io.output.rsp.context.msb){
      exclusiveWritePending := False
    }

    when(io.input.cmd.valid && io.input.cmd.isRead && io.input.cmd.exclusive){
      when(sourceHit && !haltSource) {
        valid := True
        address := io.input.cmd.address
        length := io.input.cmd.length
        context := io.input.cmd.context
        state := FENCE_START
      }
    }
    when(addressHit && io.input.cmd.lastFire && io.input.cmd.isWrite){
      when(!exclusiveWriteCancel){
        valid := False
      }
      when(sourceHit){
        exclusiveWritePending := True
      }
    }


    val exclusiveReadCmd = Stream(Fragment(BmbCmd(inputParameter.copy(canWrite = false))))
    exclusiveReadCmd.valid := False
    exclusiveReadCmd.opcode := Bmb.Cmd.Opcode.READ
    exclusiveReadCmd.exclusive := True
    exclusiveReadCmd.address := address
    exclusiveReadCmd.length := length
    exclusiveReadCmd.context := context
    exclusiveReadCmd.source := sourceId
    exclusiveReadCmd.last := True

    switch(state){
      is(FENCE_START){
        when(!fence.busy){
          fence.start := True
          state := FENCE_BUSY
        }
      }
      is(FENCE_BUSY){
        when(fence.done){
          state := EMIT
        }
      }
      is(EMIT){
        exclusiveReadCmd.valid := True
        when(exclusiveReadCmd.ready){
          state := IDLE
        }
      }
    }

    val tracker = new Area{
      val cmdCounter, rspCounter = Reg(UInt(log2Up(pendingWriteMax) + 1 bits)) init(0)
      val full = cmdCounter.msb =/= rspCounter.msb && cmdCounter.trim(1) === rspCounter.trim(1)
      when(io.output.cmd.firstFire &&  io.output.cmd.source === sourceId){ cmdCounter := cmdCounter + 1 }
      when(io.output.rsp.firstFire &&  io.output.rsp.source === sourceId){ rspCounter := rspCounter + 1 }


      val target = Reg(UInt(log2Up(pendingWriteMax) + 1 bits))
      val hit = target === rspCounter
      val done = Reg(Bool())
      when(hit){
        done := True
      }
      when(fence.start){
        target := cmdCounter
        done := False
      }

      fence.done clearWhen(!done)
    }
  }

  //output cmd arbitrations
  val exclusiveReadArbiter = StreamArbiterFactory.roundRobin.transactionLock.build(Fragment(BmbCmd(inputParameter.copy(canWrite = false))), sourceCount)
  exclusiveReadArbiter.io.inputs <> Vec(sources.map(_.exclusiveReadCmd))

  val cmdArbiter = StreamArbiterFactory.lowerFirst.fragmentLock.build(Fragment(BmbCmd(inputParameter.copy(canWrite = false))), 2)
  cmdArbiter.io.inputs(0) << exclusiveReadArbiter.io.output

  val inputCmdHalted = io.input.cmd.haltWhen(sources.map(_.haltSource).read(io.input.cmd.source)).throwWhen(io.input.cmd.valid && io.input.cmd.isRead && io.input.cmd.exclusive)
  cmdArbiter.io.inputs(1).arbitrationFrom(inputCmdHalted)
  cmdArbiter.io.inputs(1).payload.assignSomeByName(inputCmdHalted.payload)

  val exclusiveSuccess = sources.map(s => s.valid && s.addressHit).read(io.input.cmd.source)
  io.output.cmd.arbitrationFrom(cmdArbiter.io.output.haltWhen(sources.map(_.tracker.full).orR))
  io.output.cmd.payload.assignSomeByName(cmdArbiter.io.output.payload)
  io.output.cmd.context.removeAssignments() := (io.input.cmd.exclusive && exclusiveSuccess) ## cmdArbiter.io.output.context

  //output cmd data/mask
  io.output.cmd.data := io.input.cmd.data
  io.output.cmd.mask := io.input.cmd.mask
  when(io.input.cmd.exclusive && !exclusiveSuccess){
    io.output.cmd.mask := 0
    exclusiveWriteCancel := True
  }

  //rsp
  io.input.rsp.arbitrationFrom(io.output.rsp)
  io.input.rsp.payload.assignSomeByName(io.output.rsp.payload)
  io.input.rsp.context.removeAssignments() := io.output.rsp.context.resized
  io.input.rsp.exclusive := io.output.rsp.context.msb

  //misc
  if(inputParameter.canInvalidate) {
    io.input.inv <> io.output.inv
    io.input.ack <> io.output.ack
  }
  if(inputParameter.canSync) {
    io.input.sync <> io.output.sync
  }
}

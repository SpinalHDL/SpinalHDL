package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._

object BmbExclusiveMonitor{
  def outputParameter(inputParameter : BmbAccessParameter) = inputParameter.sourcesTransform(s => s.copy(
    canExclusive = false,
    contextWidth = s.contextWidth + 1
  ))
}

object BmbExclusiveMonitorState extends SpinalEnum{
  val IDLE, FENCE_START, FENCE_BUSY, EMIT = newElement()
}

//Only LENGTH aligned access for now
//Ensure exclusive ordering across sources by waiting on conflicts
case class BmbExclusiveMonitor(inputParameter : BmbParameter,
                               pendingWriteMax : Int) extends Component{
  import BmbExclusiveMonitorState._
//  assert(inputParameter.access.aggregated.alignment == BmbParameter.BurstAlignement.LENGTH)
  val outputParameter = BmbExclusiveMonitor.outputParameter(inputParameter.access)
  val io = new Bundle {
    val input = slave(Bmb(inputParameter))
    val output = master(Bmb(outputParameter, inputParameter.invalidation))
  }

  //Before we create a reservation for a exclusive read, we have to ensure that no pending write could conflict that
  //future reservation (due to potential reordering), so we implement a fence logic to track pending transaction
  val fence = new Area{
    val start = False
    val done = True
    val busy = RegInit(False) clearWhen(done) setWhen(start)
  }

  val exclusiveReadParameter = BmbParameter(inputParameter.access.sourcesTransform(_.copy(canWrite = false)))
  val exclusiveWriteCancel = False
  val inputAddressLow = io.input.cmd.address(Bmb.boundaryWidth -1 downto 0)
  val inputAddressLowEnd = inputAddressLow + io.input.cmd.length
  val sources = for(sourceId <- inputParameter.access.sources.keys;
                    sourceParameter = inputParameter.access.sources(sourceId);
                    if sourceParameter.canExclusive) yield new Area {
    val valid = RegInit(False) //Validity of the reservation for the given source
    val exclusiveWritePending = RegInit(False) //While a exclusive write is ongoing, this is used to block all conflicting writes to ensure atomicity
    val state = RegInit(IDLE)
    val address = Reg(UInt(inputParameter.access.addressWidth bits))
    val length = Reg(UInt(sourceParameter.lengthWidth bits))
    val context = Reg(Bits(sourceParameter.contextWidth bits))
    val addressLow = address(Bmb.boundaryWidth -1 downto 0)
    val addressLowEnd = Reg(UInt(Bmb.boundaryWidth bits))
    val addressHitHigh = address >> Bmb.boundaryWidth === io.input.cmd.address >> Bmb.boundaryWidth
    val addressHitLow = addressLow <= inputAddressLowEnd && addressLowEnd >= inputAddressLow
    val addressHit = addressHitLow && addressHitHigh // address >> inputParameter.access.aggregated.lengthWidth === io.input.cmd.address >> inputParameter.access.aggregated.lengthWidth
    val inputSourceHit = io.input.cmd.source === sourceId
    val haltSource = state =/= IDLE


    when(io.output.rsp.fire && io.output.rsp.source === sourceId && io.output.rsp.context.msb) {
      exclusiveWritePending := False
    }

    when(io.input.cmd.valid && io.input.cmd.isRead && io.input.cmd.exclusive) {
      when(inputSourceHit && !haltSource) {
        valid := True
        address := io.input.cmd.address
        length := io.input.cmd.length
        addressLowEnd := inputAddressLowEnd
        context := io.input.cmd.context.resized
        state := FENCE_START
      }
    }
    when(addressHit && io.input.cmd.lastFire && io.input.cmd.isWrite) {
      when(!exclusiveWriteCancel) {
        valid := False
      }
      when(inputSourceHit) {
        exclusiveWritePending := True
      }
    }


    val exclusiveReadCmd = Stream(Fragment(BmbCmd(exclusiveReadParameter)))
    exclusiveReadCmd.valid := False
    exclusiveReadCmd.opcode := Bmb.Cmd.Opcode.READ
    exclusiveReadCmd.exclusive := True
    exclusiveReadCmd.address := address
    exclusiveReadCmd.length := length
    exclusiveReadCmd.context := context.resized
    exclusiveReadCmd.source := sourceId
    exclusiveReadCmd.last := True

    switch(state) {
      is(FENCE_START) {
        when(!fence.busy) {
          fence.start := True
          state := FENCE_BUSY
        }
      }
      is(FENCE_BUSY) {
        when(fence.done) {
          state := EMIT
        }
      }
      is(EMIT) {
        exclusiveReadCmd.valid := True
        when(exclusiveReadCmd.ready) {
          state := IDLE
        }
      }
    }
  }

  val trackers = for(sourceId <- inputParameter.access.sources.keys) yield new Area{
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

  //output cmd arbitrations
  val exclusiveReadArbiter = StreamArbiterFactory.roundRobin.transactionLock.build(Fragment(BmbCmd(exclusiveReadParameter)), sources.size)
  exclusiveReadArbiter.io.inputs <> Vec(sources.map(_.exclusiveReadCmd))

  val cmdArbiter = StreamArbiterFactory.lowerFirst.fragmentLock.build(Fragment(BmbCmd(exclusiveReadParameter)), 2)
  cmdArbiter.io.inputs(0) << exclusiveReadArbiter.io.output

  val inputCmdHalted = io.input.cmd.haltWhen(sources.map(s => s.inputSourceHit && s.haltSource).toSeq.orR).throwWhen(io.input.cmd.valid && io.input.cmd.isRead && io.input.cmd.exclusive)
  cmdArbiter.io.inputs(1).arbitrationFrom(inputCmdHalted)
  cmdArbiter.io.inputs(1).payload.assignSomeByName(inputCmdHalted.payload)

  val exclusiveSuccess = sources.map(s => s.valid && s.addressHit && s.inputSourceHit).toSeq.orR
  io.output.cmd.arbitrationFrom(cmdArbiter.io.output.haltWhen(trackers.map(_.full).toSeq.orR))
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
  if(inputParameter.invalidation.canInvalidate) {
    io.input.inv <> io.output.inv
    io.input.ack <> io.output.ack
  }
  if(inputParameter.invalidation.canSync) {
    io.input.sync <> io.output.sync
  }
}

package spinal.lib.misc.plic

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactory, AllMapping, SingleMapping}

case class PlicMapping(
  gatewayPriorityOffset : Int,
  gatewayPendingOffset  : Int,
  targetEnableOffset    : Int,
  targetThresholdOffset : Int,
  targetClaimOffset     : Int,
  gatewayPriorityShift  : Int,
  gatewayPendingShift    : Int,
  targetThresholdShift  : Int,
  targetClaimShift      : Int,
  targetEnableShift     : Int,
  gatewayPriorityWriteGen : Boolean = true,
  gatewayPriorityReadGen : Boolean,
  gatewayPendingReadGen : Boolean,
  targetThresholdWriteGen : Boolean = true,
  targetThresholdReadGen : Boolean,
  targetEnableWriteGen : Boolean = true,
  targetEnableReadGen : Boolean
)

object PlicMapping{
  def sifive = PlicMapping(
    gatewayPriorityOffset =   0x0000,
    gatewayPendingOffset  =   0x1000,
    targetEnableOffset    =   0x2000,
    targetThresholdOffset = 0x200000,
    targetClaimOffset     = 0x200004,
    gatewayPriorityShift  =       2,
    gatewayPendingShift    =      2,
    targetThresholdShift  =      12,
    targetClaimShift      =      12,
    targetEnableShift     =       7,
    gatewayPriorityReadGen = true,
    gatewayPendingReadGen = true,
    targetThresholdReadGen = true,
    targetEnableReadGen = true
  )


  def light = PlicMapping(
    gatewayPriorityOffset =  0x0000,
    gatewayPendingOffset  =  0x1000,
    targetEnableOffset    =  0x2000,
    targetThresholdOffset =  0xF000,
    targetClaimOffset     =  0xF004,
    gatewayPriorityShift  =       2,
    gatewayPendingShift   =       2,
    targetThresholdShift  =      12,
    targetClaimShift      =      12,
    targetEnableShift     =       7,
    gatewayPriorityReadGen = false,
    gatewayPendingReadGen = false,
    targetThresholdReadGen = false,
    targetEnableReadGen = true
  )
}


object PlicMapper{
  def apply(bus: BusSlaveFactory, mapping: PlicMapping)(gateways : Seq[PlicGateway], targets : Seq[PlicTarget]) = new Area{
    import mapping._
    val gatewayMapping = for(gateway <- gateways) yield new Area{
      if(gatewayPriorityWriteGen && !gateway.priority.hasAssignement) bus.drive(gateway.priority, address = gatewayPriorityOffset + (gateway.id << gatewayPriorityShift)) init(0)
      if(gatewayPriorityReadGen) bus.read(gateway.priority, address = gatewayPriorityOffset + (gateway.id << gatewayPriorityShift))
      if(gatewayPendingReadGen) bus.read(gateway.ip, address = gatewayPendingOffset + (gateway.id << gatewayPendingShift))
    }

    val idWidth = log2Up((gateways.map(_.id) ++ Seq(0)).max + 1)
    val claim = Flow(UInt(idWidth bits))
    claim.valid := False
    claim.payload.assignDontCare()
    when(claim.valid) {
      switch(claim.payload) {
        for (gateway <- gateways) {
          is(gateway.id) {
            gateway.doClaim()
          }
        }
      }
    }

    val completion = Flow(UInt(idWidth bits))
    completion.valid := False
    completion.payload.assignDontCare()
    when(completion.valid) {
      switch(completion.payload) {
        for (gateway <- gateways) {
          is(gateway.id) {
            gateway.doCompletion()
          }
        }
      }
    }


    val coherencyStall = Counter(2)
    when(coherencyStall =/= 0){
      bus.readHalt()
      coherencyStall.increment()
    }
    bus.onReadPrimitive(AllMapping, haltSensitive = false, documentation = ""){
      coherencyStall.increment()
    }
    bus.onWritePrimitive(AllMapping, haltSensitive = false, documentation = ""){
      coherencyStall.increment()
    }

    val targetMapping = for((target, targetId) <- targets.zipWithIndex) yield new Area {
      val thresholdOffset = targetThresholdOffset + (targetId << targetThresholdShift)
      val claimOffset = targetClaimOffset + (targetId << targetClaimShift)
      if(targetThresholdWriteGen && !target.threshold.hasAssignement) bus.drive(target.threshold, address = thresholdOffset) init (0)
      if(targetThresholdReadGen) bus.read(target.threshold, address = thresholdOffset)
      bus.read(target.claim, address = claimOffset)
      bus.onRead(claimOffset) {
        claim.valid := True
        claim.payload := target.claim
      }




      val targetCompletion = bus.createAndDriveFlow(UInt(target.idWidth bits), claimOffset)
      when(targetCompletion.valid){
        completion.valid := True
        completion.payload := targetCompletion.payload
      }

      for ((gateway, gatewayIndex) <- gateways.zipWithIndex) {
        val address = targetEnableOffset + (targetId << targetEnableShift) + bus.busDataWidth/8 * (gateway.id / bus.busDataWidth)
        val bitOffset = gateway.id % bus.busDataWidth
        if(targetEnableWriteGen && !target.ie(gatewayIndex).hasAssignement) bus.drive(target.ie(gatewayIndex), address, bitOffset) init(False)
        if(targetEnableReadGen)  bus.read(target.ie(gatewayIndex),  address, bitOffset)
      }
    }
  }
}

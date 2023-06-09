package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink._
import spinal.lib.system.tag._
import scala.collection.mutable.ArrayBuffer

class CoherencyHub() extends Area{
  val memPut = Node.master()
  val memGet = Node.master()
  val coherents = ArrayBuffer[Node]()
  val blockSize = 64

  def addressWidth = coherents.map(_.m2s.proposed.addressWidth).max

  def createPort() ={
    val ret = Node.slave()
    coherents += ret

    new MemoryConnection{
      override def m = ret
      override def s = internalConnection
      override def offset = 0
      override def mapping = List(SizeMapping(0, BigInt(1) << addressWidth))
      populate()
      override def sToM(down: MemoryTransfers, args: MappedNode) = {
        down match{
          case t : M2sTransfers => {
            val canGet = t.get.contains(blockSize)
            val canPut = t.putFull.contains(blockSize)
            val isMain = args.node.hasTag(PMA.MAIN)
            t.copy(
              acquireT = if(isMain && canGet && canPut) SizeRange(blockSize) else SizeRange.none,
              acquireB = if(isMain && canGet) SizeRange(blockSize) else SizeRange.none
            )
          }
        }
      }
    }
    ret
  }

  val internalConnection = new Nameable with SpinalTagReady {
    override type RefOwnerType = this.type
  }

  List(memGet, memPut).foreach(node => new MemoryConnection {
    override def m = internalConnection
    override def s = node
    override def offset = 0
    override def mapping = List(SizeMapping(0, BigInt(1) << addressWidth))
    override def sToM(downs: MemoryTransfers, args : MappedNode) = downs
    populate()
  })

  val logic = Fiber build new Area{
    val slotsCount = 4
    val cSourceCount = 4
    val dataWidth = coherents.map(_.m2s.proposed.dataWidth).max
    for(node <- coherents){
      node.m2s.supported.loadAsync(
        M2sSupport(
          transfers = node.m2s.proposed.transfers.intersect(
            M2sTransfers(
              acquireT = SizeRange(blockSize),
              acquireB = SizeRange(blockSize),
              get = SizeRange.upTo(blockSize),
              putFull = SizeRange.upTo(blockSize),
              putPartial = SizeRange.upTo(blockSize),
              probeAckData = SizeRange(blockSize)
            )
          ),
          dataWidth = dataWidth,
          addressWidth = addressWidth,
          allowExecute = true
        )
      )

      node.s2m.parameters.load(
        node.m2s.proposed.transfers.withBCE match {
          case false =>  S2mParameters.none
          case true => S2mParameters(List(S2mAgent(
            name = this,
            emits = S2mTransfers(
              probe = SizeRange(blockSize)
            ),
            sinkId = SizeMapping(0, slotsCount)
          )))
        }
      )
      node.s2m.setProposedFromParameters()
    }

    memPut.m2s.parameters.load(
      coherent.CoherentHub.downPutM2s(
        name           = this,
        addressWidth   = addressWidth,
        dataWidth      = dataWidth,
        blockSize      = blockSize,
        slotCount      = slotsCount,
        cSourceCount   = cSourceCount
      )
    )
    memPut.m2s.setProposedFromParameters()
    memPut.s2m.supported.load(S2mSupport.none)

    memGet.m2s.parameters.load(
      coherent.CoherentHub.downGetM2s(
        name           = this,
        addressWidth   = addressWidth,
        dataWidth      = dataWidth,
        blockSize      = blockSize,
        slotCount      = slotsCount
      )
    )
    memGet.m2s.setProposedFromParameters()
    memGet.s2m.supported.load(S2mSupport.none)


    val hub = new coherent.CoherentHub(
      coherent.CoherentHubParameters(
        nodes     = coherents.map(_.bus.p.node),
        slotCount = slotsCount,
        cacheSize = 1024,
        wayCount  = 2,
        lineSize  = blockSize,
        cSourceCount = cSourceCount
      )
    )
    for((s, m) <- hub.io.ups zip coherents) s << m.bus
    hub.io.downGet >> memGet.bus
    hub.io.downPut >> memPut.bus
  }
}

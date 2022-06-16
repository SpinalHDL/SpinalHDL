package spinal.lib.pipeline

import spinal.core._
import spinal.lib.StreamFifoLowLatency


case class ConnectionPoint(valid : Bool, ready : Bool, payload : Seq[Data]) extends Nameable
trait ConnectionLogic extends Nameable with OverridedEqualsHashCode {
  def on(m : ConnectionPoint,
         s : ConnectionPoint,
         flush : Bool, flushNext : Bool, flushNextHit : Bool,
         throwHead : Bool, throwHeadHit : Bool) : Area // Remove => one element, flush =>

  def latency : Int = ???
  def tokenCapacity : Int = ???
  def alwasContainsSlaveToken : Boolean = false
  def withPayload : Boolean = true
}

object Connection{
  case class DIRECT() extends ConnectionLogic {
    def on(m : ConnectionPoint,
           s : ConnectionPoint,
           flush : Bool, flushNext : Bool, flushNextHit : Bool,
           throwHead : Bool, throwHeadHit : Bool) = new Area {
      if(flushNextHit != null) flushNextHit := False
      if(throwHeadHit != null) throwHeadHit := False
      if(m.ready != null) m.ready   := s.ready
      s.valid   := m.valid
      (s.payload, m.payload).zipped.foreach(_ := _)
    }

    override def latency = 0
    override def tokenCapacity = 0
  }

  case class M2S(collapse : Boolean = true,
                 holdPayload : Boolean = false,
                 flushPreserveInput : Boolean = false) extends ConnectionLogic {
    def on(m : ConnectionPoint,
           s : ConnectionPoint,
           flush : Bool, flushNext : Bool, flushNextHit : Bool,
           throwHead : Bool, throwHeadHit : Bool) = new Area{

      s.valid.setAsReg() init(False)
      s.payload.foreach(_.setAsReg())


      m.ready match {
        case null =>
          s.valid := m.valid
          (s.payload, m.payload).zipped.foreach(_ := _)
        case r => {
          if (flush != null && flushPreserveInput) s.valid clearWhen(flush)
          if(throwHead != null) s.valid clearWhen(throwHead)
          when(r) {
            s.valid := m.valid
          }
          when(if (holdPayload) m.valid && r else r) {
            (s.payload, m.payload).zipped.foreach(_ := _)
          }
        }
      }


      if (flush != null && !flushPreserveInput) s.valid clearWhen(flush)
      if(flushNext != null && !flushPreserveInput) s.valid clearWhen(flushNext && s.ready)
      if(flushNextHit != null) flushNextHit := True

    //  assert(!(flushNext != null && flushPreserveInput))

      if(m.ready != null) {
        m.ready := s.ready
        if (collapse) m.ready setWhen (!s.valid)
      }
    }

    override def latency = 1
    override def tokenCapacity = 1
    override def alwasContainsSlaveToken : Boolean = true
  }

  case class S2M() extends ConnectionLogic {
    def on(m : ConnectionPoint,
           s : ConnectionPoint,
           flush : Bool, flushNext : Bool, flushNextHit : Bool,
           throwHead : Bool, throwHeadHit : Bool) = new Area{
      assert(s.ready != null)
      assert(throwHead == null, "not implemented but could be")

      val rValid = RegInit(False) setWhen(m.valid) clearWhen(s.ready)
      val rData = m.payload.map(e => RegNextWhen(e, m.ready).setCompositeName(e, "s2mBuffer"))

      m.ready := !rValid

      s.valid := m.valid || rValid
      when(rValid){
        (s.payload, rData).zipped.foreach(_ := _)
      } otherwise {
        (s.payload, m.payload).zipped.foreach(_ := _)
      }

      if(flush != null) when(flush){
        rValid := False
      }
    }

    override def latency = 1
    override def tokenCapacity = 1
    override def alwasContainsSlaveToken : Boolean = true
  }

  case class QueueLowLatency(depth : Int) extends ConnectionLogic {
    def on(m : ConnectionPoint,
           s : ConnectionPoint,
           flush : Bool, flushNext : Bool, flushNextHit : Bool,
           throwHead : Bool, throwHeadHit : Bool) = new Area{
      assert(s.ready != null)

      val queue = StreamFifoLowLatency(Bits(s.payload.map(widthOf(_)).sum bits), depth)
      queue.io.push.valid := m.valid
      queue.io.push.payload := Cat(m.payload)
      m.ready := queue.io.push.ready

      val offsets = s.payload.scanLeft(0)(_ + widthOf(_))
      s.valid := queue.io.pop.valid
      (s.payload, offsets).zipped.foreach{case (b, o) => b.assignFromBits(queue.io.pop.payload(o, widthOf(b) bits))}
      queue.io.pop.ready := s.ready
      if(throwHead != null) queue.io.pop.ready setWhen(throwHead)

      if(flush != null) queue.io.flush := flush
    }

    override def latency = 1
    override def tokenCapacity = 1
    override def alwasContainsSlaveToken : Boolean = true
  }

}
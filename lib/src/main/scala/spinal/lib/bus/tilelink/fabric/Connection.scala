package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, OffsetTransformer, SizeMapping}
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink
import spinal.lib.system.tag._

import scala.collection.mutable.ArrayBuffer

/**
 * Implementation of ConnectionRaw which allows the automatic insertion of bridges
 */
class Connection(m : NodeUpDown, s : NodeUpDown) extends ConnectionRaw(m, s) {

  //Will negociate the parameters and then connect the ends through the required adapters
  val thread = Fiber build new Area{
    soon(down.m2s.parameters)
    soon(up.s2m.parameters)

    down.m2s.parameters.load(s.m2s.supported join up.m2s.parameters)
    up.s2m.parameters.load(m.s2m.supported join down.s2m.parameters)

    var ptr = up.bus.get
    for(adapter <- adapters){
      if(adapter.isRequired(Connection.this)){
        ptr = adapter.build(Connection.this)(ptr)
      }
    }
    ptr >> down.bus
  }

  
  val adapters = ArrayBuffer[InterconnectAdapter]()
  adapters += new InterconnectAdapterCc()
  adapters += new InterconnectAdapterWidth()

}


class InterconnectAdapterWidth extends InterconnectAdapter{
  var adapter = Option.empty[tilelink.WidthAdapter]

  override def isRequired(c : Connection) = c.m.m2s.parameters.dataWidth != c.s.m2s.parameters.dataWidth
  override def build(c : Connection)(m: Bus) : Bus = {
    val adapter = new tilelink.WidthAdapter(
      ip = m.p,
      op = m.p.copy(dataWidth = c.s.m2s.parameters.dataWidth),
      ctxBuffer = ContextAsyncBufferFull
    )
    adapter.setLambdaName(c.m.isNamed && c.s.isNamed)(s"${c.m.getName()}_to_${c.s.getName()}_widthAdapter")
    this.adapter = Some(adapter)
    adapter.io.up << m
    adapter.io.down
  }
}


trait InterconnectAdapter {
  def isRequired(c : Connection) : Boolean
  def build(c : Connection)(m : Bus) : Bus
}

class InterconnectAdapterCc extends InterconnectAdapter{
  var aDepth = 8
  var bDepth = 8
  var cDepth = 8
  var dDepth = 8
  var eDepth = 8

  var cc = Option.empty[FifoCc]
  override def isRequired(c : Connection) = c.m.clockDomain.clock != c.s.clockDomain.clock
  override def build(c : Connection)(m: Bus) : Bus = {
    val cc = FifoCc(
      busParameter = m.p,
      inputCd      = c.m.clockDomain,
      outputCd     = c.s.clockDomain,
      aDepth       = aDepth,
      bDepth       = bDepth,
      cDepth       = cDepth,
      dDepth       = dDepth,
      eDepth       = eDepth
    )
    cc.setLambdaName(c.m.isNamed && c.s.isNamed)(s"${c.m.getName()}_to_${c.s.getName()}_cc")
    this.cc = Some(cc)
    cc.io.input << m
    cc.io.output
  }
}



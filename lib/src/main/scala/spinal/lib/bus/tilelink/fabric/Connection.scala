package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib.StreamPipe
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, OffsetTransformer, SizeMapping}
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink
import spinal.lib.system.tag._

import scala.collection.mutable.ArrayBuffer

/**
 * Implementation of ConnectionRaw which allows the automatic insertion of bridges
 */
class Connection(m : NodeUpDown, s : NodeUpDown) extends ConnectionRaw(m, s) {
  var upConnection: (Bus, Bus) => Any = null
  var downConnection: (Bus, Bus) => Any = null
  var frequencyRatio = 1.0
  var forceWidthAdapterUp = false
  var forceWidthAdapterDown = false

  def setUpConnection(body: (Bus, Bus) => Any): Unit = upConnection = body
  def setDownConnection(body: (Bus, Bus) => Any): Unit = downConnection = body

  def setUpConnection(a: StreamPipe = StreamPipe.NONE,
                      b: StreamPipe = StreamPipe.NONE,
                      c: StreamPipe = StreamPipe.NONE,
                      d: StreamPipe = StreamPipe.NONE,
                      e: StreamPipe = StreamPipe.NONE): Unit = {
    setUpConnection(_.connectFrom(_)(a, b, c, d, e))
  }

  def setDownConnection(a: StreamPipe = StreamPipe.NONE,
                        b: StreamPipe = StreamPipe.NONE,
                        c: StreamPipe = StreamPipe.NONE,
                        d: StreamPipe = StreamPipe.NONE,
                        e: StreamPipe = StreamPipe.NONE): Unit = {
    setDownConnection(_.connectFrom(_)(a, b, c, d, e))
  }

  // Will negotiate the parameters and then connect the ends through the required adapters.
  val thread = Fiber build new Area {
    soon(down.m2s.parameters)
    soon(up.s2m.parameters)

    down.m2s.parameters.load(s.m2s.supported join up.m2s.parameters)
    up.s2m.parameters.load(m.s2m.supported join down.s2m.parameters)

    if(m.clockDomain.frequency.isInstanceOf[FixedFrequency] && s.clockDomain.frequency.isInstanceOf[FixedFrequency]){
      frequencyRatio = (m.clockDomain.frequency.getValue / s.clockDomain.frequency.getValue).toDouble
    }
    assert(!(forceWidthAdapterUp && forceWidthAdapterDown))

    var ptr = up.bus.get
    val upCon = (upConnection != null) generate {
      val bus = cloneOf(ptr)
      upConnection(bus, ptr)
      ptr = bus
      bus
    }
    for(adapter <- adapters) {
      if(adapter.isRequired(Connection.this)){
        ptr = adapter.build(Connection.this)(ptr)
      }
    }
    val downCon = (downConnection != null) generate {
      val bus = cloneOf(ptr)
      downConnection(bus, ptr)
      ptr = bus
      bus
    }
    ptr >> down.bus
  }

  
  val adapters = ArrayBuffer[InterconnectAdapter]()
  adapters += new InterconnectAdapterWidth(m)
  adapters += new InterconnectAdapterCc()
  adapters += new InterconnectAdapterWidth(s)
}


class InterconnectAdapterWidth(sideNode : NodeUpDown) extends InterconnectAdapter{
  var adapter = Option.empty[tilelink.WidthAdapter]

  override def isRequired(c : Connection) : Boolean = {
    val widthMissmatch = c.m.m2s.parameters.dataWidth != c.s.m2s.parameters.dataWidth
    if(!widthMissmatch) return false
    if(c.forceWidthAdapterUp) return sideNode == c.m
    if(c.forceWidthAdapterDown) return sideNode == c.s

    val mw = c.m.m2s.parameters.dataWidth
    val sw = c.s.m2s.parameters.dataWidth
    val maxBw = Math.min(mw * c.frequencyRatio, sw / c.frequencyRatio)*0.99 //Maximal bandwidth (without adapater)

    // Bandwidth if implemented on the given side.
    val mbw = sw * c.frequencyRatio
    val sbw = mw / c.frequencyRatio

    // If both implementation side bandwidth is big enough
    if(sbw > maxBw && mbw > maxBw) return sideNode == (mw > sw).mux(c.m, c.s)

    // Implement on the side which provide the most bandwidth
    return sideNode == (mbw > sbw).mux(c.m, c.s)
  }
  override def build(c : Connection)(m: Bus) : Bus = sideNode.clockDomain{
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

class InterconnectAdapterCc extends InterconnectAdapter {
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



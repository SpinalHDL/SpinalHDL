package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib.DataCarrier.toImplicit
import spinal.lib._

class WidthAdapter(ip : BusParameter,
                   op : BusParameter,
                   ctxBuffer : ContextAsyncBufferFactory = ContextAsyncBufferFull) extends Component{
  val io = new Bundle {
    val up = slave(Bus(ip))
    val down = master(Bus(op))
  }

  class ChannelDownSizer[T <: BusFragment](src : Stream[T], dst : Stream[T], offset : UInt) extends Area{
    val counter = Reg(offset) init(0)
    val sel = counter + offset
    val burstLast = dst.isLast()

    when(dst.fire){
      counter := (counter + 1).resized
      when(burstLast){
        counter := 0
      }
    }

    dst.valid   := src.valid
    dst.payload := src.payload
    src.ready := dst.ready && (counter.andR || burstLast)
    if(src.withData) dst.data.removeAssignments() := src.data.subdivideIn(1 << widthOf(offset) slices).read(sel)
    if(src.withMask) dst.maskNull.removeAssignments() := src.maskNull.subdivideIn(1 << widthOf(offset) slices).read(sel)
  }

  //When stream is without data mask, sel is allowed to just be a beat counter.
  class ChannelUpSizer[T <: BusFragment](src : Stream[T], dst : Stream[T], sel : UInt) extends Area{
    def ratio = 1 << widthOf(sel)
    val burstLast = src.isLast()
    val wordLast = sel.andR || burstLast

    val buffer = new Area{
      val valid   = RegInit(False)
      val first   = RegInit(True)
      val args    = Reg(ChannelA(ip.copy(withDataA = false)))
      val data    = Vec.fill(ratio)(Reg(src.data))
      val mask    = src.withMask generate Vec.fill(ratio)(Reg(src.maskNull))
      val corrupt = Reg(Bool())
      val denied  = src.withDenied generate Reg(Bool())
      val sink  = src.withSink generate Reg(op.sink)
    }

    dst.valid := buffer.valid
    dst.payload.assignSomeByName(buffer.args)
    if(dst.withMask) dst.maskNull := Cat(buffer.mask)
    dst.data    := Cat(buffer.data)
    dst.corrupt := buffer.corrupt
    if(src.withDenied) dst.deniedNull := buffer.denied
    if(src.withSink) dst.sinkNull := buffer.sink

    buffer.valid clearWhen(dst.ready)
    src.ready := !buffer.valid || dst.ready

    when(src.fire){
      buffer.valid := wordLast
      buffer.first := wordLast
      when(buffer.first) {
        buffer.args.assignSomeByName(src.payload)
        buffer.corrupt := False
        if(src.withDenied) buffer.denied := False
        if(src.withMask) buffer.mask.foreach(_ := 0)
        if(src.withSink) buffer.sink := src.sinkNull
      }

      if(src.withMask) {
        buffer.data(sel) := src.data
        buffer.mask(sel) := src.maskNull
      } else {
        val maskRange = log2Up(src.p.dataBytes)+1 to log2Up(dst.p.dataBytes)
        val mask = maskRange.map(src.size >= _).asBits().asUInt
        for(i <- 0 until ratio){
          when(((sel ^ i) & mask) === 0){
            buffer.data(i) := src.data
          }
        }
      }
      buffer.corrupt setWhen(src.corrupt)
      if(src.withDenied) buffer.denied setWhen(src.deniedNull)
    }
  }

  val direct = (ip.dataWidth == op.dataWidth) generate new Area{
    io.down << io.up
  }

  val upsize = (ip.dataWidth < op.dataWidth) generate new Area{
    val ratio = op.dataWidth / ip.dataWidth
    val addrRange = op.dataBytesLog2Up-1 downto ip.dataBytesLog2Up

    val iaHalt = False
    val ia = io.up.a.haltWhen(iaHalt)

    val a = new Area{
      val ctrl = ia.withData generate new ChannelUpSizer(ia, io.down.a, ia.address(addrRange))
      if(!ia.withData) ia >> io.down.a
    }

    val b = ip.withBCE generate new Area{
      val ctrl = new ChannelDownSizer(io.down.b, io.up.b, io.down.b.address(addrRange))
      io.up.b.address(addrRange) := ctrl.sel
    }

    val c = ip.withBCE generate new Area{
      val ctrl = new ChannelUpSizer(io.up.c, io.down.c, io.up.c.address(addrRange))
    }

    val d = new Area{
      val ctx = ctxBuffer(ip.sourceWidth, UInt(addrRange.size bits))
      ctx.io.bind(iaHalt, io.up.a, io.up.d)
      ctx.io.add.context := io.up.a.address(addrRange)
      ctx.io.query.id := io.down.d.source

      val ctrl = new ChannelDownSizer(io.down.d, io.up.d, ctx.io.query.context)
    }

    val e = ip.withBCE generate new Area{
      io.down.e << io.up.e
    }
  }

  val downsize = (ip.dataWidth > op.dataWidth) generate new Area{
    val ratio = ip.dataWidth / op.dataWidth
    val addrRange = ip.dataBytesLog2Up-1 downto op.dataBytesLog2Up

    val a = new Area{
      val ctrl = new ChannelDownSizer(io.up.a, io.down.a, io.up.a.address(addrRange))
      io.down.a.address(addrRange) := ctrl.sel
    }

    val b = ip.withBCE generate new Area{
      val ctrl = io.up.b.withData generate new ChannelUpSizer(io.down.b, io.up.b, io.down.b.address(addrRange))
      if(!io.up.b.withData) io.up.b << io.down.b
    }

    val c = ip.withBCE generate new Area{
      val ctrl = new ChannelDownSizer(io.up.c, io.down.c, io.up.c.address(addrRange))
      io.down.c.address(addrRange) := ctrl.sel
    }

    val d = new Area{
      val sel = io.down.d.beatCounter().resize(addrRange.size)
      val ctrl = new ChannelUpSizer(io.down.d, io.up.d, sel)
    }

    val e = ip.withBCE generate new Area{
      io.down.e << io.up.e
    }
  }


}

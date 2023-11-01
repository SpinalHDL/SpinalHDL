package spinal.lib.misc

import spinal.core._
import spinal.lib._

/**
  * Pseudo least recently used combinatorial logic
  * io.context.state need to be handled externaly.
  * When you want to specify a access to a entry, you can use the io.update interface
  * to get the new state value.
  */
object Plru{
  def State(entries : Int) = Vec.tabulate(log2Up(entries))(l =>  Bits(1 << l bits))
}
case class Plru(entries : Int, withEntriesValid : Boolean) extends Area{
  assert(isPow2(entries))
  val io = new Bundle{
    val context = new Bundle{
      val state = Plru.State(entries)
      val valids = withEntriesValid generate Bits(entries bits) //Allow to specify prefered entries to remove
    }
    val evict = new Bundle{
      val id =  UInt(log2Up(entries) bits)
    }
    val update = new Bundle{
      val id = UInt(log2Up(entries) bits)
      val state = Plru.State(entries)
    }
  }

  val evict = new Area{
    val sel = Vec.fill(log2Up(entries))(Bool())
    val logic = for(i <- 0 until log2Up(entries)) yield new Area{
      val stateSel = U(B(sel.take(i).reverse))
      val state = io.context.state(i)(stateSel)
      sel(i) := !state

      val validCheck = withEntriesValid generate new Area{
        val groups = io.context.valids.subdivideIn(1 << i slices).map(e => Vec(e.subdivideIn(2 slices).map(!_.orR)))
        val notOks = groups.read(stateSel)
        sel(i) clearWhen(notOks(1)) setWhen(notOks(0))
      }
    }
    io.evict.id := U(sel.reverse.asBits)
  }

  val update = new Area{
    val logic = for(i <- 0 until log2Up(entries)) yield new Area{
      val state = io.update.state(i)
      val sel = io.update.id.takeHigh(i).asUInt
      if(i != 0) state := io.context.state(i)
      state(sel) := io.update.id(log2Up(entries)-i-1)
    }
  }
}

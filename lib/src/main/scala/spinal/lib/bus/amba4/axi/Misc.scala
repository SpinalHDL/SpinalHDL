package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bsb.Bsb



object Axi4ToAxi4Shared{
  def apply(axi : Axi4): Axi4Shared ={
    val axiShared = new Axi4Shared(axi.config)
    val arbiter = StreamArbiterFactory.roundRobin.build(new Axi4Ax(axi.config, axi.config.arwUserWidth),2)
    arbiter.io.inputs(0) << axi.ar.asInstanceOf[Stream[Axi4Ax]]
    arbiter.io.inputs(1) << axi.aw.asInstanceOf[Stream[Axi4Ax]]

    axiShared.arw.arbitrationFrom(arbiter.io.output)
    axiShared.arw.payload.assignSomeByName(arbiter.io.output.payload)
    axiShared.arw.write := arbiter.io.chosenOH(1)
    axi.w >> axiShared.w
    axi.b << axiShared.b
    axi.r << axiShared.r
    axiShared
  }

  def main(args: Array[String]) {
    SpinalVhdl(new Component{
      val axi = slave(Axi4(Axi4Config(32,32,2)))
      val axiShared = master(Axi4ToAxi4Shared(axi))
    })
  }
}

object  Axi4SpecRenamer{
  def apply[T <: Bundle with Axi4Bus](that : T): T ={
    def doIt = {
      that.flatten.foreach((bt) => {
        bt.setName(bt.getName().replace("_payload_",""))
        bt.setName(bt.getName().replace("_valid","valid"))
        bt.setName(bt.getName().replace("_ready","ready"))
        if(bt.getName().startsWith("io_")) bt.setName(bt.getName().replaceFirst("io_",""))
      })
    }
    if(Component.current == that.component)
      that.component.addPrePopTask(() => {doIt})
    else
      doIt

    that
  }

  def apply(that : Bsb): Bsb ={
    def doIt = {
      that.data.overrideLocalName("tdata")
      that.mask.overrideLocalName("tkeep")
      that.source.overrideLocalName("tid")
      that.sink.overrideLocalName("tdest")
      that.last.overrideLocalName("tlast")
      that.flatten.foreach((bt) => {
        bt.setName(bt.getName().replace("payload_",""))
        bt.setName(bt.getName().replace("valid","tvalid"))
        bt.setName(bt.getName().replace("ready","tready"))
        if(bt.getName().startsWith("io_")) bt.setName(bt.getName().replaceFirst("io_",""))
      })
    }
    if(Component.current == that.component)
      that.component.addPrePopTask(() => {doIt})
    else
      doIt

    that
  }

  def main(args: Array[String]) {
    SpinalVhdl(new Component{
      val c = Axi4Config(32,32,4)
      val masters = new Bundle{
        def setup[T <: Bundle with IMasterSlave with Axi4Bus](that : T) = Axi4SpecRenamer(master(that))
        val rw = setup(Axi4(c))
        val ro = setup(Axi4ReadOnly(c))
        val wo = setup(Axi4WriteOnly(c))
        val srw = setup(Axi4Shared(c))
      }
      val slaves = new Bundle{
        def setup[T <: Bundle with IMasterSlave with Axi4Bus](that : T) = Axi4SpecRenamer(slave(that))
        val rw = setup(Axi4(c))
        val ro = setup(Axi4ReadOnly(c))
        val wo = setup(Axi4WriteOnly(c))
        val srw = setup(Axi4Shared(c))
      }

      masters.rw <> slaves.rw
      masters.ro <> slaves.ro
      masters.wo <> slaves.wo
      masters.srw <> slaves.srw

    }.setDefinitionName("TopLevel"))
  }
}



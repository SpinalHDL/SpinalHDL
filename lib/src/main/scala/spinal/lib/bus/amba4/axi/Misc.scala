package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib.{IMasterSlave, slave, master}


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

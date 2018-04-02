package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import scala.collection.mutable

class WishboneInterconFactory(config: WishboneConfig){
  val masters = mutable.ListBuffer[Wishbone]()
  val slaves = mutable.Map[Wishbone,SizeMapping]()

  def addSlave(slave : Wishbone, mapping : SizeMapping): Unit = {
    slaves += (slave -> mapping)
  }

  def addSlaves(slaves : Seq[(Wishbone,SizeMapping)]): Unit = {
    for(slave <- slaves)
      addSlave(slave._1, slave._2)
  }

  def addSlaveWithAdapter(slave : Wishbone,
                          mapping : SizeMapping,
                          allowAddressResize : Boolean = true,
                          allowDataResize : Boolean = false,
                          allowTagResize : Boolean = false): Unit = {
    val adapter = new WishboneAdapter(config,
                                      slave.config,
                                      allowAddressResize,
                                      allowDataResize,
                                      allowTagResize)
    slave <> adapter.io.wbs
    slaves += (adapter.io.wbm -> mapping)
  }

  def addSlavesWithAdapter( slaves : Seq[(Wishbone,SizeMapping)],
                            allowAddressResize  : Boolean = true,
                            allowDataResize     : Boolean = false,
                            allowTagResize      : Boolean = false): Unit = {
    for(slave <- slaves)
      addSlaveWithAdapter(slave._1,
                          slave._2,
                          allowAddressResize,
                          allowDataResize,
                          allowTagResize)
  }

  def addMaster(master : Wishbone) : Unit = {
    masters += master
  }

  def addMasters(masters : Seq[Wishbone]) : Unit = {
    this.masters ++= masters
  }

  def addMasterWithAdapter( master : Wishbone,
                            allowAddressResize  : Boolean = true,
                            allowDataResize     : Boolean = false,
                            allowTagResize      : Boolean = false) : Unit = {
    val adapter = new WishboneAdapter(master.config,
                                      config, allowAddressResize,
                                      allowDataResize,
                                      allowTagResize)
    master <> adapter.io.wbm
    masters += adapter.io.wbs
  }

  def addMastersWithAdapter(masters : Seq[Wishbone],
                            allowAddressResize  : Boolean = true,
                            allowDataResize     : Boolean = false,
                            allowTagResize      : Boolean = false) : Unit = {
    for(master <- masters)
      addMasterWithAdapter( master,
                            allowAddressResize,
                            allowDataResize,
                            allowTagResize)
  }

  def build() = new Area {
    val arbiters = for(slave <- slaves.unzip._1) yield new Area{
      val arbiter = new WishboneArbiter(slave.config, masters.size)
      arbiter.io.output <> slave
      arbiter.setPartialName(slave,"arbiter")
    }

    val decoders = for(master <- masters) yield new Area{
      val decoder = new WishboneDecoder(master.config, slaves.unzip._2.toList)
      decoder.io.input <> master
      decoder.setPartialName(master,"decoder")
    }

    for((arbiter,count_arb) <- (arbiters).zipWithIndex){
      for((decoder,count_dec) <- (decoders).zipWithIndex){
        decoder.decoder.io.outputs(count_arb) <> arbiter.arbiter.io.inputs(count_dec)
      }
    }
  }
}

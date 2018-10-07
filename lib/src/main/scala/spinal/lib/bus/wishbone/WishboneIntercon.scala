package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import scala.collection.mutable

/** This is the slave facotory fot the wishbone bus
  * @param config the wishbone bus configuration fort he intercon
  */
class WishboneInterconFactory(config: WishboneConfig){
  val masters = mutable.ListBuffer[Wishbone]()
  val slaves = mutable.Map[Wishbone,SizeMapping]()

  /** Queue a slave to be connected
    * must have same configuration as the intercon
    * @param slave a slave wishbone bus interface
    * @param mapping the address range that the slave wishbone bus will be mapped to
    */
  def addSlave(slave : Wishbone, mapping : SizeMapping): Unit = {
    slaves += (slave -> mapping)
  }

  /** Queue a list of slaves to be connected
    * must have same configuration as the intercon
    * @param slaves a list of tuples made of the salve bus and his address mapping
    */
  def addSlaves(slaves : Seq[(Wishbone,SizeMapping)]): Unit = {
    for(slave <- slaves)
      addSlave(slave._1, slave._2)
  }

  /** Queue a slave to be connected, add an adapter if needed
    * @param slave a slave wishbone bus interfaces
    * @param mapping the address range that the slave wishbone bus will be mapped to
    * @param allowAddressResize allow to resize the address line, default to true
    * @param allowDataResize allow to resize the data line, default to false
    * @param allowTagResize allow to resize TGA,TGD,TGC line, default to false
    */
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

  /** Queue a list of slaves to be connected, add an adapter if needed
    * @param slaves a list of tuples made of the salve bus and his address mapping
    * @param allowAddressResize allow to resize the address line, default to true
    * @param allowDataResize allow to resize the data line, default to false
    * @param allowTagResize allow to resize TGA,TGD,TGC line, default to false
    */
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
  /** Queue a master to be connected
    * must have same configuration as the intercon
    * @param master a slave wishbone bus interface
    */
  def addMaster(master : Wishbone) : Unit = {
    masters += master
  }

  /** Queue a list of masters to be connected
    * must have same configuration as the intercon
    * @param masters a list of master wishbone bus interface
    */
  def addMasters(masters : Seq[Wishbone]) : Unit = {
    this.masters ++= masters
  }

  /** Queue a master to be connected, add an adapter if needed
    * @param master a masters wishbone bus interfaces
    * @param allowAddressResize allow to resize the address line, default to true
    * @param allowDataResize allow to resize the data line, default to false
    * @param allowTagResize allow to resize TGA,TGD,TGC line, default to false
    */
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

  /** Queue a list of masters to be connected, add an adapter if needed
    * @param masters a list of masters wishbone bus interfaces
    * @param allowAddressResize allow to resize the address line, default to true
    * @param allowDataResize allow to resize the data line, default to false
    * @param allowTagResize allow to resize TGA,TGD,TGC line, default to false
    */
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

  /** Create the wishbone intercon and connect all the masters and slaves to it
    */
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
        decoder.decoder.io.outputs(count_arb) >> arbiter.arbiter.io.inputs(count_dec)
      }
    }
  }
}

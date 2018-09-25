/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Lib                          **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/  MIT Licence                      **
**                                                                           **
** Permission is hereby granted, free of charge, to any person obtaining a   **
** copy of this software and associated documentation files (the "Software"),**
** to deal in the Software without restriction, including without limitation **
** the rights to use, copy, modify, merge, publish, distribute, sublicense,  **
** and/or sell copies of the Software, and to permit persons to whom the     **
** Software is furnished to do so, subject to the following conditions:      **
**                                                                           **
** The above copyright notice and this permission notice shall be included   **
** in all copies or substantial portions of the Software.                    **
**                                                                           **
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS   **
** OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF                **
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.    **
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY      **
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT **
** OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR  **
** THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                **
\*                                                                           */
package spinal.lib.bus.amba3.ahblite

import spinal.core._
import spinal.lib.bus.misc.SizeMapping

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class AhbLite3CrossbarSlaveConnection(master: AhbLite3 /*,priority : Int*/)


case class AhbLite3CrossbarSlaveConfig(mapping: SizeMapping){
  val masters = ArrayBuffer[AhbLite3CrossbarSlaveConnection]()
}


/**
  * AhbLite3 Crossbar Factory
  *
  * @example {{{
  *     class TopLevel extends Component {
  *         val ahbConfig = AhbLite3Config(addressWidth = 16, dataWidth = 32)
  *
  *         val io = new Bundle{
  *             val ahbMasters = Vec(slave(AhbLite3Master(ahbConfig)), 3)
  *             val ahbSlaves  = Vec(master(AhbLite3(ahbConfig)), 4)
  *         }
  *
  *         val crossbar = AhbLite3CrossbarFactory(ahbConfig)
  *             .addSlaves(
  *                 io.ahbSlaves(0) -> (0x1000,0x1000),
  *                 io.ahbSlaves(1) -> (0x3000,0x1000),
  *                 io.ahbSlaves(2) -> (0x4000,0x1000),
  *                 io.ahbSlaves(3) -> (0x5000,0x1000)
  *              )
  *              .addConnections(
  *                 io.ahbMasters(0).toAhbLite3() -> List(ahbSlaves(0), ahbSlaves(1)),
  *                 io.ahbMasters(1).toAhbLite3() -> List(ahbSlaves(1), ahbSlaves(2), ahbSlaves(3)),
  *                 io.ahbMasters(2).toAhbLite3() -> List(ahbSlaves(0), ahbSlaves(3))
  *              )
  *              .build()
  *             }
  *          }}}
  */
case class AhbLite3CrossbarFactory(ahbLite3Config: AhbLite3Config){

  val slavesConfigs = mutable.HashMap[AhbLite3, AhbLite3CrossbarSlaveConfig]()


  def addSlave(ahb: AhbLite3, mapping: SizeMapping): this.type = {
    slavesConfigs(ahb) = AhbLite3CrossbarSlaveConfig(mapping)
    this
  }

  def addSlaves(orders: (AhbLite3, SizeMapping)*): this.type = {
    orders.foreach(order => addSlave(order._1, order._2))
    this
  }

  def addConnection(ahb: AhbLite3, ahbLite3Slave: Seq[AhbLite3]): this.type = {
    ahbLite3Slave.foreach(slavesConfigs(_).masters += AhbLite3CrossbarSlaveConnection(ahb))
    this
  }

  def addConnection(order: (AhbLite3, Seq[AhbLite3])): this.type = addConnection(order._1, order._2)

  def addConnections(orders: (AhbLite3, Seq[AhbLite3])*): this.type = {
    orders.foreach(addConnection(_))
    this
  }


  /** Build the crossbar */
  def build() = new Area {

    val masters = slavesConfigs.values.map(_.masters.map(_.master)).flatten.toSet

    val masterToDecodedSlave = mutable.HashMap[AhbLite3, Map[AhbLite3, AhbLite3]]()

    // Create a decoder for each master
    val decoders = for(master <- masters) yield new Area {

      val slaves = slavesConfigs.filter {
        case (_, config) => config.masters.exists(connection => connection.master == master)
      }.toSeq

      val decoder = AhbLite3Decoder(ahbLite3Config = ahbLite3Config, decodings = slaves.map(_._2.mapping))

      masterToDecodedSlave(master) = (slaves.map(_._1), decoder.io.outputs).zipped.toMap
      decoder.io.input <> master

      decoder.setPartialName(master, "decoder")
    }

    // Create an arbiters for each slave
    val arbiters = for((slave, config) <- slavesConfigs) yield new Area {

      val arbiter = AhbLite3Arbiter(ahbLite3Config = ahbLite3Config, inputsCount = config.masters.length)

      for((input, master) <- (arbiter.io.inputs, config.masters).zipped){
        input <> masterToDecodedSlave(master.master)(slave)
      }

      arbiter.io.output <> slave
      arbiter.setPartialName(slave, "arbiter")
    }
  }
}
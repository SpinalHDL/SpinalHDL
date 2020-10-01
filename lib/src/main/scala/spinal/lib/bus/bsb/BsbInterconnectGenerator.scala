package spinal.lib.bus.bsb

import spinal.core._
import spinal.lib._
import spinal.lib.generator._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


//case class BspParameterOption(byteCount : Option[Int],
//                         sourceWidth : Option[Int],
//                         sinkWidth : Option[Int],
//                         withMask : Option[Boolean])

case class BsbInterconnectGenerator() extends Generator{
  val masters = mutable.LinkedHashMap[Handle[Bsb], MasterModel]()
  val slaves = mutable.LinkedHashMap[Handle[Bsb], SlaveModel]()

  def getMaster(key : Handle[Bsb]) = masters.getOrElseUpdate(key, MasterModel(key).setCompositeName(key, "masterModel"))
  def getSlave(key : Handle[Bsb]) = slaves.getOrElseUpdate(key, SlaveModel(key).setCompositeName(key, "slaveModel"))

  case class MasterModel(bsb : Handle[Bsb]) extends Generator {
    val byteCount = Handle[Int]
    val sourceWidth = Handle[Int]
    val sinkWidth = Handle[Int]
    val withMask = Handle[Boolean]

    val connections = ArrayBuffer[ConnectionModel]()

    dependencies += BsbInterconnectGenerator.this

    val sinkWidthGen = add task new Generator{
      dependencies ++= connections.map(_.s.sinkWidth)
      add task{
        sinkWidth.load(connections.map(_.s.sinkWidth.get).max + log2Up(connections.size))
      }
    }
  }

  case class SlaveModel(bsb : Handle[Bsb]) extends Generator {
    val byteCount = Handle[Int]
    val sourceWidth = Handle[Int]
    val sinkWidth = Handle[Int]
    val withMask = Handle[Boolean]

    val connections = ArrayBuffer[ConnectionModel]()

    dependencies += BsbInterconnectGenerator.this

    val byteCountGen = add task new Generator{
      dependencies ++= connections.map(_.m.byteCount)
      add task{
        byteCount.load(connections.map(_.m.byteCount.get).max)
      }
    }

    val sourceWidthGen = add task new Generator{
      dependencies ++= connections.map(_.m.sourceWidth)
      add task{
        sourceWidth.load(connections.map(_.m.sourceWidth.get).max + log2Up(connections.size))
      }
    }

    val withMaskGen = add task new Generator{
      dependencies ++= connections.map(_.m.withMask)
      add task{
        withMask.load(connections.map(_.m.withMask.get).reduce(_ || _))
      }
    }
  }

  case class ConnectionModel(m : MasterModel, s : SlaveModel) extends Generator {
    m.connections += this
    s.connections += this

    val rtl = new Generator{
      dependencies += m.bsb
      dependencies += s.bsb

      val logic = add task new Area{
        if(m.generatorClockDomain.get != s.generatorClockDomain.get) { //TODO better sync check
          m.bsb.queue(128, m.generatorClockDomain, s.generatorClockDomain) >> s.bsb
        } else {
          m.bsb >> s.bsb
        }
      }
    }
  }

  def addMaster(bsb : Handle[Bsb]/*,
                byteCount : Handle[Int],
                sourceWidth : Handle[Int],
                sinkWidth : Handle[Int],
                withMask : Handle[Boolean]*/): MasterModel ={
    getMaster(bsb)
  }

  def addSlave(bsb : Handle[Bsb]/*,
               byteCount : Handle[Int],
               sourceWidth : Handle[Int],
               sinkWidth : Handle[Int],
               withMask : Handle[Boolean]*/): SlaveModel ={

    getSlave(bsb)
  }

  def connect(m : Handle[Bsb], s : Handle[Bsb], id : Handle[Int] = Handle(0)): Unit ={
    val c = new ConnectionModel(getMaster(m), getSlave(s))

  }
}

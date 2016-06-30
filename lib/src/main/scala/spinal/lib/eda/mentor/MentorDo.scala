package spinal.lib.eda.mentor

import spinal.core._

import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC32F_USER on 29/06/2016.
 */
object MentorDo {
  def apply() = new MentorDo
}

class MentorDo {
  val components = ArrayBuffer[Component]()
  def add(c : Component) : this.type = {
    components +=  c
    this
  }

  def build(prefix : String,file : String) : this.type = {
    var outFile: java.io.FileWriter = null
    outFile = new java.io.FileWriter(file)
    val builder = new StringBuilder()

    def getLabel(that : Nameable with Ownable) : String = that.owner match{
      case owner : Nameable => {
        val name = that.getName()
        val ownerName = owner.getName()
        val label = if(name.startsWith(ownerName + "_")) name.substring(ownerName.length + 1) else name
        label
      }
      case _ =>  that.getName()
    }
    for(component <- components){
      val componentPath = component.getPath()
      for(node <- component.nodes) node match{
        case bt : BaseType => {
          val groups = bt.getOwners.map(e => " -group " + getLabel(e.asInstanceOf[Nameable with Ownable]))
          if(!groups.isEmpty) {
            val label = getLabel(bt)
            builder ++= s"add wave -noupdate ${groups.reduce(_ + " " + _)} -label $label $prefix$componentPath/${bt.getName()}\n"
          }
        }
        case _ =>
      }
    }
    outFile.write(builder.toString())
    outFile.flush();
    outFile.close();
    this
  }
}

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
    for(component <- components){
      val componentPath = component.getPath()
      for(node <- component.nodes) node match{
        case bt : BaseType => {
          val groups = bt.childParents.map(" -group " + _.asInstanceOf[Nameable].getName())
          if(!groups.isEmpty)
            builder ++= s"add wave -noupdate ${groups.reduce(_ + " " + _)} $prefix$componentPath/${bt.getName()}\n"
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

package spinal.lib.eda.mentor

import spinal.core._

import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC32F_USER on 29/06/2016.
 */
object MentorDo {
  def apply() = new MentorDo
}
trait MentorDoTask
case class MentorDoComponentTask(c : Component,in : Seq[String],depth : Int) extends MentorDoTask
class MentorDo {
  val tasks = ArrayBuffer[MentorDoTask]()

  def add(c: Component, in: Seq[String] = null, depth: Int = 1): this.type = {
    tasks += MentorDoComponentTask(c, in, depth)
    this
  }

  def build(prefix: String, file: String): this.type = {
    var outFile: java.io.FileWriter = null
    outFile = new java.io.FileWriter(file)
    val builder = new StringBuilder()

    def getLabel(that: Nameable with OwnableRef): String = that.refOwner match {
      case owner: Nameable => {
        val name = that.getName()
        val ownerName = owner.getName()
        val label = if (name.startsWith(ownerName + "_")) name.substring(ownerName.length + 1) else name
        label
      }
      case _ => that.getName()
    }
    for (task <- tasks) task match {
      case task: MentorDoComponentTask => {
        def getComponents(that: Component, level: Int): Seq[Component] = {
          if (level != task.depth) {
            that.children ++ List(that)
          } else {
            List(that)
          }
        }
        val components = getComponents(task.c, 0)
        var nodes = ArrayBuffer[Nameable]()
        components.foreach(c => c.dslBody.walkStatements{
          case s : Nameable => nodes += s
          case s =>
        })
        nodes = nodes.sortBy(_.getInstanceCounter)
        for (node <- nodes) node match {
          case node: BaseType => {
            val componentPath = node.getComponents().tail.mkString("/")
            val preGroups = if (task.in == null) Nil else task.in
            val nodeGroups = node.getRefOwnersChain.dropWhile(_ != task.c).map(e => getLabel(e.asInstanceOf[Nameable with OwnableRef]))
            if (!nodeGroups.isEmpty) {
              val groupsMap = (preGroups ++ nodeGroups.tail).map(e => " -group " + e)
              val label = getLabel(node)
              builder ++= s"add wave -noupdate ${groupsMap.foldLeft("")(_ + " " + _)} -label $label $prefix$componentPath${if(componentPath != "")"/" else ""}${node.getName()}\n"
            }
          }
          case _ =>
        }
      }
    }

    outFile.write(builder.toString())
    outFile.flush();
    outFile.close();
    this
  }
}

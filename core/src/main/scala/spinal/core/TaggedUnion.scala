/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.core

import scala.collection.mutable.ArrayBuffer
import spinal.core.internals._
import spinal.idslplugin.{Location, ValCallback}

import scala.collection.mutable
import spinal.idslplugin.PostInitCallback



/**
  * TaggedUnion is a data structure that allows for the creation of type-safe unions
  * in hardware description using SpinalHDL.
  *
  * @param encoding the encoding used for the internal enumeration representing the union tags.
  */
class TaggedUnion(var encoding: SpinalEnumEncoding = native) extends MultiData with Nameable with ValCallbackRec with PostInitCallback {

    // A cache of union member descriptors, storing a tuple of their name and the corresponding Data object.
    var unionDescriptors = ArrayBuffer[(String, Data)]()

    // The enumeration representing possible tags for the union members.
    var tagEnum: SpinalEnum = new SpinalEnum(encoding)

    // A mapping from member names to their respective enumeration elements.
    var tagUnionDescriptors: mutable.Map[String, SpinalEnumElement[SpinalEnum]] = mutable.Map[String, SpinalEnumElement[SpinalEnum]]()

    // The current tag value, which indicates the active member of the union.
    var tag: SpinalEnumCraft[SpinalEnum] = null

    // The payload of the union, representing the current value of the active member.
    var unionPayload: Bits = null

    /**
        * Assigns a "don't care" value to the union payload and tag.
        *
        * @return the instance of the current TaggedUnion.
        */
    override def assignDontCare(): this.type = {
        this.unionPayload.assignDontCare()
        this.tag.assignDontCare()
        this
    }

    /**
        * Clones the current instance of TaggedUnion.
        *
        * @return a new instance of TaggedUnion with the same hardtype.
        */
    override def clone: TaggedUnion = {
        super.clone.asInstanceOf[TaggedUnion]
    }

    /** Assign the bundle with an other bundle by name */
    def assignAllByName(that: TaggedUnion): Unit = {
        for ((name, element) <- elements) {
            val other = that.find(name)
            if (other == null)
                LocatedPendingError(s"TaggedUnion assignment is not complete. Missing $name")
            else element match {
                case b: TaggedUnion => b.assignAllByName(other.asInstanceOf[TaggedUnion])
                case _         => element := other
            }
        }
    }

    /** Assign all possible signal fo the bundle with an other bundle by name */
    def assignSomeByName(that: TaggedUnion): Unit = {
        for ((name, element) <- elements) {
            val other = that.find(name)
            if (other != null) {
                element match {
                    case b: TaggedUnion => b.assignSomeByName(other.asInstanceOf[TaggedUnion])
                    case _         => element := other
                }
            }
        }
    }

    def bundleAssign(that : TaggedUnion)(f : (Data, Data) => Unit): Unit ={
        for ((name, element) <- elements) {
            val other = that.find(name)
            if (other == null) {
                LocatedPendingError(s"TaggedUnion assignment is not complete. $this need '$name' but $that doesn't provide it.")
            }
            else {
                f(element, other)
            }
        }
    }

    protected override def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef)(implicit loc: Location): Unit = {
        that match {
            case that: TaggedUnion =>
                if (!this.getClass.isAssignableFrom(that.getClass)) SpinalError("TaggedUnions must have the same final class to" +
                    " be assigned. Either use assignByName or assignSomeByName at \n" + ScalaLocated.long)
                    bundleAssign(that)((to, from) => to.compositAssignFrom(from,to,kind))
            case _ => throw new Exception("Undefined assignment")
        }
    }

    // collect the descriptors of the Tagged Union
    override def valCallbackRec(ref: Any, name: String): Unit = ref match {
        case ref : Data => {
            unionDescriptors += name -> ref
        }
        case ref =>
    }


    // Builds the TaggedUnion structure, initializing the union payload and tag based on the descriptors.
    def build(): Unit = {
        assert(unionDescriptors.nonEmpty, "TaggedUnion must have at least one element")
        initializeUnionPayload()
        initializeTag()
    }

    // Initializes the union payload based on the descriptors.
    private def initializeUnionPayload(): Unit = {
        val unionHT = HardType.union(unionDescriptors.map(_._2))
        unionPayload = unionHT()
        unionPayload.setPartialName("unionPayload")
        unionPayload.setRefOwner(this)
    }

    // Initializes the tag based on the descriptors.
    private def initializeTag(): Unit = {
        unionDescriptors.foreach { 
            case (name, _) =>
                val element = tagEnum.newElement(name) // Create one tag variant per descriptor.
                tagUnionDescriptors += name -> element // Store the mapping of name to descriptor.
        }
        tag = tagEnum()
        tag.setPartialName("tag")
        tag.setRefOwner(this)
    }

    // Callback function invoked after initialization to build the TaggedUnion.
    override def postInitCallback() = {
        build()
        this
    }

      // Provides the elements of the TaggedUnion, which are the payload and the tag.
    override def elements: ArrayBuffer[(String, Data)] = {
        ArrayBuffer(("unionPayload" -> unionPayload), ("tag" -> tag))
    }

    private[core] def rejectOlder = true

    def getTypeString = getClass.getSimpleName

    override def toString(): String = s"${component.getPath() + "/" + this.getDisplayName()} : $getTypeString"

    /**
    * Selects a member of the union and applies a callback on it.
    *
    * @param data the data to be selected.
    * @param callback the function to be applied on the selected data.
    * @tparam T the type of the data to be selected.
    */
    def choose[T <: Data](data: T)(callback: T => Unit): Unit = {
        val chosenElement = this.unionDescriptors.find(_._2 == data)
        chosenElement match {
            case Some((name, _)) => {
                val variant = tagUnionDescriptors(name)
                this.tag := variant
                callback(this.unionPayload.aliasAs(data))
            }
            case None => SpinalError(s"$data is not a member of this TaggedUnion")
        }
    }

//    def choose[T <: Data](data: T)(callback: T => Unit): Unit = {
//        val chosenElement = this.unionDescriptors.find(_._2 == data)
//        chosenElement match {
//            case Some((name, _)) => {
//                val variant = tagUnionDescriptors(name)
//                this.tag := variant
//                callback(this.unionPayload.aliasAs(data))
//            }
//            case None => SpinalError(s"$data is not a member of this TaggedUnion")
//        }
//    }
//
//    pf: PartialFunction[A, B]
    /**
    * Iterates over all members of the union and applies a callback function to the active member.
    *
    * @param callback the callback function to apply, taking a pair of the variant and the corresponding hardware element.
    */
    def among(callback: (Data, Data) => Unit): Unit = {
        switch(this.tag) {
            for((name, enumVariant) <- this.tagUnionDescriptors) {
                is(enumVariant) {
                    val dataVariant = this.unionDescriptors.find(_._1 == name)
                    dataVariant match {
                        case Some((_, d)) => {
                            val dataHardware = this.unionPayload.aliasAs(d)
                            callback(d, dataHardware)
                        }
                        case None => SpinalError(s"$name is not a member of this TaggedUnion")
                    }
                    
                }
            }
        }
    }
}


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
import scala.reflect.ClassTag


/**
  * TaggedUnion is a data structure that allows for the creation of type-safe unions
  * in hardware description using SpinalHDL.
  *
  * @param encoding the encoding used for the internal enumeration representing the union tags.
  */
class TaggedUnion(var encoding: SpinalEnumEncoding = native) extends MultiData with Nameable with ValCallbackRec with PostInitCallback {

    // A cache of union member descriptors, storing a tuple of their name and the corresponding Data object.
    val unionDescriptors = ArrayBuffer[(String, Data)]()

    // The enumeration representing possible tags for the union members.
    val tagEnum: SpinalEnum = new SpinalEnum(encoding)

    // A mapping from member names to their respective enumeration elements.
    val tagUnionDescriptors: mutable.Map[String, SpinalEnumElement[SpinalEnum]] = mutable.Map[String, SpinalEnumElement[SpinalEnum]]()

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
    * Get a hardware Bool signal if this taggedUnion selected variant is equal to specified data.
    */
    def is[T <: Data](data: T): Bool = {
        val isData = Bool()

        val inputDescriptor = this.unionDescriptors.find(_._2 == data) 
        inputDescriptor match {
            case Some((name, _)) => { // find name of variant from data: T
                val variant = tagUnionDescriptors(name) // find the enum value corresponding to name
                isData := this.tag === variant  // test if enum value is equal to tag
            }
            case None => SpinalError(s"$data is not a member of this TaggedUnion")
        }

        return isData
    }

    /**
    * Select the union variant from a given value and update the union value
    */
    def update[T <: Data](data: T)(callback: T => Unit): Unit = {
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

    /**
    * Select the union variant from its type and update the union value
    */
    def update[T <: Data : ClassTag](callback: T => Unit): Unit = {
        var matchingVariants: ArrayBuffer[(String, T)] = ArrayBuffer[(String, T)]()
        val tClass = implicitly[ClassTag[T]].runtimeClass

        for ((name: String, k) <- unionDescriptors) {
            // Check if 'k' is an instance of the runtime class associated with T
            if (tClass.isInstance(k)) {                
                matchingVariants += name -> k.asInstanceOf[T]
            }
        }

        // error if not only one match
        if(matchingVariants.size > 1) {
            SpinalError(s"Several variants (${matchingVariants}) are matching your callback for $this: your choice is ambiguous.")
        }

        if(matchingVariants.isEmpty) {
            SpinalError(s"No variant is matching your callback for $this: you need to modify it to deal with one valid case.")
        }


        val (variantName, variantData) = matchingVariants(0)
        val v = tagUnionDescriptors(variantName)
        this.tag := v
        callback(this.unionPayload.aliasAs(variantData))
    }


    /**
    * Read the union value and act according to its value and type.
    */
    def apply(callback: PartialFunction[Any, Unit]): Unit = {
        switch(this.tag) {
            for ((name, enumVariant) <- this.tagUnionDescriptors) {
                spinal.core.is(enumVariant) {
                    val dataVariant = this.unionDescriptors.find(_._1 == name)
                    dataVariant match {
                        case Some((_, d)) => { // d is descriptor (Data instance)
                            val dataHardware = this.unionPayload.aliasAs(d)

 

                            // Test the most specific first
                            if (callback.isDefinedAt((d, dataHardware))) {
                                callback((d, dataHardware))
                            } else if(callback.isDefinedAt(d)) { // Then the less specific
                                callback(dataHardware)
                            } else {
                                SpinalError(s"Nothing defined for variant $name")
                            }
                        }
                        case None => SpinalError(s"$name is not a member of this TaggedUnion")
                    }
                }
            }
        }
    }

    // /**
    // * Set the init value of this union from the input variant type
    // */
    // def initVariant[T <: Data](variantData: T): this.type = {
    //     // Ensure that the variantData provided is actually one of the variants of this TaggedUnion
    //     val variantOption = unionDescriptors.find(_._2.getClass == variantData.getClass)
        
    //     variantOption match {
    //         case Some((name, _)) =>

    //             // Set the initial value of the union payload to the provided variant data
    //             this.unionPayload.aliasAs(variantData).initFrom(variantData)

    //             // Find the corresponding tag element for the provided variant data
    //             val tagElement = tagUnionDescriptors(name)
    //             this.tag.init(tagElement)

                
    //         case None =>
    //             // If the provided data isn't a variant of this TaggedUnion, raise an error
    //             SpinalError(s"Provided data is not a valid variant of this TaggedUnion")
            
    //     }
    //     this
    // }

    // /**
    // * Set the init value of this union from the selected descriptor
    // */
    // def initVariant[T <: Data](descriptor: T)(variantData: T): this.type = {
    //     // Ensure that the variantData provided is actually one of the variants of this TaggedUnion
    //     val variantOption = unionDescriptors.find(_._2.getClass == variantData.getClass)
    //     variantOption match {
    //         case Some((name, _)) =>
    //             // Set the initial value of the union payload to the provided variant data
    //             this.unionPayload.initFrom(variantData)

    //             // Find the corresponding tag element for the provided variant data
    //             val tagElement = tagUnionDescriptors(name)
    //             this.tag.init(tagElement)

                
    //         case None =>
    //             // If the provided data isn't a variant of this TaggedUnion, raise an error
    //             SpinalError(s"Provided data is not a valid variant of this TaggedUnion")
            
    //     }
    //     this
    // }
}


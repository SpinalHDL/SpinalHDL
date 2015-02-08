/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package spinal

/**
 * Created by PIC18F on 27.01.2015.
 */

//Give number of bit to encode a given number of states


object log2Up {
  def apply(value: BigInt): Int = {
    if (value < 0) SpinalError(s"No negative value ($value) on ${this.getClass.getSimpleName}")
    (value - 1).bitLength
  }
}

trait Interface {
  def asMaster
  def asSlave
}

object master {
  def apply[T <: Interface](i: T) = {
    i.asMaster;
    i
  }

  object Flow {
    def apply[T <: Data](gen: T): Flow[T] = master.apply(new Flow(gen))
  }

  object Handshake {
    def apply[T <: Data](gen: T): Handshake[T] = master.apply(new Handshake(gen))
  }

}

object slave {
  def apply[T <: Interface](i: T) = {
    i.asSlave;
    i
  }

  object Flow {
    def apply[T <: Data](gen: T): Flow[T] = slave.apply(new Flow(gen))
  }

  object Handshake {
    def apply[T <: Data](gen: T): Handshake[T] = slave.apply(new Handshake(gen))
  }

}

class Flow[T <: Data](gen: T) extends Bundle with Interface {
  val valid = Bool()
  val bits = gen.clone()

  override def clone: this.type = new Flow(gen).asInstanceOf[this.type]

  override def asMaster: Unit = asOutput
  override def asSlave: Unit = asInput
}

class Handshake[T <: Data](gen: T) extends Bundle with Interface {
  val valid = Bool()
  val ready = Bool()
  val bits = gen.clone()

  override def clone: this.type = new Handshake(gen).asInstanceOf[this.type]

  override def asMaster: Unit = {
    valid.asOutput
    ready.asInput
    bits.asOutput
  }

  override def asSlave: Unit = {
    valid.asInput
    ready.asOutput
    bits.asInput
  }



  def <<(last: Handshake[T]): Handshake[T] = {
    this.valid := last.valid
    last.ready := this.ready
    this.bits := last.bits
    this
  }

  def ~[T2 <: Data](nextBits: T2): Handshake[T2] = {
    val next = new Handshake(nextBits)
    next.valid := this.valid
    this.ready := next.ready
    next.bits := nextBits
    next
  }

  def &(linkEnable: Bool): Handshake[T] = {
    val next = new Handshake(gen)
    next.valid := this.valid && linkEnable
    this.ready := next.ready && linkEnable
    next.bits := this.bits
    return next
  }


  def throwIf(cond: Bool): Handshake[T] = {
    val next = this.clone
    //next := this
    next << this
    when(cond) {
      next.valid := Bool(false)
      this.ready := Bool(true)
    }
    next
  }
}


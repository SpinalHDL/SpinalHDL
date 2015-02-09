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
  val data = gen.clone()

  override def clone: this.type = new Flow(gen).asInstanceOf[this.type]

  override def asMaster: Unit = asOutput
  override def asSlave: Unit = asInput

  def <<(that: Flow[T]): Unit = this connectFrom that
  def <-<(that: Flow[T]): Unit = this m2sPipeFrom that

  def connectFrom(that: Flow[T]): Unit = {
    valid := that.valid
    data := that.data
  }

  def takeIf(cond: Bool): Flow[T] = {
    val next = new Flow(gen)
    next.valid := this.valid && cond
    next.data := this.data
    return next
  }

  def throwIt(cond: Bool): Flow[T] = {
    this takeIf (!cond)
  }

  def translateWith[T2 <: Data](that: T2): Flow[T2] = {
    val next = new Flow(that)
    next.valid := this.valid
    next.data := that
    next
  }

  def m2sPipeFrom(that : Flow[T]): Flow[T] = {
    val reg = RegNext(that)
    reg.valid.setRegInit(Bool(false))
    this connectFrom reg
    this
  }

}

class Handshake[T <: Data](gen: T) extends Bundle with Interface {
  val valid = Bool()
  val ready = Bool()
  val data = gen.clone()

  override def clone: this.type = new Handshake(gen).asInstanceOf[this.type]

  override def asMaster: Unit = {
    valid.asOutput
    ready.asInput
    data.asOutput
  }

  override def asSlave: Unit = {
    valid.asInput
    ready.asOutput
    data.asInput
  }

  def <<(that: Handshake[T]): Unit = connectFrom(that)
  def ~[T2 <: Data](that: T2): Handshake[T2] = translateWith(that)
  def &(cond: Bool): Handshake[T] = continueIf(cond)


  def connectFrom(that: Handshake[T]): Unit = {
    this.valid := that.valid
    that.ready := this.ready
    this.data := that.data
  }

  def m2sPipeFrom(that: Handshake[T]): Handshake[T] = {
    val rValid = RegInit(Bool(false))
    val rData = Reg(gen)

    that.ready := (!this.valid) || this.ready

    when(that.ready) {
      rValid := that.valid
      rData := that.data
    }

    this.valid := rValid
    this.data := rData
    this
  }

  def s2mPipeFrom(that: Handshake[T]): Handshake[T] = {
    val rValid = RegInit(Bool(false))
    val rBits = Reg(gen)

    this.valid := that.valid || rValid
    that.ready := !rValid
    this.data := Mux(rValid, rBits, that.data)

    when(this.ready) {
      rValid := Bool(false)
    }

    when(that.ready && (!this.ready)) {
      rValid := that.valid
      rBits := that.data
    }
    this
  }

  def translateWith[T2 <: Data](that: T2): Handshake[T2] = {
    val next = new Handshake(that)
    next.valid := this.valid
    this.ready := next.ready
    next.data := that
    next
  }

  def continueIf(cond: Bool): Handshake[T] = {
    val next = new Handshake(gen)
    next.valid := this.valid && cond
    this.ready := next.ready && cond
    next.data := this.data
    return next
  }

  def throwIf(cond: Bool): Handshake[T] = {
    val next = this.clone

    next connectFrom this
    when(cond) {
      next.valid := Bool(false)
      this.ready := Bool(true)
    }
    next
  }

  def haltIf(cond: Bool): Handshake[T] = continueIf(!cond)
  def takeIf(cond: Bool): Handshake[T] = throwIf(!cond)
}


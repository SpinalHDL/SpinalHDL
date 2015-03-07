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

package spinal.test.bug

/**
 * Created by PIC18F on 23.01.2015.
 */
class Bool {

}

object tt {
  abstract class Data {
    type Self <: Data
    def :=(that: Self)
  }
/*
  class Bits extends Data {
    override type Self = Bits
    override def :=(that: Self): Unit = println(s"$this := $that")
  }

  class UInt extends Data {
    override type Self = UInt
    override def :=(that: Self): Unit = println(s"$this := $that")
  }*/

  class Flow[T <: Data](val gen: T){
    def <<(that: Flow[gen.Self]) = {
      this.gen := that.gen
    }
  }

  //Some stupide exemples of usage
 /* new Bits() := new Bits()
  val f1 = new Flow(new Bits)
  val f2 = new Flow(new Bits)
  f1 << f2*/
}

object out {

  object Bool {
    def apply(): Bool = new Bool()
    def apply(value: Boolean): Bool = this.apply()
  }
}


class bug {
  val a = out Bool()
  //a
}

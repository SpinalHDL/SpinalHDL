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

/*
class BoolFactory {
  def apply(): Bool = new Bool()
}

object Bool extends BoolFactory {

}
trait Direction {
  object Bool extends BoolFactory {
    override def apply(): Bool = super.apply()
  }
}


object out extends Direction{



}
object in extends Direction{



}
*/

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

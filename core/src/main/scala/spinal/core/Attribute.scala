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

package spinal.core

import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC18F on 09.02.2015.
 */
trait AttributeReady{
  private[core] val attributes = ArrayBuffer[Attribute]()
  def add(attribute: Attribute): Unit
}

trait Attribute {
  def getName : String
  def sameType(that: Attribute) : Boolean
  override def toString: String = getName
}


class AttributeString(name : String, val value : String) extends Attribute{
  override def getName: String = name
  override def sameType(that: Attribute) : Boolean = that.isInstanceOf[AttributeString]
}
class AttributeFlag(name : String) extends Attribute{
  override def getName: String = name
  override def sameType(that: Attribute) : Boolean = that.isInstanceOf[AttributeFlag]
}




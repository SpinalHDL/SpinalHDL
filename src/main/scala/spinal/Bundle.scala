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

import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC18F on 08.01.2015.
 */

object Bundle{

}

class Bundle extends MultiData with Nameable{
  override type SSelf = Bundle

  override def :=(that: SSelf): Unit = super.:=(that)
  override def <>(that: SSelf): Unit = super.<>(that)

  override def assignFrom(that: Data): Unit = {
    that match {
      case that : Bundle => {
        for((name,element) <- elements){
          val other = that.find(name)
          if(other != null)
            element := other
        }
      }
      case _ =>throw new Exception("Undefined assignement")
    }
  }

  private var elementsCache: ArrayBuffer[(String, Data)] = null


  def elements = {
    if (elementsCache == null) {
      elementsCache = ArrayBuffer[(String, Data)]()
      Misc.reflect(this, (name, obj) => {
        obj match {
          case data: Data => {
            elementsCache += Tuple2(name, data)
          }
          case _ =>
        }
      })
    }
    elementsCache.sortWith(_._2.instanceCounter < _._2.instanceCounter)
  }
}

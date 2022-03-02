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


/**
  * Represent the number of bit of a data
  */
case class BitCount(value: Int) {
  def +(right: BitCount) = BitCount(this.value + right.value)
  def -(right: BitCount) = BitCount(this.value - right.value)
  def *(right: BitCount) = BitCount(this.value * right.value)
  def /(right: BitCount) = BitCount(this.value / right.value)
  def %(right: BitCount) = BitCount(this.value % right.value)
}


/**
  * Slice size representation
  */
case class SlicesCount(value: Int)


/**
  * Exponent representation
  */
case class ExpNumber(value: Int)


/**
  * Position representation
  */
case class PosCount(value: Int)


/**
  * Cycles number representation
  */
case class CyclesCount(value: BigInt)

object CyclesCount{
  implicit def impConv(value: CyclesCount): BigInt = value.value
}


/**
  * Base class for the Physical representation (Hertz, Time, ...)
  */
abstract class PhysicalNumber[T <: PhysicalNumber[_]](protected val value: BigDecimal) {
  def newInstance(value: BigDecimal): T

  def +(that: T) = newInstance(this.value + that.value)
  def -(that: T) = newInstance(this.value - that.value)

  def *(that: PhysicalNumber[_]) = this.value * that.value
  def /(that: PhysicalNumber[_]) = this.value / that.value
  def %(that: PhysicalNumber[_]) = this.value % that.value

  def +(that: BigDecimal) = newInstance(this.value + that)
  def -(that: BigDecimal) = newInstance(this.value - that)
  def *(that: BigDecimal) = newInstance(this.value * that)
  def /(that: BigDecimal) = newInstance(this.value / that)
  def %(that: BigDecimal) = newInstance(this.value % that)

  def max(that: T) = newInstance(value.max(that.value))

  def toInt        = value.toInt
  def toLong       = value.toLong
  def toDouble     = value.toDouble
  def toBigDecimal = value
}


/**
  * Time representation
  */
case class TimeNumber(private val v: BigDecimal) extends PhysicalNumber[TimeNumber](v){

  override def newInstance(value: BigDecimal): TimeNumber = TimeNumber(value)

  def +(that: HertzNumber) = this.value + that.toBigDecimal
  def -(that: HertzNumber) = this.value - that.toBigDecimal
  def *(that: HertzNumber) = this.value * that.toBigDecimal
  def /(that: HertzNumber) = this.value / that.toBigDecimal
  def %(that: HertzNumber) = this.value % that.toBigDecimal

  def toHertz = HertzNumber(1 / this.value)

  def decompose: (BigDecimal, String) = this.value match {
    case d if value > 3600.0  => (d / 3600.0,  "hr")
    case d if value > 60.0    => (d / 60.0,    "min")
    case d if value > 1.0     => (d / 1.0,     "sec")
    case d if value > 1.0e-3  => (d / 1.0e-3,  "ms")
    case d if value > 1.0e-6  => (d / 1.0e-6,  "us")
    case d if value > 1.0e-9  => (d / 1.0e-9,  "ns")
    case d if value > 1.0e-12 => (d / 1.0e-12, "ps")
    case d if value > 1.0e-15 => (d / 1.0e-15, "fs")
    case d: BigDecimal        => (d,           "unknown")
    case _                    => (0.0,         "error")
  }
}


/**
  * Frequency representation
  */
case class HertzNumber(private val v: BigDecimal) extends PhysicalNumber[HertzNumber](v){

  override def newInstance(value: BigDecimal): HertzNumber = HertzNumber(value)

  def +(that: TimeNumber) = this.value + that.toBigDecimal
  def -(that: TimeNumber) = this.value - that.toBigDecimal
  def *(that: TimeNumber) = this.value * that.toBigDecimal
  def /(that: TimeNumber) = this.value / that.toBigDecimal
  def %(that: TimeNumber) = this.value % that.toBigDecimal

  def toTime = TimeNumber(1 / this.value)

  def decompose: (BigDecimal, String) = this.value match {
    case d if d > 1.0e18 => (d / 1.0e18, "EHz")
    case d if d > 1.0e15 => (d / 1.0e15, "PHz")
    case d if d > 1.0e12 => (d / 1.0e12, "THz")
    case d if d > 1.0e9  => (d / 1.0e9,  "GHz")
    case d if d > 1.0e6  => (d / 1.0e6,  "MHz")
    case d if d > 1.0e3  => (d / 1.0e3,  "kHz")
    case d if d > 1.0    => (d / 1.0,    "Hz")
    case d: BigDecimal   => (d,          "unknown")
    case _               => (0.0,        "error")
  }
}

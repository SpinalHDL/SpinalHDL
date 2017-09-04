///*
// * SpinalHDL
// * Copyright (c) Dolu, All rights reserved.
// *
// * This library is free software; you can redistribute it and/or
// * modify it under the terms of the GNU Lesser General Public
// * License as published by the Free Software Foundation; either
// * version 3.0 of the License, or (at your option) any later version.
// *
// * This library is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// * Lesser General Public License for more details.
// *
// * You should have received a copy of the GNU Lesser General Public
// * License along with this library.
// */
//
//package spinal.tester.scalatest
//
//import spinal.core._
//import spinal.lib._
//import spinal.lib.com.i2c._
//
//
//class I2CIoMasterTester extends SpinalTesterCocotbBase {
//  override def getName: String = "I2cIoMasterTester"
//  override def pythonTestLocation: String = "tester/src/test/python/spinal/I2CTester2/IoMasterTester"
//  override def createToplevel: Component = new I2cIoMaster(I2cIoMasterGenerics(
//    samplingSize              = 3,
//    samplingClockDividerWidth = 10 bits,
//    timerClockDividerWidth    = 20 bits,
//    timeoutBaudRatioLog2      = 5
//  ))
//}
//

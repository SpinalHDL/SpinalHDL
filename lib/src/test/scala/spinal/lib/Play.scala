package spinal.lib

import spinal.core.assert

object playbinary extends App{
  assert("16".hexToBinInts == List(0,1,1,0,1))
  println("16".hexToBinInts)
  println(32.toBinInts)
  println(77.binString)
  println(77.binString(16))
  println(77.toBinInts(num = 16))
  println("123".getBytes.map(_.toBinaryString).toList)
  println("qwe".getBytes().flatMap(_.toBinInts).toList)
  println("qwe".getBytes().toList)
  println("中文字体".getBytes().toList)
  println("中文字体".getBytes().toList.flatMap(_.binString))
  println("123".hexToBinInts)
}


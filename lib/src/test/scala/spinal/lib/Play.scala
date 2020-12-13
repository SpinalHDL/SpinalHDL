package spinal.lib

import spinal.core.assert

object playbinary extends App{
  assert("16".hexToBinst == List(0,1,1,0,1))
  println("16".hexToBinst)
  println(32.toBinst)
  println(77.binString)
  println(77.binString(16))
  println(77.toBinst(num = 16))
  println("123".getBytes.map(_.toBinaryString).toList)
  println("qwe".getBytes().flatMap(_.toBinst).toList)
  println("qwe".getBytes().toList)
  println("中文字体".getBytes().toList)
  println("中文字体".getBytes().toList.flatMap(_.binString))
  println("123".hexToBinst)
}


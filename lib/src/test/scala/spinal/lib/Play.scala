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

object playbinary2 extends App{
  assert("16".hexToBinInts == List(0,1,1,0,1))
  println(x"32456".toBinInts())
  println(x"32456" , x"32456".toBinInts().binIntsToHex)
  println(x"32456" , x"32456".toBinInts().binIntsToHexAlignHigh)
  assert("32456" == o"32456".toBinInts().binIntsToOct)
  assert("32456" == x"32456".toBinInts().binIntsToHex)
  assert(324565 == 324565.toBinInts().binIntsToInt)
  assert(32 == 32.toBinInts().binIntsToInt)
  assert(134 == 134.toBinInts().binIntsToInt)
  assert(BigInt("abcdef0123456789abcdef", 16) == BigInt("abcdef0123456789abcdef", 16).toBinInts().binIntsToBigInt)
}

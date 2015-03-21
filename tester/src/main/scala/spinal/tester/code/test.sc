import spinal.core._
import spinal.test._
//import spinal.core.test.s2
//val x: Int = s2"{ a: ${2} }"


//val asd = u"aaa${1}bbb${2}"
//asd
u"asd"

//class IntPimped(i : Int){
//  def UInt = spinal.core.u(i)
//  def UInt(bitCount: BitCount) = u(i,bitCount)
//}
//implicit def intPimped(i : Int) = new IntPimped(i)
//val myUInt = 2 UInt(3 bit)

class MyClass(i: Int) {
  def +(j: Int) = new MyClass(j + i)
  def -(j: Int) = new MyClass(i - j)
  def ^(j: Int) = new MyClass(j)
  def +|(j: Int) = new MyClass(j + i / 3)
  def \(a : Int) = new MyClass(a)
}


var c = new MyClass(1)
c \= 6

class Bar {
  def foo = 4
  def foo_=(x: Int) = {
    println(x)
  }
}

val b = new Bar
b.foo = 2
class A {
  def aa: A = {
    println("A")
    this
  }
  def aa_=(i: Int): A = {
    println("B " + i)
    new A
  }

  //  def apply_= (i : Int) : A = {
  //    println("B " + i)
  //    this
  //  }

  def update(value: Int): Unit = {
    println(value)
  }
}

val a = new A
a.aa = 2
scala.collection.mutable.HashMap


def <� : Int = 3

//10 x UInt(10 bit)

class C1{
  def f2(a : Int,b : Int): Unit ={
  }
  def f1(a : Int): Unit ={
  }
}

val c1 = new C1
c1.f2(1,2)

c1.f1(2)
c1 f1 2
c1 f2 (3,4)

object when {
  def apply(cond: Int)(block: => Unit): When = {
    val w = new When(cond)
    return w
  }
}
class When(cond : Int){
  print(cond + " ")
  def elsewhen(cond: Int)(block: => Unit): When = {
    val w = new When(cond)
    return w
  }

  def x : Y = {
    new Y()
  }
}
class Y{
  def  apply(cond: Int)(block: => Unit): When = {
    val w = new When(cond)
    return w
  }
  def y(cond: Int)(block: => Unit): When = {
    val w = new When(cond)
    return w
  }

}

when(1){
}elsewhen(2){

}



//when(3){
//} x y(4){
//
//}

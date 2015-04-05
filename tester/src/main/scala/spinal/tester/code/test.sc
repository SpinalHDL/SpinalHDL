
class BBB {

  def apply(blocks: (Int,() => Unit)*): Unit = {
    blocks.foreach(_._2)
  }

  def __(b : BBB) : BBB = this
}

val b = new BBB
b __ b __ b

//BBB(1 -> () => {
//  println("BBB block hallo1")
//},() => 2 -> {
//  println("BBB block hallo2")
//})

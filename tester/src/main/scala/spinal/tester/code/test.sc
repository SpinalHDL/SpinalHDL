
object BBB {

  def apply(blocks: (Int,() => Unit)*): Unit = {
    blocks.foreach(_._2)
  }
}

//BBB(1 -> () => {
//  println("BBB block hallo1")
//},() => 2 -> {
//  println("BBB block hallo2")
//})

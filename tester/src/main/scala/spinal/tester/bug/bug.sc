class Data{
  override def clone(): this.type = new Data().asInstanceOf[this.type]
}
class Stream[T <: Data](dataType: T){
  val data = dataType.clone()
}

object Test{
  def doit[T <: Data](that : Stream[T]): Stream[T] ={
    val ret = new Stream(that.data)
    return ret
  }
}

Test.doit(new Stream(new Data))

//
//object Data {
//  implicit def autoCast[T <: Data, T2 <: T](that: T): T2#SSelf = that.asInstanceOf[T2#SSelf]
//}
//trait Data {
//  type SSelf <: Data
//  def :=(that: SSelf): Unit = this.assignFrom(that)
//  def assignFrom(that: Data): Unit = ???
//  def autoConnect(that: Data): Unit = ???
//}
//class Bits extends Data {
//  override type SSelf = Bits
// // override def :=(that: SSelf): Unit = ???
//  override def autoConnect(that: Data): Unit = {
//    this := that
//  }
//  final override def assignFrom(that: Data): Unit = println(s"$this := $that")
//}
//class UInt extends Data {
//  override type SSelf = UInt
//  //override def :=(that: SSelf): Unit = ???
//}
//val b1 = new Bits
//val u1 = new UInt
//"***"
//b1 := u1
//"*****"
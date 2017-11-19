package spinal.lib.eda.bench

trait Report{
  def getFMax() : Double
  def getArea() : String

  override def toString = s"$getArea   $getFMax"
}

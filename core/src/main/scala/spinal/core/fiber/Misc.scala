package spinal.core.fiber

import spinal.core.assert

case class Lock() extends Handle[Int]{
  load(0)
  private var retains = 0
  def retain() : this.type = {
    retains += 1
    this.unload()
    this
  }
  def release() : Unit = {
    assert(retains > 0)
    retains -= 1
    if(retains == 0) {
      this.load(0)
    }
  }
}

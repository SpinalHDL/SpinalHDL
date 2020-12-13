package spinal.lib.memory.ram

import vendors._

trait Vendor {
  def build(mw: Ram1rw): MemBlackBox = new umc.mbb1rw(mw).Build()
  def build(mw: Ram1r1w): MemBlackBox = new umc.mbb1r1w(mw).Build()
  def build(mw: Ram2rw): MemBlackBox = new umc.mbb2rw(mw).Build()
  def build(mw: Rom): MemBlackBox = new umc.mbbrom(mw).Build()
}

case object UMC extends Vendor{
  override def build(mw: Ram1rw): MemBlackBox = new umc.mbb1rw(mw).Build()
  override def build(mw: Ram1r1w): MemBlackBox = new umc.mbb1r1w(mw).Build()
  override def build(mw: Ram2rw): MemBlackBox = new umc.mbb2rw(mw).Build()
  override def build(mw: Rom): MemBlackBox = new umc.mbbrom(mw).Build()
}

case object FishSemi extends Vendor{
  override def build(mw: Ram1rw): MemBlackBox = new umc.mbb1rw(mw).Build()
  override def build(mw: Ram1r1w): MemBlackBox = new umc.mbb1r1w(mw).Build()
  override def build(mw: Ram2rw): MemBlackBox = new umc.mbb2rw(mw).Build()
  override def build(mw: Rom): MemBlackBox = new umc.mbbrom(mw).Build()
}

case object SPRD extends Vendor{
}

case object DJI extends Vendor{
  override def build(mw: Ram1rw): MemBlackBox = new dji.mbb1rw(mw).Build()
  override def build(mw: Ram1r1w): MemBlackBox = new dji.mbb1r1w(mw).Build()
  override def build(mw: Ram2rw): MemBlackBox = new dji.mbb2rw(mw).Build()
  override def build(mw: Rom): MemBlackBox = new dji.mbbrom(mw).Build()
}

case object TSMC extends Vendor{
  override def build(mw: Ram1rw): MemBlackBox = new dji.mbb1rw(mw).Build()
  override def build(mw: Ram1r1w): MemBlackBox = new dji.mbb1r1w(mw).Build()
  override def build(mw: Ram2rw): MemBlackBox = new dji.mbb2rw(mw).Build()
  override def build(mw: Rom): MemBlackBox = new dji.mbbrom(mw).Build()
}

case object ZTE extends Vendor{
}

case object HuaWei extends Vendor{
}

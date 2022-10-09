package spinal.lib.bus.amba3.ahblite.sim

object AhbAttributes {
  def apply(attr: Hburst): AhbAttributes = new AhbAttributes()(attr)
  def apply(attr: Hmastlock): AhbAttributes = new AhbAttributes()(attr)
  def apply(attr: Hprot): AhbAttributes = new AhbAttributes()(attr)
  def apply(attr: Hsize): AhbAttributes = new AhbAttributes()(attr)
  def apply(attr: Htrans): AhbAttributes = new AhbAttributes()(attr)

  def withHburst(attr: Int): AhbAttributes = new AhbAttributes().withHburst(attr)
  def withHmastlock(attr: Boolean = true): AhbAttributes = new AhbAttributes().withHmastlock(attr)
  def withHprot(attr: Int): AhbAttributes = new AhbAttributes().withHprot(attr)
  def withHsize(attr: Int): AhbAttributes = new AhbAttributes().withHsize(attr)
  def withHtrans(attr: Int): AhbAttributes = new AhbAttributes().withHtrans(attr)
}

/** Set of AHB attributes.
  *
  * Example:
  *
  * {{{
  * AhbAttributes(Hmastlock.HIGH)(Hprot.opcodeFetch.withNonCacheable)
  * }}}
  */
case class AhbAttributes(
    hburst: Option[Int] = None,
    hmastlock: Option[Boolean] = None,
    hprot: Option[Int] = None,
    hsize: Option[Int] = None,
    htrans: Option[Int] = None
) {
  def apply(attr: Hburst): AhbAttributes = copy(hburst = Some(attr))
  def apply(attr: Hmastlock): AhbAttributes = copy(hmastlock = Some(attr))
  def apply(attr: Hprot): AhbAttributes = copy(hprot = Some(attr))
  def apply(attr: Hsize): AhbAttributes = copy(hsize = Some(attr))
  def apply(attr: Htrans): AhbAttributes = copy(htrans = Some(attr))

  def withHburst(attr: Int): AhbAttributes = copy(hburst = Some(attr))
  def withHmastlock(attr: Boolean = true): AhbAttributes = copy(hmastlock = Some(attr))
  def withHprot(attr: Int): AhbAttributes = copy(hprot = Some(attr))
  def withHsize(attr: Int): AhbAttributes = copy(hsize = Some(attr))
  def withHtrans(attr: Int): AhbAttributes = copy(htrans = Some(attr))
}

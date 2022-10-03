package spinal.lib.bus.amba3.ahblite.sim

object AhbAttributes {
  def apply(attr: Hburst): AhbAttributes = new AhbAttributes()(attr)
  def apply(attr: Hmastlock): AhbAttributes = new AhbAttributes()(attr)
  def apply(attr: Hprot): AhbAttributes = new AhbAttributes()(attr)
  def apply(attr: Hsize): AhbAttributes = new AhbAttributes()(attr)
  def apply(attr: Htrans): AhbAttributes = new AhbAttributes()(attr)
}

/** Set of AHB attributes.
  *
  * Example:
  *
  * {{{
  * AhbAttributes(Hmastlock.HIGH)(Hprot.opcodeFetch.withNonCacheable)
  * }}}
  */
class AhbAttributes(
    val hburst: Option[Int] = None,
    val hmastlock: Option[Boolean] = None,
    val hprot: Option[Int] = None,
    val hsize: Option[Int] = None,
    val htrans: Option[Int] = None
) {
  def copy(
      hburst: Option[Int] = hburst,
      hmastlock: Option[Boolean] = hmastlock,
      hprot: Option[Int] = hprot,
      hsize: Option[Int] = hsize,
      htrans: Option[Int] = htrans
  ): AhbAttributes = new AhbAttributes(
    hburst,
    hmastlock,
    hprot,
    hsize,
    htrans
  )

  def apply(attr: Hburst): AhbAttributes = copy(hburst = Some(attr))
  def apply(attr: Hmastlock): AhbAttributes = copy(hmastlock = Some(attr))
  def apply(attr: Hprot): AhbAttributes = copy(hprot = Some(attr))
  def apply(attr: Hsize): AhbAttributes = copy(hsize = Some(attr))
  def apply(attr: Htrans): AhbAttributes = copy(htrans = Some(attr))
}

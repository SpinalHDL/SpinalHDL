package spinal.lib.bus.regif 

sealed trait AccessType

object AccessType {
  object RO    extends AccessType  //- W: no effect, R: no effect
  object RW    extends AccessType  //- W: as-is, R: no effect
  object RC    extends AccessType  //- W: no effect, R: clears all bits
  object RS    extends AccessType  //- W: no effect, R: sets all bits
  object WRC   extends AccessType  //- W: as-is, R: clears all bits
  object WRS   extends AccessType  //- W: as-is, R: sets all bits
  object WC    extends AccessType  //- W: clears all bits, R: no effect
  object WS    extends AccessType  //- W: sets all bits, R: no effect
  object WSRC  extends AccessType  //- W: sets all bits, R: clears all bits
  object WCRS  extends AccessType  //- W: clears all bits, R: sets all bits
  object W1C   extends AccessType  //- W: 1/0 clears/no effect on matching bit, R: no effect
  object W1S   extends AccessType  //- W: 1/0 sets/no effect on matching bit, R: no effect
  object W1T   extends AccessType  //- W: 1/0 toggles/no effect on matching bit, R: no effect
  object W0C   extends AccessType  //- W: 1/0 no effect on/clears matching bit, R: no effect
  object W0S   extends AccessType  //- W: 1/0 no effect on/sets matching bit, R: no effect
  object W0T   extends AccessType  //- W: 1/0 no effect on/toggles matching bit, R: no effect
  object W1SRC extends AccessType  //- W: 1/0 sets/no effect on matching bit, R: clears all bits
  object W1CRS extends AccessType  //- W: 1/0 clears/no effect on matching bit, R: sets all bits
  object W0SRC extends AccessType  //- W: 1/0 no effect on/sets matching bit, R: clears all bits
  object W0CRS extends AccessType  //- W: 1/0 no effect on/clears matching bit, R: sets all bits
  object WO    extends AccessType  //- W: as-is, R: error
  object WOC   extends AccessType  //- W: clears all bits, R: error
  object WOS   extends AccessType  //- W: sets all bits, R: error
  object W1    extends AccessType  //- W: first one after ~HARD~ reset is as-is, other W have no effects, R: no effect
  object WO1   extends AccessType  //- W: first one after ~HARD~ reset is as-is, other W have no effects, R: error
  object NA    extends AccessType  // -W: reserved, R: reserved
}


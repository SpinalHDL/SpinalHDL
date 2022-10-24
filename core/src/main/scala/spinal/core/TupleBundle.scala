package spinal.core

class TupleBundleBase extends Bundle{
  override def valCallbackRec(ref: Any, name: String) = {
    super.valCallbackRec(ref, name.tail)
  }
  override def asBits: Bits = {
    var ret: Bits = null
    for ((_, e) <- elements.reverse) {
      if (ret == null.asInstanceOf[Object]) ret = e.asBits
      else ret = e.asBits ## ret
    }
    if (ret.asInstanceOf[Object] == null) ret = Bits(0 bits)
    ret
  }
  def asRevertedBits: Bits = {
    var ret: Bits = null
    for ((_, e) <- elements) {
      if (ret == null.asInstanceOf[Object]) ret = e.asBits
      else ret = e.asBits ## ret
    }
    if (ret.asInstanceOf[Object] == null) ret = Bits(0 bits)
    ret
  }
}

case class TupleBundle1[T1 <: Data](val payloadType1: HardType[T1])
    extends TupleBundleBase {
  val _1 = payloadType1()
}

case class TupleBundle2[T1 <: Data, T2 <: Data](val payloadType1: HardType[T1],
                                                val payloadType2: HardType[T2])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
}

case class TupleBundle3[T1 <: Data, T2 <: Data, T3 <: Data](
  val payloadType1: HardType[T1],
  val payloadType2: HardType[T2],
  val payloadType3: HardType[T3]
) extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
}

case class TupleBundle4[T1 <: Data, T2 <: Data, T3 <: Data, T4 <: Data](
  val payloadType1: HardType[T1],
  val payloadType2: HardType[T2],
  val payloadType3: HardType[T3],
  val payloadType4: HardType[T4]
) extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
}

case class TupleBundle5[T1 <: Data,
                        T2 <: Data,
                        T3 <: Data,
                        T4 <: Data,
                        T5 <: Data](val payloadType1: HardType[T1],
                                    val payloadType2: HardType[T2],
                                    val payloadType3: HardType[T3],
                                    val payloadType4: HardType[T4],
                                    val payloadType5: HardType[T5])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
}

case class TupleBundle6[T1 <: Data,
                        T2 <: Data,
                        T3 <: Data,
                        T4 <: Data,
                        T5 <: Data,
                        T6 <: Data](val payloadType1: HardType[T1],
                                    val payloadType2: HardType[T2],
                                    val payloadType3: HardType[T3],
                                    val payloadType4: HardType[T4],
                                    val payloadType5: HardType[T5],
                                    val payloadType6: HardType[T6])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
}

case class TupleBundle7[T1 <: Data,
                        T2 <: Data,
                        T3 <: Data,
                        T4 <: Data,
                        T5 <: Data,
                        T6 <: Data,
                        T7 <: Data](val payloadType1: HardType[T1],
                                    val payloadType2: HardType[T2],
                                    val payloadType3: HardType[T3],
                                    val payloadType4: HardType[T4],
                                    val payloadType5: HardType[T5],
                                    val payloadType6: HardType[T6],
                                    val payloadType7: HardType[T7])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
}

case class TupleBundle8[T1 <: Data,
                        T2 <: Data,
                        T3 <: Data,
                        T4 <: Data,
                        T5 <: Data,
                        T6 <: Data,
                        T7 <: Data,
                        T8 <: Data](val payloadType1: HardType[T1],
                                    val payloadType2: HardType[T2],
                                    val payloadType3: HardType[T3],
                                    val payloadType4: HardType[T4],
                                    val payloadType5: HardType[T5],
                                    val payloadType6: HardType[T6],
                                    val payloadType7: HardType[T7],
                                    val payloadType8: HardType[T8])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
  val _8 = payloadType8()
}

case class TupleBundle9[T1 <: Data,
                        T2 <: Data,
                        T3 <: Data,
                        T4 <: Data,
                        T5 <: Data,
                        T6 <: Data,
                        T7 <: Data,
                        T8 <: Data,
                        T9 <: Data](val payloadType1: HardType[T1],
                                    val payloadType2: HardType[T2],
                                    val payloadType3: HardType[T3],
                                    val payloadType4: HardType[T4],
                                    val payloadType5: HardType[T5],
                                    val payloadType6: HardType[T6],
                                    val payloadType7: HardType[T7],
                                    val payloadType8: HardType[T8],
                                    val payloadType9: HardType[T9])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
  val _8 = payloadType8()
  val _9 = payloadType9()
}

case class TupleBundle10[T1 <: Data,
                         T2 <: Data,
                         T3 <: Data,
                         T4 <: Data,
                         T5 <: Data,
                         T6 <: Data,
                         T7 <: Data,
                         T8 <: Data,
                         T9 <: Data,
                         T10 <: Data](val payloadType1: HardType[T1],
                                      val payloadType2: HardType[T2],
                                      val payloadType3: HardType[T3],
                                      val payloadType4: HardType[T4],
                                      val payloadType5: HardType[T5],
                                      val payloadType6: HardType[T6],
                                      val payloadType7: HardType[T7],
                                      val payloadType8: HardType[T8],
                                      val payloadType9: HardType[T9],
                                      val payloadType10: HardType[T10])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
  val _8 = payloadType8()
  val _9 = payloadType9()
  val _10 = payloadType10()
}

case class TupleBundle11[T1 <: Data,
                         T2 <: Data,
                         T3 <: Data,
                         T4 <: Data,
                         T5 <: Data,
                         T6 <: Data,
                         T7 <: Data,
                         T8 <: Data,
                         T9 <: Data,
                         T10 <: Data,
                         T11 <: Data](val payloadType1: HardType[T1],
                                      val payloadType2: HardType[T2],
                                      val payloadType3: HardType[T3],
                                      val payloadType4: HardType[T4],
                                      val payloadType5: HardType[T5],
                                      val payloadType6: HardType[T6],
                                      val payloadType7: HardType[T7],
                                      val payloadType8: HardType[T8],
                                      val payloadType9: HardType[T9],
                                      val payloadType10: HardType[T10],
                                      val payloadType11: HardType[T11])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
  val _8 = payloadType8()
  val _9 = payloadType9()
  val _10 = payloadType10()
  val _11 = payloadType11()
}

case class TupleBundle12[T1 <: Data,
                         T2 <: Data,
                         T3 <: Data,
                         T4 <: Data,
                         T5 <: Data,
                         T6 <: Data,
                         T7 <: Data,
                         T8 <: Data,
                         T9 <: Data,
                         T10 <: Data,
                         T11 <: Data,
                         T12 <: Data](val payloadType1: HardType[T1],
                                      val payloadType2: HardType[T2],
                                      val payloadType3: HardType[T3],
                                      val payloadType4: HardType[T4],
                                      val payloadType5: HardType[T5],
                                      val payloadType6: HardType[T6],
                                      val payloadType7: HardType[T7],
                                      val payloadType8: HardType[T8],
                                      val payloadType9: HardType[T9],
                                      val payloadType10: HardType[T10],
                                      val payloadType11: HardType[T11],
                                      val payloadType12: HardType[T12])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
  val _8 = payloadType8()
  val _9 = payloadType9()
  val _10 = payloadType10()
  val _11 = payloadType11()
  val _12 = payloadType12()
}

case class TupleBundle13[T1 <: Data,
                         T2 <: Data,
                         T3 <: Data,
                         T4 <: Data,
                         T5 <: Data,
                         T6 <: Data,
                         T7 <: Data,
                         T8 <: Data,
                         T9 <: Data,
                         T10 <: Data,
                         T11 <: Data,
                         T12 <: Data,
                         T13 <: Data](val payloadType1: HardType[T1],
                                      val payloadType2: HardType[T2],
                                      val payloadType3: HardType[T3],
                                      val payloadType4: HardType[T4],
                                      val payloadType5: HardType[T5],
                                      val payloadType6: HardType[T6],
                                      val payloadType7: HardType[T7],
                                      val payloadType8: HardType[T8],
                                      val payloadType9: HardType[T9],
                                      val payloadType10: HardType[T10],
                                      val payloadType11: HardType[T11],
                                      val payloadType12: HardType[T12],
                                      val payloadType13: HardType[T13])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
  val _8 = payloadType8()
  val _9 = payloadType9()
  val _10 = payloadType10()
  val _11 = payloadType11()
  val _12 = payloadType12()
  val _13 = payloadType13()
}

case class TupleBundle14[T1 <: Data,
                         T2 <: Data,
                         T3 <: Data,
                         T4 <: Data,
                         T5 <: Data,
                         T6 <: Data,
                         T7 <: Data,
                         T8 <: Data,
                         T9 <: Data,
                         T10 <: Data,
                         T11 <: Data,
                         T12 <: Data,
                         T13 <: Data,
                         T14 <: Data](val payloadType1: HardType[T1],
                                      val payloadType2: HardType[T2],
                                      val payloadType3: HardType[T3],
                                      val payloadType4: HardType[T4],
                                      val payloadType5: HardType[T5],
                                      val payloadType6: HardType[T6],
                                      val payloadType7: HardType[T7],
                                      val payloadType8: HardType[T8],
                                      val payloadType9: HardType[T9],
                                      val payloadType10: HardType[T10],
                                      val payloadType11: HardType[T11],
                                      val payloadType12: HardType[T12],
                                      val payloadType13: HardType[T13],
                                      val payloadType14: HardType[T14])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
  val _8 = payloadType8()
  val _9 = payloadType9()
  val _10 = payloadType10()
  val _11 = payloadType11()
  val _12 = payloadType12()
  val _13 = payloadType13()
  val _14 = payloadType14()
}

case class TupleBundle15[T1 <: Data,
                         T2 <: Data,
                         T3 <: Data,
                         T4 <: Data,
                         T5 <: Data,
                         T6 <: Data,
                         T7 <: Data,
                         T8 <: Data,
                         T9 <: Data,
                         T10 <: Data,
                         T11 <: Data,
                         T12 <: Data,
                         T13 <: Data,
                         T14 <: Data,
                         T15 <: Data](val payloadType1: HardType[T1],
                                      val payloadType2: HardType[T2],
                                      val payloadType3: HardType[T3],
                                      val payloadType4: HardType[T4],
                                      val payloadType5: HardType[T5],
                                      val payloadType6: HardType[T6],
                                      val payloadType7: HardType[T7],
                                      val payloadType8: HardType[T8],
                                      val payloadType9: HardType[T9],
                                      val payloadType10: HardType[T10],
                                      val payloadType11: HardType[T11],
                                      val payloadType12: HardType[T12],
                                      val payloadType13: HardType[T13],
                                      val payloadType14: HardType[T14],
                                      val payloadType15: HardType[T15])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
  val _8 = payloadType8()
  val _9 = payloadType9()
  val _10 = payloadType10()
  val _11 = payloadType11()
  val _12 = payloadType12()
  val _13 = payloadType13()
  val _14 = payloadType14()
  val _15 = payloadType15()
}

case class TupleBundle16[T1 <: Data,
                         T2 <: Data,
                         T3 <: Data,
                         T4 <: Data,
                         T5 <: Data,
                         T6 <: Data,
                         T7 <: Data,
                         T8 <: Data,
                         T9 <: Data,
                         T10 <: Data,
                         T11 <: Data,
                         T12 <: Data,
                         T13 <: Data,
                         T14 <: Data,
                         T15 <: Data,
                         T16 <: Data](val payloadType1: HardType[T1],
                                      val payloadType2: HardType[T2],
                                      val payloadType3: HardType[T3],
                                      val payloadType4: HardType[T4],
                                      val payloadType5: HardType[T5],
                                      val payloadType6: HardType[T6],
                                      val payloadType7: HardType[T7],
                                      val payloadType8: HardType[T8],
                                      val payloadType9: HardType[T9],
                                      val payloadType10: HardType[T10],
                                      val payloadType11: HardType[T11],
                                      val payloadType12: HardType[T12],
                                      val payloadType13: HardType[T13],
                                      val payloadType14: HardType[T14],
                                      val payloadType15: HardType[T15],
                                      val payloadType16: HardType[T16])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
  val _8 = payloadType8()
  val _9 = payloadType9()
  val _10 = payloadType10()
  val _11 = payloadType11()
  val _12 = payloadType12()
  val _13 = payloadType13()
  val _14 = payloadType14()
  val _15 = payloadType15()
  val _16 = payloadType16()
}

case class TupleBundle17[T1 <: Data,
                         T2 <: Data,
                         T3 <: Data,
                         T4 <: Data,
                         T5 <: Data,
                         T6 <: Data,
                         T7 <: Data,
                         T8 <: Data,
                         T9 <: Data,
                         T10 <: Data,
                         T11 <: Data,
                         T12 <: Data,
                         T13 <: Data,
                         T14 <: Data,
                         T15 <: Data,
                         T16 <: Data,
                         T17 <: Data](val payloadType1: HardType[T1],
                                      val payloadType2: HardType[T2],
                                      val payloadType3: HardType[T3],
                                      val payloadType4: HardType[T4],
                                      val payloadType5: HardType[T5],
                                      val payloadType6: HardType[T6],
                                      val payloadType7: HardType[T7],
                                      val payloadType8: HardType[T8],
                                      val payloadType9: HardType[T9],
                                      val payloadType10: HardType[T10],
                                      val payloadType11: HardType[T11],
                                      val payloadType12: HardType[T12],
                                      val payloadType13: HardType[T13],
                                      val payloadType14: HardType[T14],
                                      val payloadType15: HardType[T15],
                                      val payloadType16: HardType[T16],
                                      val payloadType17: HardType[T17])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
  val _8 = payloadType8()
  val _9 = payloadType9()
  val _10 = payloadType10()
  val _11 = payloadType11()
  val _12 = payloadType12()
  val _13 = payloadType13()
  val _14 = payloadType14()
  val _15 = payloadType15()
  val _16 = payloadType16()
  val _17 = payloadType17()
}

case class TupleBundle18[T1 <: Data,
                         T2 <: Data,
                         T3 <: Data,
                         T4 <: Data,
                         T5 <: Data,
                         T6 <: Data,
                         T7 <: Data,
                         T8 <: Data,
                         T9 <: Data,
                         T10 <: Data,
                         T11 <: Data,
                         T12 <: Data,
                         T13 <: Data,
                         T14 <: Data,
                         T15 <: Data,
                         T16 <: Data,
                         T17 <: Data,
                         T18 <: Data](val payloadType1: HardType[T1],
                                      val payloadType2: HardType[T2],
                                      val payloadType3: HardType[T3],
                                      val payloadType4: HardType[T4],
                                      val payloadType5: HardType[T5],
                                      val payloadType6: HardType[T6],
                                      val payloadType7: HardType[T7],
                                      val payloadType8: HardType[T8],
                                      val payloadType9: HardType[T9],
                                      val payloadType10: HardType[T10],
                                      val payloadType11: HardType[T11],
                                      val payloadType12: HardType[T12],
                                      val payloadType13: HardType[T13],
                                      val payloadType14: HardType[T14],
                                      val payloadType15: HardType[T15],
                                      val payloadType16: HardType[T16],
                                      val payloadType17: HardType[T17],
                                      val payloadType18: HardType[T18])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
  val _8 = payloadType8()
  val _9 = payloadType9()
  val _10 = payloadType10()
  val _11 = payloadType11()
  val _12 = payloadType12()
  val _13 = payloadType13()
  val _14 = payloadType14()
  val _15 = payloadType15()
  val _16 = payloadType16()
  val _17 = payloadType17()
  val _18 = payloadType18()
}

case class TupleBundle19[T1 <: Data,
                         T2 <: Data,
                         T3 <: Data,
                         T4 <: Data,
                         T5 <: Data,
                         T6 <: Data,
                         T7 <: Data,
                         T8 <: Data,
                         T9 <: Data,
                         T10 <: Data,
                         T11 <: Data,
                         T12 <: Data,
                         T13 <: Data,
                         T14 <: Data,
                         T15 <: Data,
                         T16 <: Data,
                         T17 <: Data,
                         T18 <: Data,
                         T19 <: Data](val payloadType1: HardType[T1],
                                      val payloadType2: HardType[T2],
                                      val payloadType3: HardType[T3],
                                      val payloadType4: HardType[T4],
                                      val payloadType5: HardType[T5],
                                      val payloadType6: HardType[T6],
                                      val payloadType7: HardType[T7],
                                      val payloadType8: HardType[T8],
                                      val payloadType9: HardType[T9],
                                      val payloadType10: HardType[T10],
                                      val payloadType11: HardType[T11],
                                      val payloadType12: HardType[T12],
                                      val payloadType13: HardType[T13],
                                      val payloadType14: HardType[T14],
                                      val payloadType15: HardType[T15],
                                      val payloadType16: HardType[T16],
                                      val payloadType17: HardType[T17],
                                      val payloadType18: HardType[T18],
                                      val payloadType19: HardType[T19])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
  val _8 = payloadType8()
  val _9 = payloadType9()
  val _10 = payloadType10()
  val _11 = payloadType11()
  val _12 = payloadType12()
  val _13 = payloadType13()
  val _14 = payloadType14()
  val _15 = payloadType15()
  val _16 = payloadType16()
  val _17 = payloadType17()
  val _18 = payloadType18()
  val _19 = payloadType19()
}

case class TupleBundle20[T1 <: Data,
                         T2 <: Data,
                         T3 <: Data,
                         T4 <: Data,
                         T5 <: Data,
                         T6 <: Data,
                         T7 <: Data,
                         T8 <: Data,
                         T9 <: Data,
                         T10 <: Data,
                         T11 <: Data,
                         T12 <: Data,
                         T13 <: Data,
                         T14 <: Data,
                         T15 <: Data,
                         T16 <: Data,
                         T17 <: Data,
                         T18 <: Data,
                         T19 <: Data,
                         T20 <: Data](val payloadType1: HardType[T1],
                                      val payloadType2: HardType[T2],
                                      val payloadType3: HardType[T3],
                                      val payloadType4: HardType[T4],
                                      val payloadType5: HardType[T5],
                                      val payloadType6: HardType[T6],
                                      val payloadType7: HardType[T7],
                                      val payloadType8: HardType[T8],
                                      val payloadType9: HardType[T9],
                                      val payloadType10: HardType[T10],
                                      val payloadType11: HardType[T11],
                                      val payloadType12: HardType[T12],
                                      val payloadType13: HardType[T13],
                                      val payloadType14: HardType[T14],
                                      val payloadType15: HardType[T15],
                                      val payloadType16: HardType[T16],
                                      val payloadType17: HardType[T17],
                                      val payloadType18: HardType[T18],
                                      val payloadType19: HardType[T19],
                                      val payloadType20: HardType[T20])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
  val _8 = payloadType8()
  val _9 = payloadType9()
  val _10 = payloadType10()
  val _11 = payloadType11()
  val _12 = payloadType12()
  val _13 = payloadType13()
  val _14 = payloadType14()
  val _15 = payloadType15()
  val _16 = payloadType16()
  val _17 = payloadType17()
  val _18 = payloadType18()
  val _19 = payloadType19()
  val _20 = payloadType20()
}

case class TupleBundle21[T1 <: Data,
                         T2 <: Data,
                         T3 <: Data,
                         T4 <: Data,
                         T5 <: Data,
                         T6 <: Data,
                         T7 <: Data,
                         T8 <: Data,
                         T9 <: Data,
                         T10 <: Data,
                         T11 <: Data,
                         T12 <: Data,
                         T13 <: Data,
                         T14 <: Data,
                         T15 <: Data,
                         T16 <: Data,
                         T17 <: Data,
                         T18 <: Data,
                         T19 <: Data,
                         T20 <: Data,
                         T21 <: Data](val payloadType1: HardType[T1],
                                      val payloadType2: HardType[T2],
                                      val payloadType3: HardType[T3],
                                      val payloadType4: HardType[T4],
                                      val payloadType5: HardType[T5],
                                      val payloadType6: HardType[T6],
                                      val payloadType7: HardType[T7],
                                      val payloadType8: HardType[T8],
                                      val payloadType9: HardType[T9],
                                      val payloadType10: HardType[T10],
                                      val payloadType11: HardType[T11],
                                      val payloadType12: HardType[T12],
                                      val payloadType13: HardType[T13],
                                      val payloadType14: HardType[T14],
                                      val payloadType15: HardType[T15],
                                      val payloadType16: HardType[T16],
                                      val payloadType17: HardType[T17],
                                      val payloadType18: HardType[T18],
                                      val payloadType19: HardType[T19],
                                      val payloadType20: HardType[T20],
                                      val payloadType21: HardType[T21])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
  val _8 = payloadType8()
  val _9 = payloadType9()
  val _10 = payloadType10()
  val _11 = payloadType11()
  val _12 = payloadType12()
  val _13 = payloadType13()
  val _14 = payloadType14()
  val _15 = payloadType15()
  val _16 = payloadType16()
  val _17 = payloadType17()
  val _18 = payloadType18()
  val _19 = payloadType19()
  val _20 = payloadType20()
  val _21 = payloadType21()
}

case class TupleBundle22[T1 <: Data,
                         T2 <: Data,
                         T3 <: Data,
                         T4 <: Data,
                         T5 <: Data,
                         T6 <: Data,
                         T7 <: Data,
                         T8 <: Data,
                         T9 <: Data,
                         T10 <: Data,
                         T11 <: Data,
                         T12 <: Data,
                         T13 <: Data,
                         T14 <: Data,
                         T15 <: Data,
                         T16 <: Data,
                         T17 <: Data,
                         T18 <: Data,
                         T19 <: Data,
                         T20 <: Data,
                         T21 <: Data,
                         T22 <: Data](val payloadType1: HardType[T1],
                                      val payloadType2: HardType[T2],
                                      val payloadType3: HardType[T3],
                                      val payloadType4: HardType[T4],
                                      val payloadType5: HardType[T5],
                                      val payloadType6: HardType[T6],
                                      val payloadType7: HardType[T7],
                                      val payloadType8: HardType[T8],
                                      val payloadType9: HardType[T9],
                                      val payloadType10: HardType[T10],
                                      val payloadType11: HardType[T11],
                                      val payloadType12: HardType[T12],
                                      val payloadType13: HardType[T13],
                                      val payloadType14: HardType[T14],
                                      val payloadType15: HardType[T15],
                                      val payloadType16: HardType[T16],
                                      val payloadType17: HardType[T17],
                                      val payloadType18: HardType[T18],
                                      val payloadType19: HardType[T19],
                                      val payloadType20: HardType[T20],
                                      val payloadType21: HardType[T21],
                                      val payloadType22: HardType[T22])
    extends TupleBundleBase {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
  val _8 = payloadType8()
  val _9 = payloadType9()
  val _10 = payloadType10()
  val _11 = payloadType11()
  val _12 = payloadType12()
  val _13 = payloadType13()
  val _14 = payloadType14()
  val _15 = payloadType15()
  val _16 = payloadType16()
  val _17 = payloadType17()
  val _18 = payloadType18()
  val _19 = payloadType19()
  val _20 = payloadType20()
  val _21 = payloadType21()
  val _22 = payloadType22()
}

object TupleBundle {
  def apply[T1 <: Data, T2 <: Data](input1: T1, input2: T2) = {
    val t2 = TupleBundle2(HardType(input1), HardType(input2))
    t2._1 := input1
    t2._2 := input2
    t2
  }

  def apply[T1 <: Data, T2 <: Data, T3 <: Data](input1: T1,
                                                input2: T2,
                                                input3: T3) = {
    val t3 = TupleBundle3(HardType(input1), HardType(input2), HardType(input3))
    t3._1 := input1
    t3._2 := input2
    t3._3 := input3
    t3
  }

  def apply[T1 <: Data, T2 <: Data, T3 <: Data, T4 <: Data](input1: T1,
                                                            input2: T2,
                                                            input3: T3,
                                                            input4: T4) = {
    val t4 = TupleBundle4(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4)
    )
    t4._1 := input1
    t4._2 := input2
    t4._3 := input3
    t4._4 := input4
    t4
  }

  def apply[T1 <: Data, T2 <: Data, T3 <: Data, T4 <: Data, T5 <: Data](
    input1: T1,
    input2: T2,
    input3: T3,
    input4: T4,
    input5: T5
  ) = {
    val t5 = TupleBundle5(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5)
    )
    t5._1 := input1
    t5._2 := input2
    t5._3 := input3
    t5._4 := input4
    t5._5 := input5
    t5
  }

  def apply[T1 <: Data,
            T2 <: Data,
            T3 <: Data,
            T4 <: Data,
            T5 <: Data,
            T6 <: Data](input1: T1,
                        input2: T2,
                        input3: T3,
                        input4: T4,
                        input5: T5,
                        input6: T6) = {
    val t6 = TupleBundle6(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5),
      HardType(input6)
    )
    t6._1 := input1
    t6._2 := input2
    t6._3 := input3
    t6._4 := input4
    t6._5 := input5
    t6._6 := input6
    t6
  }

  def apply[T1 <: Data,
            T2 <: Data,
            T3 <: Data,
            T4 <: Data,
            T5 <: Data,
            T6 <: Data,
            T7 <: Data](input1: T1,
                        input2: T2,
                        input3: T3,
                        input4: T4,
                        input5: T5,
                        input6: T6,
                        input7: T7) = {
    val t7 = TupleBundle7(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5),
      HardType(input6),
      HardType(input7)
    )
    t7._1 := input1
    t7._2 := input2
    t7._3 := input3
    t7._4 := input4
    t7._5 := input5
    t7._6 := input6
    t7._7 := input7
    t7
  }

  def apply[T1 <: Data,
            T2 <: Data,
            T3 <: Data,
            T4 <: Data,
            T5 <: Data,
            T6 <: Data,
            T7 <: Data,
            T8 <: Data](input1: T1,
                        input2: T2,
                        input3: T3,
                        input4: T4,
                        input5: T5,
                        input6: T6,
                        input7: T7,
                        input8: T8) = {
    val t8 = TupleBundle8(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5),
      HardType(input6),
      HardType(input7),
      HardType(input8)
    )
    t8._1 := input1
    t8._2 := input2
    t8._3 := input3
    t8._4 := input4
    t8._5 := input5
    t8._6 := input6
    t8._7 := input7
    t8._8 := input8
    t8
  }

  def apply[T1 <: Data,
            T2 <: Data,
            T3 <: Data,
            T4 <: Data,
            T5 <: Data,
            T6 <: Data,
            T7 <: Data,
            T8 <: Data,
            T9 <: Data](input1: T1,
                        input2: T2,
                        input3: T3,
                        input4: T4,
                        input5: T5,
                        input6: T6,
                        input7: T7,
                        input8: T8,
                        input9: T9) = {
    val t9 = TupleBundle9(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5),
      HardType(input6),
      HardType(input7),
      HardType(input8),
      HardType(input9)
    )
    t9._1 := input1
    t9._2 := input2
    t9._3 := input3
    t9._4 := input4
    t9._5 := input5
    t9._6 := input6
    t9._7 := input7
    t9._8 := input8
    t9._9 := input9
    t9
  }

  def apply[T1 <: Data,
            T2 <: Data,
            T3 <: Data,
            T4 <: Data,
            T5 <: Data,
            T6 <: Data,
            T7 <: Data,
            T8 <: Data,
            T9 <: Data,
            T10 <: Data](input1: T1,
                         input2: T2,
                         input3: T3,
                         input4: T4,
                         input5: T5,
                         input6: T6,
                         input7: T7,
                         input8: T8,
                         input9: T9,
                         input10: T10) = {
    val t10 = TupleBundle10(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5),
      HardType(input6),
      HardType(input7),
      HardType(input8),
      HardType(input9),
      HardType(input10)
    )
    t10._1 := input1
    t10._2 := input2
    t10._3 := input3
    t10._4 := input4
    t10._5 := input5
    t10._6 := input6
    t10._7 := input7
    t10._8 := input8
    t10._9 := input9
    t10._10 := input10
    t10
  }

  def apply[T1 <: Data,
            T2 <: Data,
            T3 <: Data,
            T4 <: Data,
            T5 <: Data,
            T6 <: Data,
            T7 <: Data,
            T8 <: Data,
            T9 <: Data,
            T10 <: Data,
            T11 <: Data](input1: T1,
                         input2: T2,
                         input3: T3,
                         input4: T4,
                         input5: T5,
                         input6: T6,
                         input7: T7,
                         input8: T8,
                         input9: T9,
                         input10: T10,
                         input11: T11) = {
    val t11 = TupleBundle11(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5),
      HardType(input6),
      HardType(input7),
      HardType(input8),
      HardType(input9),
      HardType(input10),
      HardType(input11)
    )
    t11._1 := input1
    t11._2 := input2
    t11._3 := input3
    t11._4 := input4
    t11._5 := input5
    t11._6 := input6
    t11._7 := input7
    t11._8 := input8
    t11._9 := input9
    t11._10 := input10
    t11._11 := input11
    t11
  }

  def apply[T1 <: Data,
            T2 <: Data,
            T3 <: Data,
            T4 <: Data,
            T5 <: Data,
            T6 <: Data,
            T7 <: Data,
            T8 <: Data,
            T9 <: Data,
            T10 <: Data,
            T11 <: Data,
            T12 <: Data](input1: T1,
                         input2: T2,
                         input3: T3,
                         input4: T4,
                         input5: T5,
                         input6: T6,
                         input7: T7,
                         input8: T8,
                         input9: T9,
                         input10: T10,
                         input11: T11,
                         input12: T12) = {
    val t12 = TupleBundle12(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5),
      HardType(input6),
      HardType(input7),
      HardType(input8),
      HardType(input9),
      HardType(input10),
      HardType(input11),
      HardType(input12)
    )
    t12._1 := input1
    t12._2 := input2
    t12._3 := input3
    t12._4 := input4
    t12._5 := input5
    t12._6 := input6
    t12._7 := input7
    t12._8 := input8
    t12._9 := input9
    t12._10 := input10
    t12._11 := input11
    t12._12 := input12
    t12
  }

  def apply[T1 <: Data,
            T2 <: Data,
            T3 <: Data,
            T4 <: Data,
            T5 <: Data,
            T6 <: Data,
            T7 <: Data,
            T8 <: Data,
            T9 <: Data,
            T10 <: Data,
            T11 <: Data,
            T12 <: Data,
            T13 <: Data](input1: T1,
                         input2: T2,
                         input3: T3,
                         input4: T4,
                         input5: T5,
                         input6: T6,
                         input7: T7,
                         input8: T8,
                         input9: T9,
                         input10: T10,
                         input11: T11,
                         input12: T12,
                         input13: T13) = {
    val t13 = TupleBundle13(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5),
      HardType(input6),
      HardType(input7),
      HardType(input8),
      HardType(input9),
      HardType(input10),
      HardType(input11),
      HardType(input12),
      HardType(input13)
    )
    t13._1 := input1
    t13._2 := input2
    t13._3 := input3
    t13._4 := input4
    t13._5 := input5
    t13._6 := input6
    t13._7 := input7
    t13._8 := input8
    t13._9 := input9
    t13._10 := input10
    t13._11 := input11
    t13._12 := input12
    t13._13 := input13
    t13
  }

  def apply[T1 <: Data,
            T2 <: Data,
            T3 <: Data,
            T4 <: Data,
            T5 <: Data,
            T6 <: Data,
            T7 <: Data,
            T8 <: Data,
            T9 <: Data,
            T10 <: Data,
            T11 <: Data,
            T12 <: Data,
            T13 <: Data,
            T14 <: Data](input1: T1,
                         input2: T2,
                         input3: T3,
                         input4: T4,
                         input5: T5,
                         input6: T6,
                         input7: T7,
                         input8: T8,
                         input9: T9,
                         input10: T10,
                         input11: T11,
                         input12: T12,
                         input13: T13,
                         input14: T14) = {
    val t14 = TupleBundle14(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5),
      HardType(input6),
      HardType(input7),
      HardType(input8),
      HardType(input9),
      HardType(input10),
      HardType(input11),
      HardType(input12),
      HardType(input13),
      HardType(input14)
    )
    t14._1 := input1
    t14._2 := input2
    t14._3 := input3
    t14._4 := input4
    t14._5 := input5
    t14._6 := input6
    t14._7 := input7
    t14._8 := input8
    t14._9 := input9
    t14._10 := input10
    t14._11 := input11
    t14._12 := input12
    t14._13 := input13
    t14._14 := input14
    t14
  }

  def apply[T1 <: Data,
            T2 <: Data,
            T3 <: Data,
            T4 <: Data,
            T5 <: Data,
            T6 <: Data,
            T7 <: Data,
            T8 <: Data,
            T9 <: Data,
            T10 <: Data,
            T11 <: Data,
            T12 <: Data,
            T13 <: Data,
            T14 <: Data,
            T15 <: Data](input1: T1,
                         input2: T2,
                         input3: T3,
                         input4: T4,
                         input5: T5,
                         input6: T6,
                         input7: T7,
                         input8: T8,
                         input9: T9,
                         input10: T10,
                         input11: T11,
                         input12: T12,
                         input13: T13,
                         input14: T14,
                         input15: T15) = {
    val t15 = TupleBundle15(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5),
      HardType(input6),
      HardType(input7),
      HardType(input8),
      HardType(input9),
      HardType(input10),
      HardType(input11),
      HardType(input12),
      HardType(input13),
      HardType(input14),
      HardType(input15)
    )
    t15._1 := input1
    t15._2 := input2
    t15._3 := input3
    t15._4 := input4
    t15._5 := input5
    t15._6 := input6
    t15._7 := input7
    t15._8 := input8
    t15._9 := input9
    t15._10 := input10
    t15._11 := input11
    t15._12 := input12
    t15._13 := input13
    t15._14 := input14
    t15._15 := input15
    t15
  }

  def apply[T1 <: Data,
            T2 <: Data,
            T3 <: Data,
            T4 <: Data,
            T5 <: Data,
            T6 <: Data,
            T7 <: Data,
            T8 <: Data,
            T9 <: Data,
            T10 <: Data,
            T11 <: Data,
            T12 <: Data,
            T13 <: Data,
            T14 <: Data,
            T15 <: Data,
            T16 <: Data](input1: T1,
                         input2: T2,
                         input3: T3,
                         input4: T4,
                         input5: T5,
                         input6: T6,
                         input7: T7,
                         input8: T8,
                         input9: T9,
                         input10: T10,
                         input11: T11,
                         input12: T12,
                         input13: T13,
                         input14: T14,
                         input15: T15,
                         input16: T16) = {
    val t16 = TupleBundle16(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5),
      HardType(input6),
      HardType(input7),
      HardType(input8),
      HardType(input9),
      HardType(input10),
      HardType(input11),
      HardType(input12),
      HardType(input13),
      HardType(input14),
      HardType(input15),
      HardType(input16)
    )
    t16._1 := input1
    t16._2 := input2
    t16._3 := input3
    t16._4 := input4
    t16._5 := input5
    t16._6 := input6
    t16._7 := input7
    t16._8 := input8
    t16._9 := input9
    t16._10 := input10
    t16._11 := input11
    t16._12 := input12
    t16._13 := input13
    t16._14 := input14
    t16._15 := input15
    t16._16 := input16
    t16
  }

  def apply[T1 <: Data,
            T2 <: Data,
            T3 <: Data,
            T4 <: Data,
            T5 <: Data,
            T6 <: Data,
            T7 <: Data,
            T8 <: Data,
            T9 <: Data,
            T10 <: Data,
            T11 <: Data,
            T12 <: Data,
            T13 <: Data,
            T14 <: Data,
            T15 <: Data,
            T16 <: Data,
            T17 <: Data](input1: T1,
                         input2: T2,
                         input3: T3,
                         input4: T4,
                         input5: T5,
                         input6: T6,
                         input7: T7,
                         input8: T8,
                         input9: T9,
                         input10: T10,
                         input11: T11,
                         input12: T12,
                         input13: T13,
                         input14: T14,
                         input15: T15,
                         input16: T16,
                         input17: T17) = {
    val t17 = TupleBundle17(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5),
      HardType(input6),
      HardType(input7),
      HardType(input8),
      HardType(input9),
      HardType(input10),
      HardType(input11),
      HardType(input12),
      HardType(input13),
      HardType(input14),
      HardType(input15),
      HardType(input16),
      HardType(input17)
    )
    t17._1 := input1
    t17._2 := input2
    t17._3 := input3
    t17._4 := input4
    t17._5 := input5
    t17._6 := input6
    t17._7 := input7
    t17._8 := input8
    t17._9 := input9
    t17._10 := input10
    t17._11 := input11
    t17._12 := input12
    t17._13 := input13
    t17._14 := input14
    t17._15 := input15
    t17._16 := input16
    t17._17 := input17
    t17
  }

  def apply[T1 <: Data,
            T2 <: Data,
            T3 <: Data,
            T4 <: Data,
            T5 <: Data,
            T6 <: Data,
            T7 <: Data,
            T8 <: Data,
            T9 <: Data,
            T10 <: Data,
            T11 <: Data,
            T12 <: Data,
            T13 <: Data,
            T14 <: Data,
            T15 <: Data,
            T16 <: Data,
            T17 <: Data,
            T18 <: Data](input1: T1,
                         input2: T2,
                         input3: T3,
                         input4: T4,
                         input5: T5,
                         input6: T6,
                         input7: T7,
                         input8: T8,
                         input9: T9,
                         input10: T10,
                         input11: T11,
                         input12: T12,
                         input13: T13,
                         input14: T14,
                         input15: T15,
                         input16: T16,
                         input17: T17,
                         input18: T18) = {
    val t18 = TupleBundle18(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5),
      HardType(input6),
      HardType(input7),
      HardType(input8),
      HardType(input9),
      HardType(input10),
      HardType(input11),
      HardType(input12),
      HardType(input13),
      HardType(input14),
      HardType(input15),
      HardType(input16),
      HardType(input17),
      HardType(input18)
    )
    t18._1 := input1
    t18._2 := input2
    t18._3 := input3
    t18._4 := input4
    t18._5 := input5
    t18._6 := input6
    t18._7 := input7
    t18._8 := input8
    t18._9 := input9
    t18._10 := input10
    t18._11 := input11
    t18._12 := input12
    t18._13 := input13
    t18._14 := input14
    t18._15 := input15
    t18._16 := input16
    t18._17 := input17
    t18._18 := input18
    t18
  }

  def apply[T1 <: Data,
            T2 <: Data,
            T3 <: Data,
            T4 <: Data,
            T5 <: Data,
            T6 <: Data,
            T7 <: Data,
            T8 <: Data,
            T9 <: Data,
            T10 <: Data,
            T11 <: Data,
            T12 <: Data,
            T13 <: Data,
            T14 <: Data,
            T15 <: Data,
            T16 <: Data,
            T17 <: Data,
            T18 <: Data,
            T19 <: Data](input1: T1,
                         input2: T2,
                         input3: T3,
                         input4: T4,
                         input5: T5,
                         input6: T6,
                         input7: T7,
                         input8: T8,
                         input9: T9,
                         input10: T10,
                         input11: T11,
                         input12: T12,
                         input13: T13,
                         input14: T14,
                         input15: T15,
                         input16: T16,
                         input17: T17,
                         input18: T18,
                         input19: T19) = {
    val t19 = TupleBundle19(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5),
      HardType(input6),
      HardType(input7),
      HardType(input8),
      HardType(input9),
      HardType(input10),
      HardType(input11),
      HardType(input12),
      HardType(input13),
      HardType(input14),
      HardType(input15),
      HardType(input16),
      HardType(input17),
      HardType(input18),
      HardType(input19)
    )
    t19._1 := input1
    t19._2 := input2
    t19._3 := input3
    t19._4 := input4
    t19._5 := input5
    t19._6 := input6
    t19._7 := input7
    t19._8 := input8
    t19._9 := input9
    t19._10 := input10
    t19._11 := input11
    t19._12 := input12
    t19._13 := input13
    t19._14 := input14
    t19._15 := input15
    t19._16 := input16
    t19._17 := input17
    t19._18 := input18
    t19._19 := input19
    t19
  }

  def apply[T1 <: Data,
            T2 <: Data,
            T3 <: Data,
            T4 <: Data,
            T5 <: Data,
            T6 <: Data,
            T7 <: Data,
            T8 <: Data,
            T9 <: Data,
            T10 <: Data,
            T11 <: Data,
            T12 <: Data,
            T13 <: Data,
            T14 <: Data,
            T15 <: Data,
            T16 <: Data,
            T17 <: Data,
            T18 <: Data,
            T19 <: Data,
            T20 <: Data](input1: T1,
                         input2: T2,
                         input3: T3,
                         input4: T4,
                         input5: T5,
                         input6: T6,
                         input7: T7,
                         input8: T8,
                         input9: T9,
                         input10: T10,
                         input11: T11,
                         input12: T12,
                         input13: T13,
                         input14: T14,
                         input15: T15,
                         input16: T16,
                         input17: T17,
                         input18: T18,
                         input19: T19,
                         input20: T20) = {
    val t20 = TupleBundle20(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5),
      HardType(input6),
      HardType(input7),
      HardType(input8),
      HardType(input9),
      HardType(input10),
      HardType(input11),
      HardType(input12),
      HardType(input13),
      HardType(input14),
      HardType(input15),
      HardType(input16),
      HardType(input17),
      HardType(input18),
      HardType(input19),
      HardType(input20)
    )
    t20._1 := input1
    t20._2 := input2
    t20._3 := input3
    t20._4 := input4
    t20._5 := input5
    t20._6 := input6
    t20._7 := input7
    t20._8 := input8
    t20._9 := input9
    t20._10 := input10
    t20._11 := input11
    t20._12 := input12
    t20._13 := input13
    t20._14 := input14
    t20._15 := input15
    t20._16 := input16
    t20._17 := input17
    t20._18 := input18
    t20._19 := input19
    t20._20 := input20
    t20
  }

  def apply[T1 <: Data,
            T2 <: Data,
            T3 <: Data,
            T4 <: Data,
            T5 <: Data,
            T6 <: Data,
            T7 <: Data,
            T8 <: Data,
            T9 <: Data,
            T10 <: Data,
            T11 <: Data,
            T12 <: Data,
            T13 <: Data,
            T14 <: Data,
            T15 <: Data,
            T16 <: Data,
            T17 <: Data,
            T18 <: Data,
            T19 <: Data,
            T20 <: Data,
            T21 <: Data](input1: T1,
                         input2: T2,
                         input3: T3,
                         input4: T4,
                         input5: T5,
                         input6: T6,
                         input7: T7,
                         input8: T8,
                         input9: T9,
                         input10: T10,
                         input11: T11,
                         input12: T12,
                         input13: T13,
                         input14: T14,
                         input15: T15,
                         input16: T16,
                         input17: T17,
                         input18: T18,
                         input19: T19,
                         input20: T20,
                         input21: T21) = {
    val t21 = TupleBundle21(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5),
      HardType(input6),
      HardType(input7),
      HardType(input8),
      HardType(input9),
      HardType(input10),
      HardType(input11),
      HardType(input12),
      HardType(input13),
      HardType(input14),
      HardType(input15),
      HardType(input16),
      HardType(input17),
      HardType(input18),
      HardType(input19),
      HardType(input20),
      HardType(input21)
    )
    t21._1 := input1
    t21._2 := input2
    t21._3 := input3
    t21._4 := input4
    t21._5 := input5
    t21._6 := input6
    t21._7 := input7
    t21._8 := input8
    t21._9 := input9
    t21._10 := input10
    t21._11 := input11
    t21._12 := input12
    t21._13 := input13
    t21._14 := input14
    t21._15 := input15
    t21._16 := input16
    t21._17 := input17
    t21._18 := input18
    t21._19 := input19
    t21._20 := input20
    t21._21 := input21
    t21
  }

  def apply[T1 <: Data,
            T2 <: Data,
            T3 <: Data,
            T4 <: Data,
            T5 <: Data,
            T6 <: Data,
            T7 <: Data,
            T8 <: Data,
            T9 <: Data,
            T10 <: Data,
            T11 <: Data,
            T12 <: Data,
            T13 <: Data,
            T14 <: Data,
            T15 <: Data,
            T16 <: Data,
            T17 <: Data,
            T18 <: Data,
            T19 <: Data,
            T20 <: Data,
            T21 <: Data,
            T22 <: Data](input1: T1,
                         input2: T2,
                         input3: T3,
                         input4: T4,
                         input5: T5,
                         input6: T6,
                         input7: T7,
                         input8: T8,
                         input9: T9,
                         input10: T10,
                         input11: T11,
                         input12: T12,
                         input13: T13,
                         input14: T14,
                         input15: T15,
                         input16: T16,
                         input17: T17,
                         input18: T18,
                         input19: T19,
                         input20: T20,
                         input21: T21,
                         input22: T22) = {
    val t22 = TupleBundle22(
      HardType(input1),
      HardType(input2),
      HardType(input3),
      HardType(input4),
      HardType(input5),
      HardType(input6),
      HardType(input7),
      HardType(input8),
      HardType(input9),
      HardType(input10),
      HardType(input11),
      HardType(input12),
      HardType(input13),
      HardType(input14),
      HardType(input15),
      HardType(input16),
      HardType(input17),
      HardType(input18),
      HardType(input19),
      HardType(input20),
      HardType(input21),
      HardType(input22)
    )
    t22._1 := input1
    t22._2 := input2
    t22._3 := input3
    t22._4 := input4
    t22._5 := input5
    t22._6 := input6
    t22._7 := input7
    t22._8 := input8
    t22._9 := input9
    t22._10 := input10
    t22._11 := input11
    t22._12 := input12
    t22._13 := input13
    t22._14 := input14
    t22._15 := input15
    t22._16 := input16
    t22._17 := input17
    t22._18 := input18
    t22._19 := input19
    t22._20 := input20
    t22._21 := input21
    t22._22 := input22
    t22
  }
}

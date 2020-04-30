package spinal.lib

import spinal.core.Data
import spinal.core.HardType
import spinal.core.Bundle

case class TupleBundle1[
    T1 <: Data
    ](
        val payloadType1: HardType[T1]
        ) extends Bundle {
  val _1 = payloadType1()
}

case class TupleBundle2[
    T1 <: Data,
    T2 <: Data
    ](
        val payloadType1: HardType[T1],
        val payloadType2: HardType[T2]
        ) extends Bundle {
  val _1 = payloadType1()
  val _2 = payloadType2()
}

case class TupleBundle3[
    T1 <: Data,
    T2 <: Data,
    T3 <: Data
    ](
        val payloadType1: HardType[T1],
        val payloadType2: HardType[T2],
        val payloadType3: HardType[T3]
        ) extends Bundle {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
}

case class TupleBundle4[
    T1 <: Data,
    T2 <: Data,
    T3 <: Data,
    T4 <: Data
    ](
        val payloadType1: HardType[T1],
        val payloadType2: HardType[T2],
        val payloadType3: HardType[T3],
        val payloadType4: HardType[T4]
        ) extends Bundle {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
}

case class TupleBundle5[
    T1 <: Data,
    T2 <: Data,
    T3 <: Data,
    T4 <: Data,
    T5 <: Data
    ](
        val payloadType1: HardType[T1],
        val payloadType2: HardType[T2],
        val payloadType3: HardType[T3],
        val payloadType4: HardType[T4],
        val payloadType5: HardType[T5]
        ) extends Bundle {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
}

case class TupleBundle6[
    T1 <: Data,
    T2 <: Data,
    T3 <: Data,
    T4 <: Data,
    T5 <: Data,
    T6 <: Data
    ](
        val payloadType1: HardType[T1],
        val payloadType2: HardType[T2],
        val payloadType3: HardType[T3],
        val payloadType4: HardType[T4],
        val payloadType5: HardType[T5],
        val payloadType6: HardType[T6]
        ) extends Bundle {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
}

case class TupleBundle7[
    T1 <: Data,
    T2 <: Data,
    T3 <: Data,
    T4 <: Data,
    T5 <: Data,
    T6 <: Data,
    T7 <: Data
    ](
        val payloadType1: HardType[T1],
        val payloadType2: HardType[T2],
        val payloadType3: HardType[T3],
        val payloadType4: HardType[T4],
        val payloadType5: HardType[T5],
        val payloadType6: HardType[T6],
        val payloadType7: HardType[T7]
        ) extends Bundle {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
}

case class TupleBundle8[
    T1 <: Data,
    T2 <: Data,
    T3 <: Data,
    T4 <: Data,
    T5 <: Data,
    T6 <: Data,
    T7 <: Data,
    T8 <: Data
    ](
        val payloadType1: HardType[T1],
        val payloadType2: HardType[T2],
        val payloadType3: HardType[T3],
        val payloadType4: HardType[T4],
        val payloadType5: HardType[T5],
        val payloadType6: HardType[T6],
        val payloadType7: HardType[T7],
        val payloadType8: HardType[T8]
        ) extends Bundle {
  val _1 = payloadType1()
  val _2 = payloadType2()
  val _3 = payloadType3()
  val _4 = payloadType4()
  val _5 = payloadType5()
  val _6 = payloadType6()
  val _7 = payloadType7()
  val _8 = payloadType8()
}

case class TupleBundle9[
    T1 <: Data,
    T2 <: Data,
    T3 <: Data,
    T4 <: Data,
    T5 <: Data,
    T6 <: Data,
    T7 <: Data,
    T8 <: Data,
    T9 <: Data
    ](
        val payloadType1: HardType[T1],
        val payloadType2: HardType[T2],
        val payloadType3: HardType[T3],
        val payloadType4: HardType[T4],
        val payloadType5: HardType[T5],
        val payloadType6: HardType[T6],
        val payloadType7: HardType[T7],
        val payloadType8: HardType[T8],
        val payloadType9: HardType[T9]
        ) extends Bundle {
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

case class TupleBundle10[
    T1 <: Data,
    T2 <: Data,
    T3 <: Data,
    T4 <: Data,
    T5 <: Data,
    T6 <: Data,
    T7 <: Data,
    T8 <: Data,
    T9 <: Data,
    T10 <: Data
    ](
        val payloadType1: HardType[T1],
        val payloadType2: HardType[T2],
        val payloadType3: HardType[T3],
        val payloadType4: HardType[T4],
        val payloadType5: HardType[T5],
        val payloadType6: HardType[T6],
        val payloadType7: HardType[T7],
        val payloadType8: HardType[T8],
        val payloadType9: HardType[T9],
        val payloadType10: HardType[T10]
        ) extends Bundle {
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

case class TupleBundle11[
    T1 <: Data,
    T2 <: Data,
    T3 <: Data,
    T4 <: Data,
    T5 <: Data,
    T6 <: Data,
    T7 <: Data,
    T8 <: Data,
    T9 <: Data,
    T10 <: Data,
    T11 <: Data
    ](
        val payloadType1: HardType[T1],
        val payloadType2: HardType[T2],
        val payloadType3: HardType[T3],
        val payloadType4: HardType[T4],
        val payloadType5: HardType[T5],
        val payloadType6: HardType[T6],
        val payloadType7: HardType[T7],
        val payloadType8: HardType[T8],
        val payloadType9: HardType[T9],
        val payloadType10: HardType[T10],
        val payloadType11: HardType[T11]
        ) extends Bundle {
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

case class TupleBundle12[
    T1 <: Data,
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
    T12 <: Data
    ](
        val payloadType1: HardType[T1],
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
        val payloadType12: HardType[T12]
        ) extends Bundle {
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

case class TupleBundle13[
    T1 <: Data,
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
    T13 <: Data
    ](
        val payloadType1: HardType[T1],
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
        val payloadType13: HardType[T13]
        ) extends Bundle {
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

case class TupleBundle14[
    T1 <: Data,
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
    T14 <: Data
    ](
        val payloadType1: HardType[T1],
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
        val payloadType14: HardType[T14]
        ) extends Bundle {
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

case class TupleBundle15[
    T1 <: Data,
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
    T15 <: Data
    ](
        val payloadType1: HardType[T1],
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
        val payloadType15: HardType[T15]
        ) extends Bundle {
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

case class TupleBundle16[
    T1 <: Data,
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
    T16 <: Data
    ](
        val payloadType1: HardType[T1],
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
        val payloadType16: HardType[T16]
        ) extends Bundle {
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

case class TupleBundle17[
    T1 <: Data,
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
    T17 <: Data
    ](
        val payloadType1: HardType[T1],
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
        val payloadType17: HardType[T17]
        ) extends Bundle {
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

case class TupleBundle18[
    T1 <: Data,
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
    T18 <: Data
    ](
        val payloadType1: HardType[T1],
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
        val payloadType18: HardType[T18]
        ) extends Bundle {
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

case class TupleBundle19[
    T1 <: Data,
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
    T19 <: Data
    ](
        val payloadType1: HardType[T1],
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
        val payloadType19: HardType[T19]
        ) extends Bundle {
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

case class TupleBundle20[
    T1 <: Data,
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
    T20 <: Data
    ](
        val payloadType1: HardType[T1],
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
        val payloadType20: HardType[T20]
        ) extends Bundle {
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

case class TupleBundle21[
    T1 <: Data,
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
    T21 <: Data
    ](
        val payloadType1: HardType[T1],
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
        val payloadType21: HardType[T21]
        ) extends Bundle {
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

case class TupleBundle22[
    T1 <: Data,
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
    T22 <: Data
    ](
        val payloadType1: HardType[T1],
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
        val payloadType22: HardType[T22]
        ) extends Bundle {
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


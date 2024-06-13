package spinal.lib.logic

import spinal.core._
import spinal.lib._
import spinal.lib.eda.bench.{AlteraStdTargets, Bench, EfinixStdTargets, Rtl, XilinxStdTargets}
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable
import scala.math.BigInt


object Symplify{
  def getCache(addr : Bits) = signalCache(addr)(mutable.LinkedHashMap[Masked,Bool]())

  //Generate terms logic for the given input
  def logicOf(input : Bits,terms : Seq[Masked]) : Bool = terms.map(t => getCache(input).getOrElseUpdate(t,t === input)).asBits.orR

  //Decode 'input' by using an mapping[key, decoding] specification

  def apply(input: Bits, mapping: Iterable[(Masked, Masked)], resultWidth : Int) : Bits = {
    val addrWidth = widthOf(input)
    (for(bitId <- 0 until resultWidth) yield{
      val trueTerm = mapping.filter { case (k,t) => (t.care.testBit(bitId) && t.value.testBit(bitId))}.map(_._1)
      val falseTerm = mapping.filter { case (k,t) => (t.care.testBit(bitId) &&  !t.value.testBit(bitId))}.map(_._1)
      val symplifiedTerms = SymplifyBit.getPrimeImplicantsByTrueAndFalse(trueTerm.toSeq, falseTerm.toSeq, addrWidth)
      logicOf(input, symplifiedTerms)
    }).asBits
  }

  def apply(input: Bits, trueTerms: Iterable[Masked], falseTerms: Iterable[Masked]) : Bool = {
    val addrWidth = widthOf(input)
    val symplifiedTerms = SymplifyBit.getPrimeImplicantsByTrueAndFalse(trueTerms.toSeq, falseTerms.toSeq, addrWidth)
    logicOf(input, symplifiedTerms)
  }

  //Default is zero
  def apply(input : Bits, trueTerms : Iterable[Masked]) : Bool = {
    Symplify.logicOf(input, SymplifyBit.getPrimeImplicantsByTrueAndDontCare(trueTerms.toSeq, Nil, widthOf(input)))
  }

  def trueAndDontCare(input : Bits, trueTerms : Iterable[Masked], dontCare : Iterable[Masked]) : Bool = {
    Symplify.logicOf(input, SymplifyBit.getPrimeImplicantsByTrueAndDontCare(trueTerms.toSeq, dontCare.toSeq, widthOf(input)))
  }
}

object SymplifyBit{

  //Return a new term with only one bit difference with 'term' and not included in falseTerms. above => 0 to 1 dif, else 1 to 0 diff
  def genImplicitDontCare(falseTerms: Seq[Masked], term: Masked, bits: Int, above: Boolean): Masked = {
    for (i <- 0 until bits; if term.care.testBit(i)) {
      var t: Masked = null
      if(above) {
        if (!term.value.testBit(i))
          t = Masked(term.value.setBit(i), term.care)
      } else {
        if (term.value.testBit(i))
          t = Masked(term.value.clearBit(i), term.care)
      }
      if (t != null && !falseTerms.exists(_.intersects(t))) {
        t.isPrime = false
        return t
      }
    }
    null
  }

  //Return primes implicants for the trueTerms, falseTerms spec. Default value is don't care
  def getPrimeImplicantsByTrueAndFalse(trueTerms: Seq[Masked], falseTerms: Seq[Masked], inputWidth : Int): Seq[Masked] = {
    val primes = mutable.LinkedHashSet[Masked]()
    trueTerms.foreach(_.isPrime = true)
    falseTerms.foreach(_.isPrime = true)
    val trueTermByCareCount = (inputWidth to 0 by -1).map(b => trueTerms.filter(b == _.care.bitCount))
    //table[Vector[HashSet[Masked]]](careCount)(bitSetCount)
    val table = trueTermByCareCount.map(c => (0 to inputWidth).map(b => collection.mutable.Set(c.filter(b == _.value.bitCount): _*)))
    for (i <- 0 to inputWidth) {
      //Expends explicit terms
      for (j <- 0 until inputWidth - i){
        for(term <- table(i)(j)){
          table(i+1)(j) ++= table(i)(j+1).withFilter(_.isSimilarOneBitDifSmaller(term)).map(_.mergeOneBitDifSmaller(term))
        }
      }
      //Expends implicit don't care terms
      for (j <- 0 until inputWidth-i) {
        for (prime <- table(i)(j).withFilter(_.isPrime)) {
          val dc = genImplicitDontCare(falseTerms, prime, inputWidth, true)
          if (dc != null)
            table(i+1)(j) += dc mergeOneBitDifSmaller prime
        }
        for (prime <- table(i)(j+1).withFilter(_.isPrime)) {
          val dc = genImplicitDontCare(falseTerms, prime, inputWidth, false)
          if (dc != null)
            table(i+1)(j) += prime mergeOneBitDifSmaller dc
        }
      }
      for (r <- table(i))
        for (p <- r; if p.isPrime)
          primes += p
    }

    def optimise() {
      val duplicateds = primes.filter(prime => verifyTrueFalse(primes.filterNot(_ == prime), trueTerms, falseTerms))
      if(duplicateds.nonEmpty) {
        primes -= duplicateds.maxBy(_.care.bitCount)
        optimise()
      }
    }

    optimise()

    verifyTrueFalse(primes, trueTerms, falseTerms)
    var duplication = 0
    for(prime <- primes){
      if(verifyTrueFalse(primes.filterNot(_ == prime), trueTerms, falseTerms)){
        duplication += 1
      }
    }
    if(duplication != 0){
      PendingError(s"Duplicated primes : $duplication")
    }
    primes.toSeq
  }

  //Verify that the 'terms' doesn't violate the trueTerms ++ falseTerms spec
  def verifyTrueFalse(terms : Iterable[Masked], trueTerms : Seq[Masked], falseTerms : Seq[Masked]): Boolean ={
    return (trueTerms.forall(trueTerm => terms.exists(_ covers trueTerm))) && (falseTerms.forall(falseTerm => !terms.exists(_ covers falseTerm)))
  }

  def checkTrue(terms : Iterable[Masked], trueTerms : Seq[Masked]): Boolean ={
    return trueTerms.forall(trueTerm => terms.exists(_ covers trueTerm))
  }


  def getPrimeImplicantsByTrue(trueTerms: Seq[Masked], inputWidth : Int) : Seq[Masked] = getPrimeImplicantsByTrueAndDontCare(trueTerms, Nil, inputWidth)

  // Return primes implicants for the trueTerms, default value is False.
  // You can insert don't care values by adding non-prime implicants in the trueTerms
  // Will simplify the trueTerms from the most constrained ones to the least constrained ones
  def getPrimeImplicantsByTrueAndDontCare(trueTerms: Seq[Masked],dontCareTerms: Seq[Masked], inputWidth : Int): Seq[Masked] = {
    val primes = mutable.LinkedHashSet[Masked]()
    trueTerms.foreach(_.isPrime = true)
    dontCareTerms.foreach(_.isPrime = false)
    val termsByCareCount = (inputWidth to 0 by -1).map(b => (trueTerms ++ dontCareTerms).filter(b == _.care.bitCount))
    //table[Vector[HashSet[Masked]]](careCount)(bitSetCount)
    val table = termsByCareCount.map(c => (0 to inputWidth).map(b => collection.mutable.Set(c.filter(m => b == m.value.bitCount): _*)))
    for (i <- 0 to inputWidth) {
      for (j <- 0 until inputWidth - i){
        for(term <- table(i)(j)){
          table(i+1)(j) ++= table(i)(j+1).withFilter(_.isSimilarOneBitDifSmaller(term)).map(_.mergeOneBitDifSmaller(term))
        }
      }
      for (r <- table(i))
        for (p <- r; if p.isPrime)
          primes += p
    }


    def optimise() {
      val duplicateds = primes.filter(prime => checkTrue(primes.filterNot(_ == prime), trueTerms))
      if(duplicateds.nonEmpty) {
        primes -= duplicateds.maxBy(_.care.bitCount)
        optimise()
      }
    }

    optimise()


    var duplication = 0
    for(prime <- primes){
      if(checkTrue(primes.filterNot(_ == prime), trueTerms)){
        duplication += 1
      }
    }
    if(duplication != 0){
      PendingError(s"Duplicated primes : $duplication")
    }
    primes.toSeq
  }

  def main(args: Array[String]) {
    {
      //      val default = Masked(0, 0xF)
      //      val primeImplicants = List(4, 8, 10, 11, 12, 15).map(v => Masked(v, 0xF))
      //      val dcImplicants = List(9, 14).map(v => Masked(v, 0xF).setPrime(false))
      //      val reducedPrimeImplicants = getPrimeImplicantsByTrueAndDontCare(primeImplicants, dcImplicants, 4)
      //      println("UUT")
      //      println(reducedPrimeImplicants.map(_.toString(4)).mkString("\n"))
      //      println("REF")
      //      println("-100\n10--\n1--0\n1-1-")
    }

    {
      val primeImplicants = List(0).map(v => Masked(v, 0xF))
      val dcImplicants = (1 to 15).map(v => Masked(v, 0xF))
      val reducedPrimeImplicants = getPrimeImplicantsByTrueAndDontCare(primeImplicants, dcImplicants, 4)
      println("UUT")
      println(reducedPrimeImplicants.map(_.toString(4)).mkString("\n"))
    }
    {
      val trueTerms = List(0, 15).map(v => Masked(v, 0xF))
      val falseTerms = List(3).map(v => Masked(v, 0xF))
      val primes =  getPrimeImplicantsByTrueAndFalse(trueTerms, falseTerms, 4)
      println(primes.map(_.toString(4)).mkString("\n"))
    }
  }
}








object SymplifyBench extends App{
  val all = List(
    Masked(BigInt("51"), BigInt("4261441663")) ,
    Masked(BigInt("1073741875"), BigInt("4261441663")) ,
    Masked(BigInt("8243"), BigInt("4261441663")) ,
    Masked(BigInt("12339"), BigInt("4261441663")) ,
    Masked(BigInt("16435"), BigInt("4261441663")) ,
    Masked(BigInt("24627"), BigInt("4261441663")) ,
    Masked(BigInt("28723"), BigInt("4261441663")) ,
    Masked(BigInt("19"), BigInt("28799")) ,
    Masked(BigInt("8211"), BigInt("28799")) ,
    Masked(BigInt("12307"), BigInt("28799")) ,
    Masked(BigInt("16403"), BigInt("28799")) ,
    Masked(BigInt("24595"), BigInt("28799")) ,
    Masked(BigInt("28691"), BigInt("28799")) ,
    Masked(BigInt("55"), BigInt("127")) ,
    Masked(BigInt("23"), BigInt("127")) ,
    Masked(BigInt("4147"), BigInt("4261441663")) ,
    Masked(BigInt("20531"), BigInt("4261441663")) ,
    Masked(BigInt("1073762355"), BigInt("4261441663")) ,
    Masked(BigInt("4115"), BigInt("4227887231")) ,
    Masked(BigInt("20499"), BigInt("4227887231")) ,
    Masked(BigInt("1073762323"), BigInt("4227887231")) ,
    Masked(BigInt("111"), BigInt("127")) ,
    Masked(BigInt("103"), BigInt("28799")) ,
    Masked(BigInt("99"), BigInt("28799")) ,
    Masked(BigInt("4195"), BigInt("28799")) ,
    Masked(BigInt("16483"), BigInt("28799")) ,
    Masked(BigInt("20579"), BigInt("28799")) ,
    Masked(BigInt("24675"), BigInt("28799")) ,
    Masked(BigInt("28771"), BigInt("28799")) ,
    Masked(BigInt("4211"), BigInt("28799")) ,
    Masked(BigInt("8307"), BigInt("28799")) ,
    Masked(BigInt("12403"), BigInt("28799")) ,
    Masked(BigInt("20595"), BigInt("28799")) ,
    Masked(BigInt("24691"), BigInt("28799")) ,
    Masked(BigInt("28787"), BigInt("28799")) ,
    Masked(BigInt("3"), BigInt("28799")) ,
    Masked(BigInt("4099"), BigInt("28799")) ,
    Masked(BigInt("8195"), BigInt("28799")) ,
    Masked(BigInt("16387"), BigInt("28799")) ,
    Masked(BigInt("20483"), BigInt("28799")) ,
    Masked(BigInt("35"), BigInt("28799")) ,
    Masked(BigInt("4131"), BigInt("28799")) ,
    Masked(BigInt("8227"), BigInt("28799")) ,
    Masked(BigInt("15"), BigInt("28799")) ,
    Masked(BigInt("115"), BigInt("4294967295")) ,
    Masked(BigInt("1048691"), BigInt("4294967295")) ,
    Masked(BigInt("807403635"), BigInt("4294967295")) ,
    Masked(BigInt("4111"), BigInt("28799")) ,
    Masked(BigInt("273678451"), BigInt("4294967295"))
  )

  val mayFlush = List(
    Masked(BigInt("111"), BigInt("127")),
    Masked(BigInt("103"), BigInt("28799")),
    Masked(BigInt("99"), BigInt("28799")),
    Masked(BigInt("4195"), BigInt("28799")),
    Masked(BigInt("16483"), BigInt("28799")),
    Masked(BigInt("20579"), BigInt("28799")),
    Masked(BigInt("24675"), BigInt("28799")),
    Masked(BigInt("28771"), BigInt("28799")),
    Masked(BigInt("4211"), BigInt("28799")),
    Masked(BigInt("8307"), BigInt("28799")),
    Masked(BigInt("12403"), BigInt("28799")),
    Masked(BigInt("20595"), BigInt("28799")),
    Masked(BigInt("24691"), BigInt("28799")),
    Masked(BigInt("28787"), BigInt("28799")),
    Masked(BigInt("3"), BigInt("28799")),
    Masked(BigInt("4099"), BigInt("28799")),
    Masked(BigInt("8195"), BigInt("28799")),
    Masked(BigInt("16387"), BigInt("28799")),
    Masked(BigInt("20483"), BigInt("28799")),
    Masked(BigInt("35"), BigInt("28799")),
    Masked(BigInt("4131"), BigInt("28799")),
    Masked(BigInt("8227"), BigInt("28799")),
    Masked(BigInt("115"), BigInt("4294967295")),
    Masked(BigInt("1048691"), BigInt("4294967295")),
    Masked(BigInt("807403635"), BigInt("4294967295")),
    Masked(BigInt("4111"), BigInt("28799")),
    Masked(BigInt("273678451"), BigInt("4294967295"))
  )
  val mayFlushNot = (all.toSet -- mayFlush).toList

  val symplify = Rtl(SpinalVerilog(new Component {
    setDefinitionName("simplify")
    val input = in Bits (32 bits)
    val output = out Bool()

    val bufferIn = Delay(input, 3)
    val value = Symplify(bufferIn, all)
    output := Delay(value, 3)
  }))


  val raw = Rtl(SpinalVerilog(new Component {
    setDefinitionName("raw")
    val input = in Bits (32 bits)
    val output = out Bool()

    val bufferIn = Delay(input, 3)
    val value = False
    switch(bufferIn){
      for(e <- all){
        is(new MaskedLiteral(e.value, e.care, 32)){
          value := True
        }
      }
    }
    output := Delay(value, 3)
  }))

  val symplifyFlush = Rtl(SpinalVerilog(new Component {
    setDefinitionName("simplifyFlush")
    val input = in Bits (32 bits)
    val output = out Bool()

    val bufferIn = Delay(input, 3)
    val value = Symplify(bufferIn, mayFlush, mayFlushNot)
    output := Delay(value, 3)
  }))


  val rawFlush = Rtl(SpinalVerilog(new Component {
    setDefinitionName("rawFlush")
    val input = in Bits (32 bits)
    val output = out Bool()

    val bufferIn = Delay(input, 3)
    val value = Bool().assignDontCare()
    switch(bufferIn) {
      for (e <- mayFlush) {
        is(new MaskedLiteral(e.value, e.care, 32)) {
          value := True
        }
      }
      for (e <- mayFlushNot) {
        is(new MaskedLiteral(e.value, e.care, 32)) {
          value := False
        }
      }
    }
    output := Delay(value, 3)
  }))


  val rtls = List(symplify, raw, symplifyFlush, rawFlush)

  val targets = XilinxStdTargets().take(2) ++ AlteraStdTargets() ++ EfinixStdTargets()

  Bench(rtls, targets)
}

/*
simplify ->
Artix 7 -> 92 Mhz 18 LUT 97 FF
Artix 7 -> 446 Mhz 18 LUT 98 FF
Cyclone V -> 364 Mhz 31 ALMs
Cyclone IV -> 250 Mhz 33 LUT 99 FF
Trion -> 181 Mhz LUT 37   FF 143
Trion -> 327 Mhz LUT 37   FF 143
Titanium -> 754 Mhz LUT 37   FF 143
Titanium -> 1176 Mhz LUT 37   FF 143
raw ->
Artix 7 -> 92 Mhz 19 LUT 97 FF
Artix 7 -> 471 Mhz 21 LUT 98 FF
Cyclone V -> 348 Mhz 32 ALMs
Cyclone IV -> 250 Mhz 33 LUT 99 FF
Trion -> 194 Mhz LUT 34   FF 141
Trion -> 338 Mhz LUT 34   FF 141
Titanium -> 682 Mhz LUT 34   FF 141
Titanium -> 1050 Mhz LUT 34   FF 141
simplifyFlush ->
Artix 7 -> 92 Mhz 2 LUT 13 FF
Artix 7 -> 495 Mhz 2 LUT 13 FF
Cyclone V -> 717 Mhz 4 ALMs
Cyclone IV -> 250 Mhz 1 LUT 15 FF
Trion -> 276 Mhz LUT 1   FF 15
Trion -> 506 Mhz LUT 1   FF 15
Titanium -> 1066 Mhz LUT 1   FF 15
Titanium -> 1455 Mhz LUT 1   FF 15
rawFlush ->
Artix 7 -> 92 Mhz 2 LUT 16 FF
Artix 7 -> 495 Mhz 2 LUT 16 FF
Cyclone V -> 470 Mhz 16 ALMs
Cyclone IV -> 250 Mhz 10 LUT 54 FF
Trion -> 245 Mhz LUT 15   FF 69
Trion -> 343 Mhz LUT 15   FF 69
Titanium -> 720 Mhz LUT 15   FF 69
Titanium -> 940 Mhz LUT 15   FF 69

 */
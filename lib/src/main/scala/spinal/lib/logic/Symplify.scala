package spinal.lib.logic

import spinal.core._
import spinal.lib._

import scala.collection.mutable


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
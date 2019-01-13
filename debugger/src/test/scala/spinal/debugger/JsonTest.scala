package spinal.debugger

import net.liftweb.json.DefaultFormats

/**
 * Created by PIC on 20.04.2015.
 */


object JsonTest {
  def main(args: Array[String]) {
    import net.liftweb.json.Extraction._
    import net.liftweb.json.JsonAST._
    import net.liftweb.json.Printer._
    implicit val formats = DefaultFormats // Brings in default date formats etc.


    class Child(name: String, age: Int, birthdate: Option[java.util.Date])
    class Address(street: String, city: String)
    class Person( name: String, address: Address, children: Seq[Child])
    val json = net.liftweb.json.parse( """
         { "name": "joe",
           "address": {
             "street": "Bulevard",
             "city": "Helsinki"
           },
           "children": [
             {
               "name": "Mary",
               "age": 5
               "birthdate": "2004-09-04T18:06:22Z"
             },
             {
               "name": "Mazy",
               "age": 3
             }
           ]
         }""")


    import net.liftweb.json.JsonDSL._

    val person = json.extract[Person]
    println(prettyRender(decompose(person)))

    case class Winner(id: Long, numbers: List[Int])
    case class Lotto(id: Long, winningNumbers: List[Int], winners: List[Winner], drawDate: Option[java.util.Date])

    val winners = List(Winner(23, List(2, 45, 34, 23, 3, 5)), Winner(54, List(52, 3, 12, 11, 18, 22)))
    val lotto = Lotto(5, List(2, 45, 34, 23, 7, 5, 3), winners, None)


    val j =
      ("lotto" ->
        ("lotto-id" -> lotto.id) ~
          ("winning-numbers" -> lotto.winningNumbers) ~
          ("draw-date" -> lotto.drawDate.map(_.toString)) ~
          ("winners" ->
            lotto.winners.map { w =>
              (("winner-id" -> w.id) ~
                ("numbers" -> w.numbers))}))

    println(prettyRender(j))


    println("done")
  }

}

//
//import argonaut._, Argonaut._
//
//case class Person(name: String, age: Int, things: List[String])
//import com.codahale.jerkson.Json._
//object JsonTest {
//
//  implicit def PersonCodecJson =
//    casecodec3(Person.apply, Person.unapply)("name", "age", "things")
//
//  val person =
//    Person("Bam Bam", 2, List("club"))
//
//  val json: Json =
//    person.asJson
//
//  val prettyprinted: String =
//    json.spaces2
//
//  val parsed: Option[Person] =
//    prettyprinted.decodeOption[Person]
//
//
//
//
//  def main(args: Array[String]) {
//    println(prettyprinted)
//    println
//
//    val jsonObject = Json.toJson(
//      Map(
//        "users" -> Seq(
//          toJson(
//            Map(
//              "name" -> toJson("Bob"),
//              "age" -> toJson(31),
//              "email" -> toJson("bob@gmail.com")
//            )
//          ),
//          toJson(
//            Map(
//              "name" -> toJson("Kiki"),
//              "age" -> toJson(25),
//              "email" -> JsNull
//            )
//          )
//        )
//      )
//    )
//  }
//}
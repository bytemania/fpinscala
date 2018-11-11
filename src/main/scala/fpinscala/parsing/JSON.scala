package fpinscala.parsing

trait JSON


object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON


  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    // we'll hide the string implicit conversion and promote strings to tokens instead
    // this is a bit nicer than having to write token everywhere
    import P.{string => _, _}
    implicit def tok(s: String): Parser[String] = token(P.string(s))

    def array: Parser[JArray] = surround("[","]")(value sep "," map (vs => JArray(vs.toIndexedSeq))) scope "array"
    def obj: Parser[JObject] = surround("{","}")(keyval sep "," map (kvs => JObject(kvs.toMap))) scope "object"
    def keyval:Parser[(String, JSON)] = escapedQuoted ** (":" *> value)
    def lit: Parser[JSON] = scope("literal") {
      "null".as(JNull) |
        double.map(JNumber) |
        escapedQuoted.map(JString) |
        "true".as(JBool(true)) |
        "false".as(JBool(false))
    }
    def value: Parser[JSON] = lit | obj | array
    root(whitespace *> (obj | array))
  }

}

object JSONExample extends App {
  val jsonTxt: String =
    """
      |{
      | "Company Name"       : "Microsoft Corporation",
      | "Ticker"             : "MSFT",
      | "Active"             : true,
      | "Price"              : 30.66,
      | "Shares outstanding" : 8.38e9,
      | "Related companies"  : ["HPQ", "IBM", "YHOO", "DELL", "GOOG"]
      |}
    """.stripMargin

  val malformedJson1 =
    """
      |{
      | "Company name" ; "Microsoft Corporation"
      |}
    """.stripMargin

  val malformedJson2 =
    """
      |[
      | [ "HPQ", "IBM",
      |   "YHOO", "DELL" ++
      |   "GOOG"
      | ]
      |]
    """.stripMargin

  val P = fpinscala.parsing.Reference
  import fpinscala.parsing.ReferenceTypes.Parser

  def printResult[E](e: Either[E, JSON]): Unit = e.fold(println, println)

  val json: Parser[JSON] = JSON.jsonParser(P)
  printResult {P.run(json)(jsonTxt)}
  println("--")
  printResult {P.run(json)(malformedJson1)}
  println("--")
  printResult {P.run(json)(malformedJson2)}
}

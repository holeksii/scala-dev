sealed trait Json:
  lazy val toJson: String = this match
    case JNumber(value)  => value.toString
    case JString(value)  => "\"" + value + "\""
    case JBoolean(value) => value.toString
    case JNull()         => "null"
    case JArray(elems)   => "[" + elems.map(_.toJson).mkString(", ") + "]"
    case JObject(fields) => toJson

case class JNumber(value: BigDecimal) extends Json
case class JString(value: String) extends Json
case class JBoolean(value: Boolean) extends Json
case class JNull() extends Json
case class JArray(elems: List[Json]) extends Json
case class JObject(fields: (String, Json)*) extends Json:
  override lazy val toJson: String =
    "{" + fields
      .map((name, value) => s""""$name": ${value.toJson}""")
      .mkString(", ") + "}"

object Json:
  import scala.language.implicitConversions

  implicit def stringToJson(s: String): Json = JString(s)
  implicit def intToJson(n: Int): Json = JNumber(n)
  implicit def booleanToJson(b: Boolean): Json = JBoolean(b)
  implicit def listToJson[T](l: List[T])(using f: T => Json): Json = JArray(
    l.map(f)
  )
  implicit def optionToJson[T](o: Option[T])(using f: T => Json): Json =
    o match {
      case Some(value) => f(value)
      case None        => JNull()
    }
end Json

def jobj(fields: (String, Json)*): Json = JObject(fields: _*)

jobj("name" -> "Oleksii", "grades" -> List(1, 2, 3), "money" -> None).toJson

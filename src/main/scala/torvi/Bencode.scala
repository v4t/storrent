package torvi

case class BencodeParseException(str: String) extends Exception

trait BencodeValue

case class BencodeStringValue(value: String) extends BencodeValue

case class BencodeIntValue(value: Long) extends BencodeValue

case class BencodeListValue(values: List[BencodeValue]) extends BencodeValue

case class BencodeDictValue(values: Map[BencodeStringValue, BencodeValue]) extends BencodeValue

import scala.util.parsing.combinator._

object BencodeParser extends RegexParsers {

  override def skipWhitespace: Boolean = false

  def parse(source: String) = {
    parseAll(bencode, source) match {
      case Success(matched, _) => matched
      case Failure(msg, _) =>
        println(msg)
        throw new BencodeParseException(msg)
      case Error(msg, _) =>
        println(msg)
        throw new BencodeParseException(msg)
    }
  }

  def bencode: Parser[List[BencodeValue]] = rep(value)

  def value: Parser[BencodeValue] = elem ||| list ||| dictionary

  def elem: Parser[BencodeValue] = emptyString ||| string ||| int

  def emptyString: Parser[BencodeValue] = "0:" ^^ (_ => BencodeStringValue(""))

  def string: Parser[BencodeStringValue] = ("""[1-9]\d*""".r <~ ":") >> { strLength =>
    repN(strLength.toInt, ".|\n".r) ^^ (i => BencodeStringValue(i.mkString))
  }

  def int: Parser[BencodeIntValue] = "i" ~> """(0|\-?[1-9]\d*)""".r <~ "e" ^^ (i => BencodeIntValue(i.toLong))

  def list: Parser[BencodeListValue] = "l" ~> rep(value) <~ "e" ^^ (i => BencodeListValue(i))

  def dictionary: Parser[BencodeDictValue] = "d" ~> rep(string ~ value) <~ "e" ^^ (i =>
    BencodeDictValue(i.map(x => (BencodeStringValue(x._1.value), x._2)).toMap))
}

object BencodeEncoder {

  def encode(values: List[BencodeValue]): String = encodeValues(values, "")

  private def encodeValues(values: List[BencodeValue], result: String): String = {
    if (values == Nil) return result
    val head = values.head
    val tail = values.tail
    head match {
      case BencodeStringValue(str) => encodeValues(tail, result + (str.size + ":" + str))
      case BencodeIntValue(int) => encodeValues(tail, result + ("i" + int + "e"))
      case BencodeListValue(list) => encodeValues(tail, result + ("l" + encodeValues(list, "") + "e"))
      case BencodeDictValue(dict) => encodeValues(tail, result + ("d" + encodeDict(dict) + "e"))
    }
  }

  private def encodeDict(dict: Map[BencodeStringValue, BencodeValue]): String = {
    val keys = dict.keys.toSeq.sortBy(_.value)
    val contents = for {
      key <- keys
    } yield encodeValues(List(key), "") + encodeValues(List(dict(key)), "")
    contents.mkString("")
  }

}
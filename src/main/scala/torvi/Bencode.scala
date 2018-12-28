package torvi

case class BencodeParseException(str: String) extends Exception

trait BencodeValue

case class BencodeStringValue(value: String) extends BencodeValue

case class BencodeIntValue(value: Long) extends BencodeValue

import scala.util.parsing.combinator._

object BencodeParser extends RegexParsers {

  override def skipWhitespace: Boolean = false

  def parse(source: String) = {
    parseAll(bencode, source) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => throw new BencodeParseException(msg)
      case Error(msg, _) => throw new BencodeParseException(msg)
    }
  }

  def bencode: Parser[List[BencodeValue]] = rep(value)

  def value: Parser[BencodeValue] = elem

  def elem: Parser[BencodeValue] = emptyString ||| string ||| int

  def emptyString: Parser[BencodeValue] = "0:" ^^ (_ => BencodeStringValue(""))

  def string: Parser[BencodeValue] = ("""[1-9]\d*""".r <~ ":") >> { strLength =>
    repN(strLength.toInt, ".|\n".r) ^^ (i => BencodeStringValue(i.mkString))
  }

  def int: Parser[BencodeValue] = "i" ~> """(0|\-?[1-9]\d*)""".r <~ "e" ^^ (i => BencodeIntValue(i.toLong))

}
package storrent.bencode

import scala.util.Try
import scala.util.parsing.combinator._

object BencodeParser extends RegexParsers {

  override def skipWhitespace: Boolean = false

  def parse(source: String): Try[List[BencodeValue]] = {
    parseAll(bencode, source) match {
      case Success(matched, _) => scala.util.Success(matched)
      case Failure(msg, _) => scala.util.Failure(BencodeParseException(msg))
      case Error(msg, _) => scala.util.Failure(BencodeParseException(msg))
    }
  }

  def bencode: Parser[List[BencodeValue]] = rep(value)

  def value: Parser[BencodeValue] = elem ||| list ||| dictionary

  def elem: Parser[BencodeValue] = emptyString ||| string ||| int

  def emptyString: Parser[BencodeValue] = "0:" ^^ (_ => BencodeString(""))

  def string: Parser[BencodeString] = ("""[1-9]\d*""".r <~ ":") >> { strLength =>
    repN(strLength.toInt, elem("any char", c => true)) ^^ (i => BencodeString(i.mkString))
  }

  def int: Parser[BencodeInt] = "i" ~> """(0|-?[1-9]\d*)""".r <~ "e" ^^ (i => BencodeInt(i.toLong))

  def list: Parser[BencodeList] = "l" ~> rep(value) <~ "e" ^^ (i => BencodeList(i))

  def dictionary: Parser[BencodeDict] = "d" ~> rep(string ~ value) <~ "e" ^^ (i =>
    BencodeDict(i.map(x => (BencodeString(x._1.value), x._2)).toMap))
}

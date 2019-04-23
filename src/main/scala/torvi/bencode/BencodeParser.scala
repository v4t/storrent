package torvi.bencode

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
    repN(strLength.toInt, elem("any char", c => true)) ^^ (i => BencodeStringValue(i.mkString))
  }

  def int: Parser[BencodeIntValue] = "i" ~> """(0|\-?[1-9]\d*)""".r <~ "e" ^^ (i => BencodeIntValue(i.toLong))

  def list: Parser[BencodeListValue] = "l" ~> rep(value) <~ "e" ^^ (i => BencodeListValue(i))

  def dictionary: Parser[BencodeDictValue] = "d" ~> rep(string ~ value) <~ "e" ^^ (i =>
    BencodeDictValue(i.map(x => (BencodeStringValue(x._1.value), x._2)).toMap))
}



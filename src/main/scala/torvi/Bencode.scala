package torvi

case class BencodeParseException(str: String) extends Exception

trait BencodeValue

case class BencodeStringValue(value: String) extends BencodeValue

object Bencode {

  def decode(stream: Stream[Char]) : List[BencodeValue] = decodeRec(stream, Nil).reverse

  private def decodeRec(stream: Stream[Char], cur: List[BencodeValue]) : List[BencodeValue] = {
    if(stream.isEmpty) cur
    else stream.head match {
      case c if c.isDigit =>
        val strLength = stream.takeWhile(i => ':' != i).mkString
        val (str, next) = stream.drop(strLength.length + 1).splitAt(strLength.toInt)
        decodeRec(next, BencodeStringValue(str.mkString) :: cur)
      case _ => throw BencodeParseException("Could not parse identifier: " + stream.head)
    }
  }

  def encode(values: List[BencodeValue]): Unit = ???
}

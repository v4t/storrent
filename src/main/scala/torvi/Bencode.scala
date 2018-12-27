package torvi

trait BencodeValue

case class BencodeStringValue(value: String) extends BencodeValue

object Bencode {

  def decode(stream: Stream[Char]) : List[BencodeValue] = ???

  def encode(values: List[BencodeValue]): Unit = ???
}

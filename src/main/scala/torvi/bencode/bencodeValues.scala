package torvi.bencode

trait BencodeValue

case class BencodeString(value: String) extends BencodeValue

case class BencodeInt(value: Long) extends BencodeValue

case class BencodeList(values: List[BencodeValue]) extends BencodeValue

case class BencodeDict(dict: Map[BencodeString, BencodeValue]) extends BencodeValue

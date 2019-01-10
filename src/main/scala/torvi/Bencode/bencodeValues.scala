package torvi.Bencode

trait BencodeValue

case class BencodeStringValue(value: String) extends BencodeValue

case class BencodeIntValue(value: Long) extends BencodeValue

case class BencodeListValue(values: List[BencodeValue]) extends BencodeValue

case class BencodeDictValue(values: Map[BencodeStringValue, BencodeValue]) extends BencodeValue
package torvi.Bencode

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
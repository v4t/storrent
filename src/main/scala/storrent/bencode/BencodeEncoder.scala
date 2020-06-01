package storrent.bencode

object BencodeEncoder {

  def encode(values: List[BencodeValue]): String = encodeValues(values, "")

  private def encodeValues(values: List[BencodeValue], result: String): String = {
    if (values == Nil) return result
    val head = values.head
    val tail = values.tail
    head match {
      case BencodeString(str) => encodeValues(tail, result + (str.length + ":" + str))
      case BencodeInt(int) => encodeValues(tail, result + ("i" + int + "e"))
      case BencodeList(list) => encodeValues(tail, result + ("l" + encodeValues(list, "") + "e"))
      case BencodeDict(dict) => encodeValues(tail, result + ("d" + encodeDict(dict) + "e"))
    }
  }

  private def encodeDict(dict: Map[BencodeString, BencodeValue]): String = {
    val keys = dict.keys.toSeq.sortBy(_.value)
    val contents = for {
      key <- keys
    } yield encodeValues(List(key), "") + encodeValues(List(dict(key)), "")
    contents.mkString("")
  }

}

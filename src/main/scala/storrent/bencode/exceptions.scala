package storrent.bencode

case class BencodeParseException(str: String) extends Exception

case class BencodeWriteException(str: String) extends Exception
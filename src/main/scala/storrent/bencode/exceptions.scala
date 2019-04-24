package storrent.bencode

final case class BencodeParseException(message: String) extends Exception(message)

final case class BencodeWriteException(message: String) extends Exception(message)

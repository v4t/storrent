package storrent.torrent

import storrent.bencode.{BencodeInt, BencodeString, BencodeValue}

case class InfoDictionary(
  pieceLength: Int,
  pieces: String,
  noExternalPeerSource: Option[Int],
  name: String,
  files: List[FileInfo]
)

object InfoDictionary {

  def fromBencode(bencodeDict: Map[BencodeString, BencodeValue]): InfoDictionary =
    InfoDictionary(
      pieceLength = pieceLength(bencodeDict),
      pieces = pieces(bencodeDict),
      noExternalPeerSource = noExternalPeerSource(bencodeDict),
      name = name(bencodeDict),
      files = FileInfo.fromBencode(bencodeDict)
    )

  private def pieceLength(dict: Map[BencodeString, BencodeValue]): Int =
    dict.get(BencodeString("piece length")) match {
      case Some(BencodeInt(value)) => value.toInt
      case Some(_) => throw MetaInfoException("Field 'piece length' expects an integer value")
      case None => throw MetaInfoException("Field 'piece length' is required")
    }

  private def pieces(dict: Map[BencodeString, BencodeValue]): String =
    dict.get(BencodeString("pieces")) match {
      case Some(BencodeString(value)) => value
      case Some(_) => throw MetaInfoException("Field 'pieces' expects a string value")
      case None => throw MetaInfoException("Field 'pieces' is required")
    }

  private def noExternalPeerSource(dict: Map[BencodeString, BencodeValue]): Option[Int] =
    dict.get(BencodeString("private")) match {
      case Some(BencodeInt(value)) => Some(value.toInt)
      case _ => None
    }

  private def name(dict: Map[BencodeString, BencodeValue]): String =
    dict.get(BencodeString("name")) match {
      case Some(BencodeString(value)) => value
      case Some(_) => throw MetaInfoException("Field 'name' expects a string value")
      case None => throw MetaInfoException("Field 'name' is required")
    }

}

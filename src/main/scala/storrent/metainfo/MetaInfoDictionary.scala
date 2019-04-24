package storrent.metainfo

import storrent.bencode.{BencodeInt, BencodeString, BencodeValue}

case class MetaInfoDictionary(
  pieceLength: Long,
  pieces: String,
  noExternalPeerSource: Option[Long],
  name: String,
  files: List[FileInfo]
)

object MetaInfoDictionary {
  def fromBencode(bencodeDict: Map[BencodeString, BencodeValue]): MetaInfoDictionary =
    MetaInfoDictionary(
      pieceLength = pieceLength(bencodeDict),
      pieces = pieces(bencodeDict),
      noExternalPeerSource = noExternalPeerSource(bencodeDict),
      name = name(bencodeDict),
      files = FileInfo.fromBencode(bencodeDict)
    )

  private def pieceLength(dict: Map[BencodeString, BencodeValue]): Long =
    dict.get(BencodeString("piece length")) match {
      case Some(BencodeInt(value)) => value
      case Some(_) => throw MetaInfoException("Piece length field must be an integer")
      case None => throw MetaInfoException("Piece length field is required")
    }

  private def pieces(dict: Map[BencodeString, BencodeValue]): String =
    dict.get(BencodeString("pieces")) match {
      case Some(BencodeString(value)) => value
      case Some(_) => throw MetaInfoException("Pieces field must be a string")
      case None => throw MetaInfoException("Pieces field is required")
    }

  private def noExternalPeerSource(dict: Map[BencodeString, BencodeValue]): Option[Long] =
    dict.get(BencodeString("private")) match {
      case Some(BencodeInt(value)) => Some(value)
      case _ => None
    }

  private def name(dict: Map[BencodeString, BencodeValue]): String =
    dict.get(BencodeString("name")) match {
      case Some(BencodeString(value)) => value
      case Some(_) => throw MetaInfoException("Name field must be a string")
      case None => throw MetaInfoException("Name field is required")
    }

}
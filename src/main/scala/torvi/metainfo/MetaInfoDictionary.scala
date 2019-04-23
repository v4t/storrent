package torvi.metainfo

import torvi.bencode.{BencodeInt, BencodeString, BencodeValue}

case class MetaInfoDictionary(
  pieceLength: Long,
  pieces: String,
  noExternalPeerSource: Option[Long],
  name: String,
  files: List[FileInfo]
)

object MetaInfoDictionary {
  def fromBencode(bencodeDict: Map[BencodeString, BencodeValue]): MetaInfoDictionary = {
    val pieceLength = bencodeDict.get(BencodeString("piece length")) match {
      case Some(BencodeInt(value)) => value
    }
    val pieces = bencodeDict.get(BencodeString("pieces")) match {
      case Some(BencodeString(value)) => value
    }
    val noExternalPeerSource = bencodeDict.get(BencodeString("private")) match {
      case Some(BencodeInt(value)) => Some(value)
      case _ => None
    }
    val name = bencodeDict.get(BencodeString("name")) match {
      case Some(BencodeString(value)) => value
    }
    val files = FileInfo.fromBencode(bencodeDict)

    MetaInfoDictionary(
      pieceLength = pieceLength,
      pieces = pieces,
      noExternalPeerSource = noExternalPeerSource,
      name = name,
      files = files
    )
  }
}
package torvi.metainfo

import torvi.bencode.{BencodeIntValue, BencodeStringValue, BencodeValue}

case class MetaInfoDictionary(
  pieceLength: Long,
  pieces: String,
  noExternalPeerSource: Option[Long],
  name: String,
  files: List[FileInfo]
)

object MetaInfoDictionary {
  def fromBencode(bencodeDict: Map[BencodeStringValue, BencodeValue]): MetaInfoDictionary = {
    val pieceLength = bencodeDict.get(BencodeStringValue("piece length")) match {
      case Some(BencodeIntValue(value)) => value
    }
    val pieces = bencodeDict.get(BencodeStringValue("pieces")) match {
      case Some(BencodeStringValue(value)) => "pieces here"
    }
    val noExternalPeerSource = bencodeDict.get(BencodeStringValue("private")) match {
      case Some(BencodeIntValue(value)) => Some(value)
      case _ => None
    }
    val name = bencodeDict.get(BencodeStringValue("name")) match {
      case Some(BencodeStringValue(value)) => value
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
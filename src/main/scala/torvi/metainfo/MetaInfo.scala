package torvi.metainfo

import torvi.bencode.{BencodeDict, BencodeInt, BencodeList, BencodeString, BencodeValue}

case class MetaInfo(
  info: MetaInfoDictionary,
  announceList: Set[String],
  creationDate: Option[Long],
  comment: Option[String],
  createdBy: Option[String],
  encoding: Option[String]
)

object MetaInfo {
  def fromBencode(bencodeValues: List[BencodeValue]): Option[MetaInfo] = {
    if (bencodeValues.size != 1) return None
    val metafile = bencodeValues.head

    metafile match {
      case BencodeDict(map) =>
        val infoMap = map(BencodeString("info")) match {
          case BencodeDict(im) => im
        }

        Some(MetaInfo(
          info = MetaInfoDictionary.fromBencode(infoMap),
          announceList = metainfoAnnounce(map),
          creationDate = metainfoCreationDate(map),
          comment = metainfoComment(map),
          createdBy = metainfoCreatedBy(map),
          encoding = metainfoEncoding(map)
        ))
      case _ => None
    }
  }

  private def metainfoAnnounce(bencodeDict: Map[BencodeString, BencodeValue]): Set[String] = {
    val announce = bencodeDict.get(BencodeString("announce"))
    val announceList = bencodeDict.get(BencodeString("announce-list"))
    if (announce.isEmpty && announceList.isEmpty)
      throw MetaInfoException("Either announce or announce-list have to be specified")

    if (announce.isDefined) announce match {
      case Some(BencodeString(url)) => return Set(url)
      case _ => throw MetaInfoException("Invalid format in metainfo file: announce should be a string.")
    }

    val urls = announceList.get match {
      case BencodeList(c) => c map {
        case BencodeString(u) => u
        case _ => throw MetaInfoException("Invalid format in metainfo file: announce-list should contain only strings.")
      }
      case _ => throw MetaInfoException("Invalid format in metainfo file: announce-list should be a list.")
    }
    urls.foldLeft(Set[String]()) { (set, i) => set + i }
  }

  private def metainfoCreationDate(bencodeDict: Map[BencodeString, BencodeValue]): Option[Long] =
    bencodeDict.get(BencodeString("creation date")) match {
      case Some(BencodeInt(date)) => Some(date)
      case _ => None
    }

  private def metainfoCreatedBy(bencodeDict: Map[BencodeString, BencodeValue]): Option[String] =
    bencodeDict.get(BencodeString("created by")) match {
      case Some(BencodeString(s)) => Some(s)
      case _ => None
    }

  private def metainfoComment(bencodeDict: Map[BencodeString, BencodeValue]): Option[String] =
    bencodeDict.get(BencodeString("comment")) match {
      case Some(BencodeString(s)) => Some(s)
      case _ => None
    }

  private def metainfoEncoding(bencodeDict: Map[BencodeString, BencodeValue]): Option[String] =
    bencodeDict.get(BencodeString("encoding")) match {
      case Some(BencodeString(s)) => Some(s)
      case _ => None
    }

}

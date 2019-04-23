package torvi.metainfo

import torvi.bencode.{BencodeDictValue, BencodeIntValue, BencodeListValue, BencodeStringValue, BencodeValue}

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
      case BencodeDictValue(map) =>
        val infoMap = map(BencodeStringValue("info")) match {
          case BencodeDictValue(im) => im
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

  private def metainfoAnnounce(bencodeDict: Map[BencodeStringValue, BencodeValue]): Set[String] = {
    val announce = bencodeDict.get(BencodeStringValue("announce"))
    val announceList = bencodeDict.get(BencodeStringValue("announce-list"))
    if (announce.isEmpty && announceList.isEmpty)
      throw MetaInfoException("Either announce or announce-list have to be specified")

    if (announce.isDefined) announce match {
      case Some(BencodeStringValue(url)) => return Set(url)
      case _ => throw MetaInfoException("Invalid format in metainfo file: announce should be a string.")
    }

    val urls = announceList.get match {
      case BencodeListValue(c) => c map {
        case BencodeStringValue(u) => u
        case _ => throw MetaInfoException("Invalid format in metainfo file: announce-list should contain only strings.")
      }
      case _ => throw MetaInfoException("Invalid format in metainfo file: announce-list should be a list.")
    }
    urls.foldLeft(Set[String]()) { (set, i) => set + i }
  }

  private def metainfoCreationDate(bencodeDict: Map[BencodeStringValue, BencodeValue]): Option[Long] =
    bencodeDict.get(BencodeStringValue("creation date")) match {
      case Some(BencodeIntValue(date)) => Some(date)
      case _ => None
    }

  private def metainfoCreatedBy(bencodeDict: Map[BencodeStringValue, BencodeValue]): Option[String] =
    bencodeDict.get(BencodeStringValue("created by")) match {
      case Some(BencodeStringValue(s)) => Some(s)
      case _ => None
    }

  private def metainfoComment(bencodeDict: Map[BencodeStringValue, BencodeValue]): Option[String] =
    bencodeDict.get(BencodeStringValue("comment")) match {
      case Some(BencodeStringValue(s)) => Some(s)
      case _ => None
    }

  private def metainfoEncoding(bencodeDict: Map[BencodeStringValue, BencodeValue]): Option[String] =
    bencodeDict.get(BencodeStringValue("encoding")) match {
      case Some(BencodeStringValue(s)) => Some(s)
      case _ => None
    }

}

package torvi.metainfo

import torvi.bencode.{BencodeDict, BencodeInt, BencodeList, BencodeParser, BencodeString, BencodeValue}
import scala.io.{Codec, Source}

case class MetaInfo(
  info: MetaInfoDictionary,
  announceList: Set[String],
  creationDate: Option[Long],
  comment: Option[String],
  createdBy: Option[String],
  encoding: Option[String]
)

object MetaInfo {
  def fromFile(fileName: String): Option[MetaInfo] = {
    val source = Source.fromFile(fileName)(Codec.ISO8859)
    val contents = source.mkString
    source.close
    val bencodeValues = BencodeParser.parse(contents)
    fromBencode(bencodeValues)
  }

  def fromBencode(bencodeValues: List[BencodeValue]): Option[MetaInfo] = {
    if (bencodeValues.size != 1) return None
    bencodeValues.head match {
      case BencodeDict(map) =>
        val infoMap = map(BencodeString("info")) match {
          case BencodeDict(im) => im
        }
        Some(MetaInfo(
          info = MetaInfoDictionary.fromBencode(infoMap),
          announceList = metaInfoAnnounce(map),
          creationDate = metaInfoCreationDate(map),
          comment = metainfoComment(map),
          createdBy = metaInfoCreatedBy(map),
          encoding = metaInfoEncoding(map)
        ))
      case _ => None
    }
  }

  private def metaInfoAnnounce(bencodeDict: Map[BencodeString, BencodeValue]): Set[String] = {
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

  private def metaInfoCreationDate(bencodeDict: Map[BencodeString, BencodeValue]): Option[Long] =
    bencodeDict.get(BencodeString("creation date")) match {
      case Some(BencodeInt(date)) => Some(date)
      case _ => None
    }

  private def metaInfoCreatedBy(bencodeDict: Map[BencodeString, BencodeValue]): Option[String] =
    bencodeDict.get(BencodeString("created by")) match {
      case Some(BencodeString(s)) => Some(s)
      case _ => None
    }

  private def metainfoComment(bencodeDict: Map[BencodeString, BencodeValue]): Option[String] =
    bencodeDict.get(BencodeString("comment")) match {
      case Some(BencodeString(s)) => Some(s)
      case _ => None
    }

  private def metaInfoEncoding(bencodeDict: Map[BencodeString, BencodeValue]): Option[String] =
    bencodeDict.get(BencodeString("encoding")) match {
      case Some(BencodeString(s)) => Some(s)
      case _ => None
    }

}

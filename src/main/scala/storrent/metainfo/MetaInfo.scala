package storrent.metainfo

import storrent.bencode._

import scala.util.Try

case class MetaInfo(
  info: MetaInfoDictionary,
  announceList: Set[String],
  creationDate: Option[Long],
  comment: Option[String],
  createdBy: Option[String],
  encoding: Option[String]
)

object MetaInfo {

  def fromBencode(bencodeValues: List[BencodeValue]): Try[MetaInfo] = Try(parseMetaInfo(bencodeValues))

  private def parseMetaInfo(bencodeValues: List[BencodeValue]): MetaInfo = {
    if (bencodeValues.size != 1) throw MetaInfoException("Invalid bencode values for metainfo file")
    val dict = bencodeValues.head match {
      case BencodeDict(map) => map
      case _ => throw MetaInfoException("Metainfo file should contain only one dictionary value")
    }
    val infoDict = dict.get(BencodeString("info")) match {
      case Some(BencodeDict(map)) => map
      case Some(_) => throw MetaInfoException("Field 'info' should be a dictionary")
      case None => throw MetaInfoException("Field 'info' is required")
    }
    MetaInfo(
      info = MetaInfoDictionary.fromBencode(infoDict),
      announceList = announce(dict),
      creationDate = creationDate(dict),
      comment = comment(dict),
      createdBy = createdBy(dict),
      encoding = encoding(dict)
    )
  }

  private def announce(dict: Map[BencodeString, BencodeValue]): Set[String] =
    if (dict.contains(BencodeString("announce"))) announceSingle(dict)
    else if (dict.contains(BencodeString("announce-list"))) announceList(dict)
    else throw MetaInfoException("Either 'announce' or 'announce-list' have to be specified")


  private def announceSingle(dict: Map[BencodeString, BencodeValue]): Set[String] =
    dict(BencodeString("announce")) match {
      case BencodeString(url) => Set(url)
      case _ => throw MetaInfoException("Field 'announce' expects a string value.")
    }

  private def announceList(dict: Map[BencodeString, BencodeValue]): Set[String] = {
    val urls = dict(BencodeString("announce-list")) match {
      case BencodeList(c) => c map {
        case BencodeString(u) => u
        case _ => throw MetaInfoException("Field 'announce-list' should contain only strings values.")
      }
      case _ => throw MetaInfoException("Field 'announce-list' expects a list value.")
    }
    urls.foldLeft(Set[String]()) { (set, i) => set + i }
  }

  private def creationDate(dict: Map[BencodeString, BencodeValue]): Option[Long] =
    dict.get(BencodeString("creation date")) match {
      case Some(BencodeInt(date)) => Some(date)
      case _ => None
    }

  private def createdBy(dict: Map[BencodeString, BencodeValue]): Option[String] =
    dict.get(BencodeString("created by")) match {
      case Some(BencodeString(s)) => Some(s)
      case _ => None
    }

  private def comment(dict: Map[BencodeString, BencodeValue]): Option[String] =
    dict.get(BencodeString("comment")) match {
      case Some(BencodeString(s)) => Some(s)
      case _ => None
    }

  private def encoding(dict: Map[BencodeString, BencodeValue]): Option[String] =
    dict.get(BencodeString("encoding")) match {
      case Some(BencodeString(s)) => Some(s)
      case _ => None
    }

}


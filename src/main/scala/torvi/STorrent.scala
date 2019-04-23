package torvi

import akka.actor.Actor
import torvi.Bencode.{BencodeDictValue, BencodeIntValue, BencodeListValue, BencodeParser, BencodeStringValue, BencodeValue}

import scala.io.{Codec, Source}
import scala.util.{Failure, Try}

case class StartDownload(torrentFile: String)


case class MetaInfo(info: String,
                    announceList: Set[String],
                    creationDate: Option[Long],
                    comment: Option[String],
                    createdBy: Option[String],
                    encoding: Option[String])


class STorrent extends Actor {
  def receive = {
    case StartDownload(torrentFile) => {
      println(torrentFile)
      val source = Source.fromFile(torrentFile)(Codec.ISO8859)
      val contents = source.mkString
      source.close
      val bencodeValues = BencodeParser.parse(contents)

      val metaInfo = metainfo(bencodeValues)
      metaInfo match {
        case Some(x) => println(x)
        case None => println("No metainfo :(")
      }
    }
  }

  case class MetaInfoException(msg: String) extends Exception

  def metainfo(bencodeValues: List[BencodeValue]): Option[MetaInfo] = {
    if (bencodeValues.size != 1) return None
    val metafile = bencodeValues.head
    metafile match {
      case BencodeDictValue(map) => Some(MetaInfo(
        info = "foo",
        announceList = metainfoAnnounce(map),
        creationDate = metainfoCreationDate(map),
        comment = metainfoComment(map),
        createdBy = metainfoCreatedBy(map),
        encoding = metainfoEncoding(map)
      ))
      case _ => None
    }
  }

  def metainfoAnnounce(bencodeDict: Map[BencodeStringValue, BencodeValue]): Set[String] = {
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

  def metainfoCreationDate(bencodeDict: Map[BencodeStringValue, BencodeValue]): Option[Long] =
    bencodeDict.get(BencodeStringValue("creation date")) match {
      case Some(BencodeIntValue(date)) => Some(date)
      case _ => None
    }

  def metainfoCreatedBy(bencodeDict: Map[BencodeStringValue, BencodeValue]): Option[String] =
    bencodeDict.get(BencodeStringValue("created by")) match {
      case Some(BencodeStringValue(s)) => Some(s)
      case _ => None
    }

  def metainfoComment(bencodeDict: Map[BencodeStringValue, BencodeValue]): Option[String] =
    bencodeDict.get(BencodeStringValue("comment")) match {
      case Some(BencodeStringValue(s)) => Some(s)
      case _ => None
    }

  def metainfoEncoding(bencodeDict: Map[BencodeStringValue, BencodeValue]): Option[String] =
    bencodeDict.get(BencodeStringValue("encoding")) match {
      case Some(BencodeStringValue(s)) => Some(s)
      case _ => None
    }

}

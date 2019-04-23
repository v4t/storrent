package torvi

import akka.actor.Actor
import torvi.Bencode.{BencodeDictValue, BencodeIntValue, BencodeListValue, BencodeParser, BencodeStringValue, BencodeValue}

import scala.io.{Codec, Source}
import scala.util.{Failure, Try}

case class StartDownload(torrentFile: String)


case class MetaInfo(
  info: MetaInfoDictionary,
  announceList: Set[String],
  creationDate: Option[Long],
  comment: Option[String],
  createdBy: Option[String],
  encoding: Option[String]
)

case class MetaInfoDictionary(
  pieceLength: Long,
  pieces: String,
  noExternalPeerSource: Option[Long],
  name: String,
  files: List[MetaInfoFile]
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
    val files = MetaInfoFile.fromBencode(bencodeDict)

    MetaInfoDictionary(
      pieceLength = pieceLength,
      pieces = pieces,
      noExternalPeerSource = noExternalPeerSource,
      name = name,
      files = files
    )
  }
}

case class MetaInfoFile(
  path: List[String],
  length: Long,
  md5Sum: Option[String]
)

object MetaInfoFile {
  def fromBencode(bencodeDict: Map[BencodeStringValue, BencodeValue]): List[MetaInfoFile] = {
    if (bencodeDict.isDefinedAt(BencodeStringValue("files")))
      multipleFiles(bencodeDict)
    else
      singleFile(bencodeDict)
  }


  private def multipleFiles(bencodeDict: Map[BencodeStringValue, BencodeValue]): List[MetaInfoFile] = {
    val fileDicts = bencodeDict(BencodeStringValue("files")) match {
      case BencodeListValue(f) => f
    }

    val fileLs = fileDicts.map {
      case BencodeDictValue(map) => {
        val path = map.get(BencodeStringValue("path")) match {
          case Some(BencodeListValue(vs)) => vs map { case BencodeStringValue(x) => x }
        }
        val md5Sum = map.get(BencodeStringValue("md5sum")) match {
          case Some(BencodeStringValue(value)) => Some(value)
          case _ => None
        }
        val length = map.get(BencodeStringValue("length")) match {
          case Some(BencodeIntValue(value)) => value
        }
        MetaInfoFile(path, length, md5Sum)
      }
    }
    fileLs
  }

  private def singleFile(bencodeDict: Map[BencodeStringValue, BencodeValue]): List[MetaInfoFile] = {
    val path = bencodeDict.get(BencodeStringValue("name")) match {
      case Some(BencodeStringValue(value)) => List(value)
    }
    val length = bencodeDict.get(BencodeStringValue("length")) match {
      case Some(BencodeIntValue(value)) => value
    }
    val md5Sum = bencodeDict.get(BencodeStringValue("md5sum")) match {
      case Some(BencodeStringValue(value)) => Some(value)
      case _ => None
    }
    List(MetaInfoFile(path = path, length = length, md5Sum = md5Sum))
  }
}

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
      case BencodeDictValue(map) =>
        val infoMap = map(BencodeStringValue("info")) match { case BencodeDictValue(im) => im}

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

package torvi.metainfo

import torvi.bencode.{BencodeDictValue, BencodeIntValue, BencodeListValue, BencodeStringValue, BencodeValue}

case class FileInfo(
  path: List[String],
  length: Long,
  md5Sum: Option[String]
)

object FileInfo {
  def fromBencode(bencodeDict: Map[BencodeStringValue, BencodeValue]): List[FileInfo] = {
    if (bencodeDict.isDefinedAt(BencodeStringValue("files")))
      multipleFiles(bencodeDict)
    else
      singleFile(bencodeDict)
  }

  private def multipleFiles(bencodeDict: Map[BencodeStringValue, BencodeValue]): List[FileInfo] = {
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
        FileInfo(path, length, md5Sum)
      }
    }
    fileLs
  }

  private def singleFile(bencodeDict: Map[BencodeStringValue, BencodeValue]): List[FileInfo] = {
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
    List(FileInfo(path = path, length = length, md5Sum = md5Sum))
  }
}
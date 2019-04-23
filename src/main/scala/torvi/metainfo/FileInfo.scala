package torvi.metainfo

import torvi.bencode.{BencodeDict, BencodeInt, BencodeList, BencodeString, BencodeValue}

case class FileInfo(
  path: List[String],
  length: Long,
  md5Sum: Option[String]
)

object FileInfo {
  def fromBencode(bencodeDict: Map[BencodeString, BencodeValue]): List[FileInfo] = {
    if (bencodeDict.isDefinedAt(BencodeString("files")))
      multipleFiles(bencodeDict)
    else
      singleFile(bencodeDict)
  }

  private def multipleFiles(bencodeDict: Map[BencodeString, BencodeValue]): List[FileInfo] = {
    val fileDicts = bencodeDict(BencodeString("files")) match {
      case BencodeList(f) => f
    }

    val fileLs = fileDicts.map {
      case BencodeDict(map) => {
        val path = map.get(BencodeString("path")) match {
          case Some(BencodeList(vs)) => vs map { case BencodeString(x) => x }
        }
        val md5Sum = map.get(BencodeString("md5sum")) match {
          case Some(BencodeString(value)) => Some(value)
          case _ => None
        }
        val length = map.get(BencodeString("length")) match {
          case Some(BencodeInt(value)) => value
        }
        FileInfo(path, length, md5Sum)
      }
    }
    fileLs
  }

  private def singleFile(bencodeDict: Map[BencodeString, BencodeValue]): List[FileInfo] = {
    val path = bencodeDict.get(BencodeString("name")) match {
      case Some(BencodeString(value)) => List(value)
    }
    val length = bencodeDict.get(BencodeString("length")) match {
      case Some(BencodeInt(value)) => value
    }
    val md5Sum = bencodeDict.get(BencodeString("md5sum")) match {
      case Some(BencodeString(value)) => Some(value)
      case _ => None
    }
    List(FileInfo(path = path, length = length, md5Sum = md5Sum))
  }
}
package storrent.metainfo

import storrent.bencode.{BencodeDict, BencodeInt, BencodeList, BencodeString, BencodeValue}

case class FileInfo(
  path: List[String],
  length: Long,
  md5Sum: Option[String]
)

object FileInfo {
  def fromBencode(infoDict: Map[BencodeString, BencodeValue]): List[FileInfo] = {
    if (infoDict.isDefinedAt(BencodeString("files")))
      multipleFiles(infoDict)
    else
      singleFile(infoDict)
  }

  private def singleFile(infoDict: Map[BencodeString, BencodeValue]): List[FileInfo] =
    List(FileInfo(path = singleFilePath(infoDict), length = fileLength(infoDict), md5Sum = fileMD5Sum(infoDict)))

  private def multipleFiles(infoDict: Map[BencodeString, BencodeValue]): List[FileInfo] = {
    val files = infoDict(BencodeString("files")) match {
      case BencodeList(f) => f
      case _ => throw MetaInfoException("Files field must be a list")
    }
    files.map {
      case BencodeDict(map) => FileInfo(path = multiFilePath(map), length = fileLength(map), md5Sum = fileMD5Sum(map))
      case _ => throw MetaInfoException("Files list elements must only be dictionaries")
    }
  }

  private def singleFilePath(dict: Map[BencodeString, BencodeValue]): List[String] =
    dict.get(BencodeString("name")) match {
      case Some(BencodeString(value)) => List(value)
      case Some(_) => throw MetaInfoException("Single file name field must be a string")
      case None => throw MetaInfoException("Missing name field in single file info")
    }

  private def multiFilePath(dict: Map[BencodeString, BencodeValue]): List[String] =
    dict.get(BencodeString("path")) match {
      case Some(BencodeList(parts)) => parts map {
        case BencodeString(x) => x
        case _ => throw MetaInfoException("Path for file in multiple files info must contain only strings")
      }
      case Some(_) => throw MetaInfoException("Path for file in multiple files info must be a list")
      case None => throw MetaInfoException("Missing path for file in multiple files info")
    }

  private def fileLength(dict: Map[BencodeString, BencodeValue]): Long =
    dict.get(BencodeString("length")) match {
      case Some(BencodeInt(value)) => value
      case Some(_) => throw MetaInfoException("File length field must be an integer")
      case None => throw MetaInfoException("Missing length field in file info")
    }

  private def fileMD5Sum(dict: Map[BencodeString, BencodeValue]): Option[String] =
    dict.get(BencodeString("md5sum")) match {
      case Some(BencodeString(value)) => Some(value)
      case _ => None
    }
}
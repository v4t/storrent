package storrent.metainfo

import storrent.bencode._

case class FileInfo(
  path: List[String],
  length: Long,
  md5Sum: Option[String],
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
      case _ => throw MetaInfoException("Field 'files' expects a list value")
    }
    files.map {
      case BencodeDict(map) => FileInfo(path = multiFilePath(map), length = fileLength(map), md5Sum = fileMD5Sum(map))
      case _ => throw MetaInfoException("Field 'files' should contain only dictionary values")
    }
  }

  private def singleFilePath(dict: Map[BencodeString, BencodeValue]): List[String] =
    dict.get(BencodeString("name")) match {
      case Some(BencodeString(value)) => List(value)
      case Some(_) => throw MetaInfoException("Field 'name' expects a string value")
      case None => throw MetaInfoException("Field 'name' is required for a single file info")
    }

  private def multiFilePath(dict: Map[BencodeString, BencodeValue]): List[String] =
    dict.get(BencodeString("path")) match {
      case Some(BencodeList(parts)) => parts map {
        case BencodeString(x) => x
        case _ => throw MetaInfoException("Field 'path' should contain only string values")
      }
      case Some(_) => throw MetaInfoException("Field 'path' expects a list value")
      case None => throw MetaInfoException("Field 'path' is required for all files in multiple files info")
    }

  private def fileLength(dict: Map[BencodeString, BencodeValue]): Long =
    dict.get(BencodeString("length")) match {
      case Some(BencodeInt(value)) => value
      case Some(_) => throw MetaInfoException("Field 'length' expects an integer value")
      case None => throw MetaInfoException("Field 'length' is required")
    }

  private def fileMD5Sum(dict: Map[BencodeString, BencodeValue]): Option[String] =
    dict.get(BencodeString("md5sum")) match {
      case Some(BencodeString(value)) => Some(value)
      case _ => None
    }

}

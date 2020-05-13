package storrent.metainfo

import java.nio.charset.StandardCharsets

import storrent.bencode.{BencodeParser, BencodeValue}

import scala.io.{Codec, Source}
import scala.util.{Failure, Success, Try}

class Torrent(val metaInfo: MetaInfo) {
  private val blockSize = 16384

  private lazy val verificationHashes: Array[String] = metaInfo.info.pieces.grouped(20).toArray

  val totalLength: Long = metaInfo.info.files.foldLeft(0.toLong)((acc, f) => acc + f.length)

  val pieceCount: Int = Math.ceil(metaInfo.info.pieces.length / 20.0).toInt

  def blockCount(piece: Int): Int = Math.ceil(pieceSize(piece) / blockSize.toFloat).toInt

  def pieceSize(piece: Int): Long = {
    if (piece == pieceCount - 1) {
      val remainder = totalLength % metaInfo.info.pieceLength
      if (remainder != 0) remainder else metaInfo.info.pieceLength
    } else {
      metaInfo.info.pieceLength
    }
  }

  def blockSize(piece: Int, block: Int): Long = {
    if (block == blockCount(piece) - 1) {
      val remainder = pieceSize(piece) % blockSize
      if (remainder != 0) remainder else blockSize
    } else {
      blockSize
    }
  }

  def pieceVerificationHash(piece: Int): Array[Byte] = verificationHashes(piece).getBytes(StandardCharsets.ISO_8859_1)

}

object Torrent {

  def fromFile(file: String): Option[Torrent] = {
    val name = file.split('/').last.replace(".torrent", "")
    val bencodeValues = parseSource(file).get
    MetaInfo.fromBencode(bencodeValues) match {
      case Success(metaInfo) => Some(new Torrent(metaInfo))
      case Failure(_) => None
    }
  }

  private def parseSource(filePath: String): Try[List[BencodeValue]] = {
    lazy val source = Source.fromFile(filePath)(Codec.ISO8859)
    try {
      BencodeParser.parse(source.mkString)
    }
    catch {
      case e: Exception => Failure(e)
    }
    finally {
      source.close
    }
  }
}
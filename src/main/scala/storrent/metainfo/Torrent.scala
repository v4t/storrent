package storrent.metainfo

import java.nio.charset.StandardCharsets

import storrent.bencode.{BencodeParser, BencodeValue}

import scala.io.{Codec, Source}
import scala.util.{Failure, Success, Try}

class Torrent(val metaInfo: MetaInfo) {

  private val verificationHashes: Array[String] = metaInfo.info.pieces.grouped(20).toArray

  private val xx: Map[Int, List[String]] = {

    Map[Int, List[String]]()
  }

  val defaultPieceSize: Int = metaInfo.info.pieceLength

  val defaultBlockSize: Int = 16384

  val totalLength: Long = metaInfo.info.files.foldLeft(0.toLong)((acc, f) => acc + f.length)

  val pieceCount: Int = Math.ceil(metaInfo.info.pieces.length / 20.0).toInt

  def blockCount(piece: Int): Int = Math.ceil(pieceSize(piece) / defaultBlockSize.toFloat).toInt

  def pieceSize(piece: Int): Int = {
    if (piece == pieceCount - 1) {
      val remainder = totalLength % defaultPieceSize
      if (remainder != 0) remainder.toInt else defaultPieceSize
    } else {
      defaultPieceSize
    }
  }

  def blockSize(piece: Int, block: Int): Int = {
    if (block == blockCount(piece) - 1) {
      val remainder = pieceSize(piece) % defaultBlockSize
      if (remainder != 0) remainder else defaultBlockSize
    } else {
      defaultBlockSize
    }
  }

  def pieceVerificationHash(piece: Int): Array[Byte] = verificationHashes(piece).getBytes(StandardCharsets.ISO_8859_1)

  def files: List[FileInfo] = metaInfo.info.files

  def filesContainingPiece(piece: Int): List[FileInfo] = {
    val pieceLength = pieceSize(piece)
    val pieceStart = defaultPieceSize * piece
    val startingPositions: List[Long] = files.map(_.length).scan(0L)(_ + _)
    for {
      (file, fileStart) <- files.zip(startingPositions)
      if ((fileStart <= pieceStart && pieceStart < fileStart + file.length) // Piece start is within file
        || (fileStart >= pieceStart && pieceStart + pieceLength > fileStart)) // Piece overlaps file
    } yield file
  }

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

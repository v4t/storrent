package storrent.torrent

import java.io.File
import java.nio.charset.StandardCharsets

import storrent.bencode.{BencodeParser, BencodeValue}

import scala.io.{Codec, Source}
import scala.util.{Failure, Success, Try}

class Torrent(private val savePath: String, val metaInfo: MetaInfo, val defaultBlockSize: Int = 16384) {

  private val verificationHashes: Array[String] = metaInfo.info.pieces.grouped(20).toArray

  val name: String = metaInfo.info.name

  val defaultPieceSize: Int = metaInfo.info.pieceLength

  val totalLength: Long = metaInfo.info.files.foldLeft(0.toLong)((acc, f) => acc + f.length)

  val pieceCount: Int = Math.ceil(metaInfo.info.pieces.length / 20.0).toInt

  val downloadPath: String =
    if (files.length == 1) savePath
    else savePath + File.separator + metaInfo.info.name

  /**
   * Return file path for given file.
   *
   * @param f File
   * @return File path
   */
  def filePath(f: FileInfo): String = downloadPath + File.separator + f.path.mkString(File.separator)

  /**
   * Calculate amount of blocks needed for given piece.
   *
   * @param piece Piece index
   * @return
   */
  def blockCount(piece: Int): Int = Math.ceil(pieceSize(piece) / defaultBlockSize.toFloat).toInt

  /**
   * Calculate piece size in bytes.
   *
   * @param piece Piece index
   * @return
   */
  def pieceSize(piece: Int): Int = {
    if (piece == pieceCount - 1) {
      val remainder = totalLength % defaultPieceSize
      if (remainder != 0) remainder.toInt else defaultPieceSize
    } else {
      defaultPieceSize
    }
  }

  /**
   * Calculate block size in bytes.
   *
   * @param piece Piece index
   * @param block Block index
   * @return
   */
  def blockSize(piece: Int, block: Int): Int = {
    if (block == blockCount(piece) - 1) {
      val remainder = pieceSize(piece) % defaultBlockSize
      if (remainder != 0) remainder else defaultBlockSize
    } else {
      defaultBlockSize
    }
  }

  /**
   * Get verification hash for piece
   *
   * @param piece Piece index
   * @return
   */
  def pieceVerificationHash(piece: Int): Array[Byte] = verificationHashes(piece).getBytes(StandardCharsets.ISO_8859_1)

  /**
   * List of files encoded in the torrent
   *
   * @return
   */
  def files: List[FileInfo] = metaInfo.info.files

}

object Torrent {

  /**
   * Construct torrent object from torrent file.
   *
   * @param file      Torrent file path
   * @param savePath  Path to download directory
   * @param blockSize Default block size for torrent
   * @return
   */
  def fromFile(file: String, savePath: String, blockSize: Int): Option[Torrent] = {
    val bencodeValues = parseSource(file).get
    MetaInfo.fromBencode(bencodeValues) match {
      case Success(metaInfo) => Some(new Torrent(savePath, metaInfo, blockSize))
      case Failure(_) => None
    }
  }

  /**
   * Parse torrent from .torrent file.
   *
   * @param filePath Path to torrent file
   * @return
   */
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

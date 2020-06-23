package storrent

import java.io.{File, RandomAccessFile}

import akka.actor.{Actor, ActorLogging, ActorRef}
import storrent.torrent.{FileInfo, Torrent}
import storrent.peerprotocol.Request
import storrent.trackerprotocol.PeerInfo

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

case class AddPeer(peer: PeerInfo)

case class RemovePeer(peer: PeerInfo)

class Downloader(client: ActorRef, torrent: Torrent, saveDir: String) extends Actor with ActorLogging {
  private val downloadPath =
    if (torrent.files.length == 1) saveDir
    else saveDir + "\\" + torrent.metaInfo.info.name

  private val messageDigest = java.security.MessageDigest.getInstance("SHA-1")

  private val peers = mutable.Set[PeerInfo]()

  private val peersWithPendingRequest = mutable.Set[PeerInfo]()

  private val failingPeers = mutable.Set[PeerInfo]()

  private val downloadedPieces: Array[Boolean] = new Array(torrent.pieceCount)

  private val requestedBlocks: mutable.Set[Int] = mutable.Set[Int]()

  private var currentPiece: Int = nextPieceIndex()

  private var currentBlocks: Array[Array[Byte]] = Array[Array[Byte]]()

  override def preStart(): Unit = {
    initializeFiles()
    super.preStart()
  }

  def receive: Receive = {
    case AddPeer(peer) =>
      peers.add(peer)
      requestBlocks()
      log.debug("Added peer " + peer.peerId)

    case RemovePeer(peer) =>
      peers.remove(peer)
      log.debug("Removed peer " + peer.peerId)

    case RequestBlockFailed(request, peer) =>
      val block = request.begin / torrent.defaultBlockSize
      log.debug("Failed to retrieve block " + block)
      failingPeers.add(peer)
      requestedBlocks.remove(block)
      requestBlocks()

    case BlockReceived(piece, peer) =>
      peersWithPendingRequest.remove(peer)
      val block = piece.begin / torrent.defaultBlockSize
      if (currentBlocks.isDefinedAt(block)) {
        currentBlocks(block) = piece.block
      }
      val blocksRemaining = currentBlocks.count(i => i == null)
      if (blocksRemaining == 0) {
        val blockAsBytes = currentBlocks.flatten
        if (validatePieceHash(currentPiece, blockAsBytes)) {
          persistPieceToDisk(blockAsBytes)
        }
        requestedBlocks.clear()
        failingPeers.clear()
      }
      requestBlocks()
  }

  private def requestBlocks(): Unit = {
    if (currentPiece < 0 || downloadComplete()) {
      log.info("Download complete")
      return
    }
    if (downloadedPieces(currentPiece)) {
      currentPiece = nextPieceIndex()
      if (currentPiece < 0) return

      val blockCount = torrent.blockCount(currentPiece)
      currentBlocks = new Array[Array[Byte]](blockCount)
    }
    for (p <- peers) {
      val block = nextBlockIndex()
      if (block.isDefined && !requestedBlocks.contains(block.get) && !peersWithPendingRequest.contains(p) && !failingPeers.contains(p)) {
        val request = Request(currentPiece, block.get * torrent.defaultBlockSize, torrent.blockSize(currentPiece, block.get))
        client ! RequestBlock(request, p)
        requestedBlocks.add(block.get)
        peersWithPendingRequest.add(p)
      }
    }
  }

  private def downloadComplete() = !downloadedPieces.contains(false)

  private def nextPieceIndex(): Int = {
    val availablePieces = downloadedPieces.zipWithIndex.filter(p => !p._1)
    if (availablePieces.length > 0) {
      val randomIndex = Random.nextInt(availablePieces.length)
      availablePieces(randomIndex)._2
    } else -1
  }

  private def nextBlockIndex(): Option[Int] =
    currentBlocks.zipWithIndex.find(i => i._1 == null && !requestedBlocks.contains(i._2)).map(_._2)

  private def validatePieceHash(piece: Int, bytes: Array[Byte]): Boolean = {
    val hash: Array[Byte] = messageDigest.digest(bytes)
    hash.sameElements(torrent.pieceVerificationHash(piece))
  }

  private def initializeFiles(): Unit = {
//    if (torrent.files.length > 1) {
//      if (!new File(downloadPath).mkdir()) {
//        log.error("Could not create directory for torrent")
//        client ! "stop"
//      }
//    }
//    for (fileInfo <- torrent.files) {
//      val raf = new RandomAccessFile(filePath(fileInfo), "rw")
//      raf.setLength(fileInfo.length)
//      raf.close()
//    }
  }

  private def filePath(f: FileInfo) = downloadPath + File.pathSeparator + f.path.mkString(File.pathSeparator)

  private def persistPieceToDisk(bytes: Array[Byte]): Unit = {
//    if (torrent.files.length == 1) {
//      persistPieceForSingleFileTorrent(bytes)
//    } else {
//      persistPieceForMultipleFileTorrent(currentPiece, bytes)
//    }
    downloadedPieces(currentPiece) = true
    val remaining = downloadedPieces.count(p => !p)
    log.debug("Piece #" + currentPiece + " downloaded, " + remaining + " remaining.")
  }

  private def persistPieceForSingleFileTorrent(bytes: Array[Byte]): Unit = {
    val pieceOffset = currentPiece * torrent.defaultPieceSize
    val f = new RandomAccessFile(filePath(torrent.files.head), "rw")
    f.seek(pieceOffset)
    f.write(bytes)
    f.close()
  }

  private def persistPieceForMultipleFileTorrent(piece: Int, bytes: Array[Byte]): Unit = {
    val pieceSize = torrent.pieceSize(piece)
    val fileStartingPositions: List[Long] = torrent.files.map(_.length).scan(0L)(_ + _)

    val files = for {
      (file, fileStart) <- torrent.files.zip(fileStartingPositions)
      if ((fileStart <= piece && piece < fileStart + file.length) // Piece start is within file
        || (fileStart >= piece && piece + pieceSize > fileStart)) // Piece overlaps file
    } yield file

    val firstPieceOffset = piece * pieceSize - fileStartingPositions.head
    writePieceToFiles(bytes, files, firstPieceOffset)
  }

  @tailrec
  private def writePieceToFiles(bytes: Array[Byte], files: List[FileInfo], pieceOffset: Long): Unit = {
    if (files.isEmpty) return
    val file = files.head
    val byteCount =
      if (pieceOffset + bytes.length > file.length) (file.length - pieceOffset).toInt
      else bytes.length

    val f = new RandomAccessFile(filePath(file), "rw")
    f.seek(pieceOffset)
    f.write(bytes.take(byteCount))
    f.close()

    // if block overlaps multiple files, the subsequent pieces are always at the beginning of file
    writePieceToFiles(bytes.drop(byteCount), files.tail, 0)
  }

}

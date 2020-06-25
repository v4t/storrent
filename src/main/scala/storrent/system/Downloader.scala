package storrent.system

import java.io.{File, RandomAccessFile}

import akka.actor.{Actor, ActorLogging, ActorRef}
import storrent.peerprotocol.Request
import storrent.system.messages.Client.{BlockReceived, RequestBlock, RequestBlockFailed, StopClient}
import storrent.system.messages.Downloader.{AddPeer, RemovePeer}
import storrent.torrent.{FileInfo, Torrent}
import storrent.trackerprotocol.PeerInfo

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

class Downloader(client: ActorRef, torrent: Torrent, saveDir: String) extends Actor with ActorLogging {

  type BlockList = Array[Array[Byte]]

  /** Path to download location. */
  private val downloadPath =
    if (torrent.files.length == 1) saveDir
    else saveDir + File.separator + torrent.metaInfo.info.name

  /** Message digest for piece validation. */
  private val messageDigest = java.security.MessageDigest.getInstance("SHA-1")

  /** Currently available peers. */
  private val peers = mutable.Set[PeerInfo]()

  /** Peers that have pending requests (client should only make one request at a time to a peer). */
  private val peersWithPendingRequest = mutable.Set[PeerInfo]()

  /** Peers that have failed to respond to block request for current piece. */
  private val failingPeers = mutable.Set[PeerInfo]()

  /** Array representing pieces that have already been downloaded. */
  private val downloadedPieces: Array[Boolean] = new Array(torrent.pieceCount)

  /** Blocks that have been requested for current piece. */
  private val requestedBlocks: mutable.Set[Int] = mutable.Set[Int]()

  /** Index of the piece currently being downloaded. */
  private var currentPiece: Int = nextPieceIndex()

  /** Array containing blocks that have been downloaded successfully for current piece. */
  private var currentBlocks: BlockList = newBlockBuffer()

  /**
   * Initialize torrent files before starting download.
   */
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

  /**
   * Request blocks to be downloaded from peers if download is not complete.
   */
  private def requestBlocks(): Unit = {
    if (currentPiece < 0 || downloadComplete()) {
      log.info("Download complete")
      return
    }
    if (downloadedPieces(currentPiece)) {
      currentPiece = nextPieceIndex()
      currentBlocks = newBlockBuffer()
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

  /**
   * Return boolean flag signifying whether download is complete or not.
   *
   * @return True if download is complete, otherwise false
   */
  private def downloadComplete() = !downloadedPieces.contains(false)

  /**
   * Get index for piece that will be downloaded next.
   *
   * @return Piece index
   */
  private def nextPieceIndex(): Int = {
    // TODO: Implement better piece selection
    val availablePieces = downloadedPieces.zipWithIndex.filter(p => !p._1)
    if (availablePieces.length > 0) {
      val randomIndex = Random.nextInt(availablePieces.length)
      availablePieces(randomIndex)._2
    } else -1
  }

  /**
   * Initialize new current blocks array based on current piece block count.
   *
   * @return
   */
  private def newBlockBuffer(): BlockList =
    if (currentPiece < 0) new BlockList(0)
    else new BlockList(torrent.blockCount(currentPiece))

  /**
   * Get index for block that will be downloaded next (blocks are requested sequentially).
   *
   * @return Block index
   */
  private def nextBlockIndex(): Option[Int] =
    currentBlocks.zipWithIndex
      .find(i => i._1 == null && !requestedBlocks.contains(i._2))
      .map(_._2)

  /**
   * Check that downloaded piece is correct.
   *
   * @param piece Piece index
   * @param bytes Piece bytes
   * @return True if piece is correct, otherwise false
   */
  private def validatePieceHash(piece: Int, bytes: Array[Byte]): Boolean = {
    val hash: Array[Byte] = messageDigest.digest(bytes)
    hash.sameElements(torrent.pieceVerificationHash(piece))
  }

  /**
   * Set up files specified in torrent file.
   */
  private def initializeFiles(): Unit = {
    if (torrent.files.length > 1) {
      if (!new File(downloadPath).mkdir()) {
        log.error("Could not create directory for torrent")
        client ! StopClient
      }
    }
    for (fileInfo <- torrent.files) {
      val raf = new RandomAccessFile(filePath(fileInfo), "rw")
      raf.setLength(fileInfo.length)
      raf.close()
    }
  }

  /**
   * Return download file path for given file.
   *
   * @param f File
   * @return File path
   */
  private def filePath(f: FileInfo) = downloadPath + File.separator + f.path.mkString(File.separator)

  /**
   * Write downloaded piece to file / files.
   *
   * @param bytes Piece bytes
   */
  private def persistPieceToDisk(bytes: Array[Byte]): Unit = {
    if (torrent.files.length == 1) {
      persistPieceForSingleFileTorrent(currentPiece, bytes)
    } else {
      persistPieceForMultipleFileTorrent(currentPiece, bytes)
    }
    downloadedPieces(currentPiece) = true
    val remaining = downloadedPieces.count(p => !p)
    log.info("Piece #" + currentPiece + " downloaded, " + remaining + " remaining.")
  }

  /**
   * Write piece to a file specified in a single file torrent.
   *
   * @param piece Piece index
   * @param bytes Piece bytes
   */
  private def persistPieceForSingleFileTorrent(piece: Int, bytes: Array[Byte]): Unit = {
    val pieceOffset = piece * torrent.defaultPieceSize
    val f = new RandomAccessFile(filePath(torrent.files.head), "rw")
    f.seek(pieceOffset)
    f.write(bytes)
    f.close()
  }

  /**
   * Write piece to file(s) specified in a multi-file torrent.
   *
   * @param piece Piece index
   * @param bytes Piece bytes
   */
  private def persistPieceForMultipleFileTorrent(piece: Int, bytes: Array[Byte]): Unit = {
    val pieceSize = torrent.pieceSize(piece)
    val fileOffsets: List[Long] = torrent.files.map(_.length).scan(0L)(_ + _)
    val pieceOffset = piece * pieceSize

    // Data is just flat byte stream, so we have to figure out the file offsets to place the file on the byte stream.
    val filesWithOffsets = for {
      (file, fileStart) <- torrent.files.zip(fileOffsets)
      if ((fileStart <= pieceOffset && pieceOffset < fileStart + file.length) // Piece byte offset is within file
        || (fileStart >= pieceOffset && pieceOffset + pieceSize > fileStart)) // Piece overlaps file
    } yield (file, fileStart)

    // Also find out the offset for piece on file
    val pieceOffsetWithinFile = pieceOffset - filesWithOffsets.head._2
    writePieceToFiles(bytes, filesWithOffsets.map(_._1), pieceOffsetWithinFile)
  }

  /**
   * Write piece to files it possibly spans over.
   *
   * @param bytes       Piece bytes
   * @param files       Files that contain bytes from piece
   * @param pieceOffset Piece offset for current file
   */
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

    // if piece overlaps multiple files, the subsequent byte blocks are always at the beginning of file
    writePieceToFiles(bytes.drop(byteCount), files.tail, 0)
  }

}

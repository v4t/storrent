package storrent

import java.io.{File, RandomAccessFile}

import akka.actor.{Actor, ActorRef}
import storrent.metainfo.Torrent
import storrent.peers.Request
import storrent.tracker.PeerInfo

import scala.collection.mutable
import scala.util.Random

case class AddPeer(peer: PeerInfo)

case class RemovePeer(peer: PeerInfo)

class Downloader(client: ActorRef, torrent: Torrent) extends Actor with ActorLogging{
  private val basePath = "D:\\Downloads\\Test"
  private val downloadPath =
    if (torrent.files.length == 1) basePath
    else basePath + "\\" + torrent.metaInfo.info.name

  private val messageDigest = java.security.MessageDigest.getInstance("SHA-1")
  private val peers = mutable.Set[PeerInfo]()
  private val peersWithPendingRequest = mutable.Set[PeerInfo]()
  private val failingPeers = mutable.Set[PeerInfo]()
  //  private val failedRequestCounts = mutable.Map[String, Int]()

  private var filesReady = false
  private var downloadComplete = false
  private var downloadedPieces: Array[Boolean] = new Array(torrent.pieceCount)

  private var currentPiece: Int = -1
  private var pieceComplete = true
  private var currentBlocks: Array[Array[Byte]] = Array[Array[Byte]]()
  private val requestedBlocks: mutable.Set[Int] = mutable.Set[Int]()



  def receive: Receive = {
    case AddPeer(peer) =>
      peers.add(peer)
      log.debug("Added peer " + peer.peerId)

    case RemovePeer(peer) =>
      peers.remove(peer)
      log.debug("Removed peer " + peer.peerId)

    case "download" =>
      if (!filesReady) {
        initializeFiles()
      }
      val pieceIndex = nextPieceIndex()
      if (pieceIndex < 0) {
        downloadComplete = true
        log.debug("All pieces have been downloaded")
      } else {

        if (pieceComplete) {
          val blockCount = torrent.blockCount(pieceIndex)
          currentBlocks = new Array[Array[Byte]](blockCount)
          pieceComplete = false
        }

        for (p <- peers) {
          val block = nextBlockIndex()
          if (block.isDefined && !requestedBlocks.contains(block.get) && !peersWithPendingRequest.contains(p) && !failingPeers.contains(p)) {
            val request = Request(pieceIndex, block.get * torrent.defaultBlockSize, torrent.blockSize(pieceIndex, block.get))
            client ! RequestBlock(request, p)
            requestedBlocks.add(block.get)
            peersWithPendingRequest.add(p)
          }
        }
      }
      if (!downloadComplete) self ! "download"

    case RequestBlockFailed(index, peer) =>
      val block = index / torrent.defaultBlockSize
      log.debug("Failed to retrieve block " + block)
      failingPeers.add(peer)
      requestedBlocks.remove(block)

    case BlockReceived(piece, peer) =>
      peersWithPendingRequest.remove(peer)
      val block = piece.begin / torrent.defaultBlockSize
      if (currentBlocks.isDefinedAt(block)) {
        currentBlocks(block) = piece.block
      }
      val blocksRemaining = currentBlocks.count(i => i == null)
      log.debug("Blocks remaining " + blocksRemaining)
      if (blocksRemaining == 0) {
        if (validateCurrentPiece(nextPieceIndex())) {
          log.debug("Current piece hash valid")
          writePieceToFile()
        } else {
          log.debug("Failed to verify current piece")
        }
        pieceComplete = true
        requestedBlocks.clear()
      }

  }

  private def nextPieceIndex(): Int =  {
    if (currentPiece >= 0 && !pieceComplete) return currentPiece
    val z = downloadedPieces.zipWithIndex.filter(p => !p._1)
    if (z.length > 0) {
      val randomIndex = Random.nextInt(z.length)
      val nextIndex = z(randomIndex)._2
      log.debug("Selected piece " + nextIndex + " for next")
      currentPiece = nextIndex
      nextIndex
    } else -1
  }

  private def nextBlockIndex(): Option[Int] =
    currentBlocks.zipWithIndex.find(i => i._1 == null && !requestedBlocks.contains(i._2)).map(_._2)

  private def validateCurrentPiece(piece: Int): Boolean = {
    val hash: Array[Byte] = messageDigest.digest(currentBlocks.flatten)
    hash.sameElements(torrent.pieceVerificationHash(piece))
  }

  private def initializeFiles(): Unit = {
//    if (torrent.files.length > 1) {
//      if (!new File(downloadPath).mkdir()) {
//        log.debug("Could not create directory for torrent")
//        client ! "stop"
//      }
//    }
//    for (fileInfo <- torrent.files) {
//      val fname = downloadPath + "\\" + fileInfo.path.mkString("\\")
//      val raf = new RandomAccessFile(fname, "rw")
//      raf.setLength(fileInfo.length)
//      raf.close()
//    }
  }

  private def writePieceToFile(): Unit = {
    val fileInfo = torrent.files.head
    val pieceIndex = currentPiece
    val filePos = pieceIndex * torrent.defaultPieceSize

//    val f = new RandomAccessFile(downloadPath + "\\" + fileInfo.path.mkString("\\"), "rw")
//    f.seek(filePos)
//    f.write(currentBlocks.flatten)
//    f.close()
    downloadedPieces(pieceIndex) = true
    log.debug("piece " + pieceIndex + " written to file")
  }

}

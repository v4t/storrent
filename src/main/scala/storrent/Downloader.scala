package storrent

import akka.actor.{Actor, ActorRef}
import storrent.metainfo.Torrent
import storrent.peers.{Piece, Request}
import storrent.tracker.PeerInfo

import scala.collection.mutable

case class AddPeer(peer: PeerInfo)
case class RemovePeer(peer: PeerInfo)

class Downloader(client: ActorRef, torrent: Torrent) extends Actor {

  private val peers = mutable.Set[PeerInfo]()
  private val peersWithPendingRequest = mutable.Set[PeerInfo]()
  private val failingPeers = mutable.Set[PeerInfo]()
  //  private val failedRequestCounts = mutable.Map[String, Int]()

  private var downloadComplete = false
  private var downloadedPieces: Array[Boolean] = new Array(torrent.pieceCount)

  //  private var currentPiece: Int = 0
  private var pieceComplete = true
  private var currentBlocks: Array[Array[Byte]] = Array[Array[Byte]]()
  private val requestedBlocks: mutable.Set[Int] = mutable.Set[Int]()


  def receive: Receive = {
    case AddPeer(peer) =>
      peers.add(peer)
      println("Added peer " + peer.peerId)

    case RemovePeer(peer) =>
      peers.remove(peer)
      println("Removed peer " + peer.peerId)

    case "download" =>
      val pieceIndex = nextPieceIndex()
      if (pieceIndex < 0) {
        downloadComplete = true
        println("All pieces have been downloaded")
      } else {

        if(pieceComplete) {
          val blockCount = torrent.blockCount(pieceIndex)
          currentBlocks = new Array[Array[Byte]](blockCount)
          pieceComplete = false
        }

        for (p <- peers) {
          val block = nextBlockIndex()
          if (block.isDefined && !requestedBlocks.contains(block.get) && !peersWithPendingRequest.contains(p) && !failingPeers.contains(p)) {
            val request = Request(block.get, block.get * torrent.defaultBlockSize, torrent.blockSize(pieceIndex, block.get))
            client ! RequestBlock(request, p)
            requestedBlocks.add(block.get)
            peersWithPendingRequest.add(p)
          }
        }
      }
      if (!downloadComplete) self ! "download"

    case RequestBlockFailed(index, peer) =>
      println("Failed to retrieve block " + index)
      failingPeers.add(peer)
      requestedBlocks.remove(index)

    case BlockReceived(piece, peer) =>
      peersWithPendingRequest.remove(peer)
      val index = piece.index
      if (currentBlocks.isDefinedAt(index))  {
        currentBlocks(index) = piece.block
      }
      val blocksRemaining = currentBlocks.count(i => i == null)
  }


  private def nextPieceIndex(): Int = downloadedPieces.indexOf(false)

  private def nextBlockIndex(): Option[Int] =
    currentBlocks.zipWithIndex.find(i => i._1 == null && !requestedBlocks.contains(i._2)).map(_._2)
}

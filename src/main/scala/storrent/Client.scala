package storrent

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import storrent.metainfo.Torrent
import storrent.peers.{Piece, Request}
import storrent.tracker.{PeerInfo, Started}

import scala.collection.mutable
import scala.util.Random

case class UpdatePeers(peers: List[PeerInfo])

case class PeerConnected(peer: PeerInfo, actor: ActorRef)

case class PeerDisconnected(peer: PeerInfo)

case class RequestFailed(index: Int, peer: PeerInfo)

class Client(torrent: Torrent, system: ActorSystem) extends Actor {
  private val localId = Random.alphanumeric.take(20).mkString("")


  private val port = 55555
  private val peerActorMap = mutable.Map[String, ActorRef]()
  private val peers = mutable.Set[PeerInfo]()
  private val peersWithPendingRequest = mutable.Set[PeerInfo]()
  //  private val failedRequestCounts = mutable.Map[String, Int]()

  private var downloadComplete = false
  private var downloadedPieces: Array[Boolean] = new Array(torrent.pieceCount)

  //  private var currentPiece: Int = 0
  private var currentBlocks: Array[Array[Byte]] = Array[Array[Byte]]()
  private val requestedBlocks: mutable.Set[Int] = mutable.Set[Int]()

  private val listener = context.actorOf(
    Props(classOf[ConnectionListener], new InetSocketAddress(port), system),
    "listener"
  )

  private val tracker = context.actorOf(
    Props(classOf[Tracker], localId, port),
    "tracker"
  )

  def receive: Receive = {
    case "stop" =>
      println("stopped " + localId)
      system.terminate()

    case "start" =>
      println("update tracker")
      tracker ! Update(torrent, Started)
      self ! "download"

    case UpdatePeers(peerList) =>
      println("update peers " + peerList)
      peerList.foreach(p => context.actorOf(
        Props(classOf[Peer], p, torrent, localId, self),
        "peer-" + p.ip + ":" + p.port + Random.alphanumeric.take(5).mkString("")
      ))

    case PeerConnected(peer, actor) =>
      println(peer.peerId + ": connected")
      peerActorMap.put(peer.peerId, actor)
      peers.add(peer)

    case PeerDisconnected(peer) =>
      println(peer.peerId + ": disconnected")
      if (peerActorMap.isDefinedAt(peer.peerId)) peerActorMap.remove(peer.peerId)
      peers.remove(peer)

    case "download" =>
      val pieceIndex = nextPieceIndex()
      if (pieceIndex < 0) {
        downloadComplete = true
        println("All pieces have been downloaded")
      } else {

        val blockCount = torrent.blockCount(pieceIndex)
        currentBlocks = new Array[Array[Byte]](blockCount)

        for (p <- peers) {
          val block = nextBlockIndex()
          if(block.isDefined && !requestedBlocks.contains(block.get) && !peersWithPendingRequest.contains(p)) {
            peerActorMap(p.peerId) ! Request(block.get, block.get * torrent.defaultBlockSize, torrent.blockSize(pieceIndex, block.get))
            requestedBlocks.add(block.get)
            peersWithPendingRequest.add(p)
          }
        }
      }
      if (!downloadComplete) self ! "download"

    case RequestFailed(index, peer) =>
      peersWithPendingRequest.remove(peer)
      requestedBlocks.remove(index)

    case Piece(index, begin, block) =>
      println("Client has piece index " + index)
      if(currentBlocks.isDefinedAt(index)) currentBlocks(index) = block
      val piecesRemaining = currentBlocks.count(i => i == null)
      println(piecesRemaining + " pieces remaining")


  }

  private def nextPieceIndex(): Int = downloadedPieces.indexOf(false)

  private def nextBlockIndex(): Option[Int] =
    currentBlocks.zipWithIndex.find(i => i._1 == null && !requestedBlocks.contains(i._2)).map(_._2)
}

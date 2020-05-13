package storrent

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import storrent.metainfo.Torrent
import storrent.tracker.{PeerInfo, Started}

import scala.collection.mutable
import scala.util.Random

case class UpdatePeers(peers: List[PeerInfo])
case class PeerConnected(peer: PeerInfo, actor: ActorRef)
case class PeerDisconnected(peerId: String)

class Client(torrent: Torrent, system: ActorSystem) extends Actor {
  private val localId = Random.alphanumeric.take(20).mkString("")
  private val port = 55555
  private val peers = mutable.Map[String, PeerInfo]()

  private var downloadComplete = false
  private var downloadedPieces: Array[Boolean] = new Array(torrent.pieceCount)

  private var currentPiece: Int = 0
  private var currentBlocks: Vector[Array[Byte]] = Vector[Array[Byte]]()

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

    case UpdatePeers(peerList) =>
      println("update peers " + peerList)
      peerList.foreach(p => context.actorOf(
        Props(classOf[Peer], p, torrent, localId, self),
        "peer-" + p.ip + ":" + p.port
      ))

    case PeerConnected(peer, actor) =>
      println("peer connected")
      if(!peers.isDefinedAt(peer.peerId)) peers.put(peer.peerId, peer)

    case PeerDisconnected(peerId) =>
      println("peer disconnected")
      if(peers.isDefinedAt(peerId)) peers.remove(peerId)

    case "download" =>
      val pieceIndex = nextPieceIndex()
      if(pieceIndex < 0) {
        downloadComplete = true
        println("All pieces have been downloaded")
      } else {

      }
      if(!downloadComplete) self ! "download"


  }

  private def nextPieceIndex() : Int = downloadedPieces.indexOf(false)
}

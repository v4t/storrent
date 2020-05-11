package storrent

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import storrent.metainfo.MetaInfo
import storrent.tracker.{PeerInfo, Started}

import scala.collection.mutable
import scala.util.Random

case class UpdatePeers(peers: List[PeerInfo])
case class PeerConnected(peer: PeerInfo, actor: ActorRef)
case class PeerDisconnected(peerId: String)

class Client(metaInfo: MetaInfo, system: ActorSystem) extends Actor {
  private val localId = Random.alphanumeric.take(20).mkString("")
  private val port = 55555
  private val peers = mutable.Map[String, PeerInfo]()

  private var downloadComplete = false
  private var downloadedPieces: Array[Boolean] = new Array(metaInfo.pieceCount)

  private var currentPiece: Int = 0
  private var currentBlocks: Vector[Array[Byte]] = Vector[Array[Byte]]()

  private val listener = context.actorOf(
    Props(classOf[ConnectionListener], new InetSocketAddress(port), system),
    "listener"
  )

  private val tracker = context.actorOf(
    Props(classOf[Tracker], port),
    "tracker"
  )

  def receive: Receive = {
    case "stop" =>
      println("stopped " + localId)
      system.terminate()

    case "start" =>
      println("update tracker")
      tracker ! Update(metaInfo, Started)

    case UpdatePeers(peerList) =>
      println("update peers " + peerList)
      peerList.foreach(p => context.actorOf(
        Props(classOf[Peer], p, metaInfo, localId, self),
        p.peerId
      ))

    case PeerConnected(peer, actor) =>
      println("peer connected")
      if(!peers.isDefinedAt(peer.peerId)) peers.put(peer.peerId, peer)

    case PeerDisconnected(peerId) =>
      println("peer disconnected")
      if(peers.isDefinedAt(peerId)) peers.remove(peerId)
  }

  private def nextPiece() : Int = downloadedPieces.indexOf(false)
}

package storrent

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import storrent.metainfo.Torrent
import storrent.peers.{Piece, Request}
import storrent.tracker.{PeerInfo, Started}

import scala.collection.mutable
import scala.util.Random

case class UpdatePeers(peers: List[PeerInfo])

case class PeerConnected(peer: PeerInfo, actor: ActorRef)

case class PeerDisconnected(peer: PeerInfo)

case class ChokeReceived(peer: PeerInfo)

case class UnchokeReceived(peer: PeerInfo)

case class RequestBlockFailed(index: Int, peer: PeerInfo)

case class RequestBlock(request: Request, peer: PeerInfo)

case class BlockReceived(block: Piece, peer: PeerInfo)

class Client(torrent: Torrent, system: ActorSystem) extends Actor with ActorLogging {
  private val localId = Random.alphanumeric.take(20).mkString("")
  private val port = 55555
  private val peerActorMap = mutable.Map[String, ActorRef]()

  var first = true;

  private val listener = context.actorOf(
    Props(classOf[ConnectionListener], new InetSocketAddress(port), system),
    "listener"
  )

  private val tracker = context.actorOf(
    Props(classOf[Tracker], localId, port),
    "tracker"
  )

  private val downloader = context.actorOf(
    Props(classOf[Downloader], self, torrent),
    "downloader"
  )

  def receive: Receive = {
    case "stop" =>
      log.info("Stopping the client")
      system.terminate()

    case "start" =>
      tracker ! Update(torrent, Started)

    case UpdatePeers(peerList) =>
      peerList.foreach(p => context.actorOf(
        Props(classOf[Peer], p, torrent, localId, self),
        "peer-" + p.ip + ":" + p.port + Random.alphanumeric.take(5).mkString("")
      ))

    case PeerConnected(peer, actor) =>
      log.debug(peer.peerId + ": connected")
      peerActorMap.put(peer.peerId, actor)

    case PeerDisconnected(peer) =>
      log.debug(peer.peerId + ": disconnected")
      if (peerActorMap.isDefinedAt(peer.peerId)) peerActorMap.remove(peer.peerId)
      downloader ! RemovePeer(peer)

    case ChokeReceived(peer) =>
      downloader ! RemovePeer(peer)

    case UnchokeReceived(peer) =>
      downloader ! AddPeer(peer)
      if (first) {
        first = false
        downloader ! "download"
      }

    case RequestBlock(request, peer) =>
      if (peerActorMap.contains(peer.peerId)) peerActorMap(peer.peerId) ! request

    case msg@RequestBlockFailed(index, peer) =>
      downloader ! msg

    case msg@BlockReceived(piece, peer) =>
      downloader ! msg

  }
}

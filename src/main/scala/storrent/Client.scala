package storrent

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import storrent.metainfo.MetaInfo
import storrent.peers.Handshake
import storrent.tracker.{PeerInfo, Started}

import scala.collection.mutable

case class UpdatePeers(peers: List[PeerInfo])

case class PeerConnected(peer: PeerInfo, actor: ActorRef)

class Client(metaInfo: MetaInfo, system: ActorSystem) extends Actor {
  private val localId = "19510014123456654321"
  private val port = 55555
  private val peers = mutable.Set[PeerInfo]()

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
      println("updatetracker")
      tracker ! Update(metaInfo, Started)

    case UpdatePeers(peerList) =>
      println("updatepeers " + peerList)
      val p = peerList.tail.head
      //      val peerActor = context.actorOf(
      //        Props(classOf[Peer], p, metaInfo, localId, self),
      //        "peer:" + p.ip + ":" + p.port
      //      )
      peerList.foreach(p => context.actorOf(
        Props(classOf[Peer], p, metaInfo, localId, self),
        "peer:" + p.ip + ":" + p.port
      ))

    case PeerConnected(peer, actor) =>
      println("peerconnected")
      actor ! Handshake(metaInfo.infoHash, localId)
  }

  def sendHandshake(peerInfo: PeerInfo) = {

  }
}
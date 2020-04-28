package storrent

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import storrent.metainfo.MetaInfo
import storrent.tracker.{PeerInfo, Started}

import scala.collection.mutable

case class UpdatePeers(peers: List[PeerInfo])

class Client(metaInfo: MetaInfo, system: ActorSystem) extends Actor {
  private val localId = "19510014123456654321"
  private val port = 6881
  private val peers = mutable.Set[PeerInfo]()

  private val listener = context.actorOf(
    Props(classOf[ConnectionListener], new InetSocketAddress(InetAddress.getLocalHost, port), system),
    "listener"
  )

  private val tracker = context.actorOf(
    Props(classOf[Tracker], port),
    "tracker"
  )

  def receive: Receive = {
    case "stop" =>
      println(localId)
      system.terminate()

    case "start" =>
      tracker ! Update(metaInfo, Started)

    case UpdatePeers(peerList) =>
      println(peerList)
  }
}
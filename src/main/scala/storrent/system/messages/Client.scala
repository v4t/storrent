package storrent.system.messages

import akka.actor.ActorRef
import storrent.peerprotocol.{Piece, Request}
import storrent.trackerprotocol.PeerInfo

object Client {

  case object StartClient

  case object StopClient

  case object Complete

  case class UpdatePeersFromTracker(peers: List[PeerInfo], interval: Long)

  case class PeerConnected(peer: PeerInfo, actor: ActorRef)

  case class PeerDisconnected(peer: PeerInfo)

  case class ChokeReceived(peer: PeerInfo)

  case class UnchokeReceived(peer: PeerInfo)

  case class RequestBlockFailed(request: Request, peer: PeerInfo)

  case class RequestBlock(request: Request, peer: PeerInfo)

  case class BlockReceived(block: Piece, peer: PeerInfo)

}

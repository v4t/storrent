package storrent.system.messages

import storrent.trackerprotocol.PeerInfo

object Downloader {

  case class AddPeer(peer: PeerInfo)

  case class RemovePeer(peer: PeerInfo)

}

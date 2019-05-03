package storrent.tracker

case class Peer(
  ip: String,
  port: Int,
  peerId: Option[String] = None
)

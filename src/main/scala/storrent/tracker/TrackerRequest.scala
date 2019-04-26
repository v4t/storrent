package storrent.tracker

case class TrackerRequest(
  infoHash: String,
  peerId: String,
  ip: String,
  port: Int,
  uploaded: Long,
  downloaded: Long,
  left: Long,
  event: Option[String],
//  started: Int,
//  stopped: Int,
//  completed: Int,
  numWant: Option[Int],
  trackerId: Option[String]
)

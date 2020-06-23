package storrent.trackerprotocol

case class TrackerRequest(
  baseUrl: String,
  infoHash: String,
  uploaded: Long,
  downloaded: Long,
  left: Long,
  peerId: String,
  port: Int,
  compact: Int,
  event: Option[TrackerEvent],
  numWant: Option[Int],
  ip: Option[String],
)

object TrackerRequest {

  def getQueryString(request: TrackerRequest): String =
    request.baseUrl + "?" +
      s"info_hash=${request.infoHash}&" +
      s"peer_id=${request.peerId}&" +
      s"uploaded=${request.uploaded}&" +
      s"downloaded=${request.downloaded}&" +
      s"left=${request.left}&" +
      s"port=${request.port}&" +
      s"compact=${request.compact}" +
      (if (request.event.isDefined) s"&event=${eventToStr(request.event.get)}" else "") +
      (if (request.numWant.isDefined) s"&numwant=${request.numWant.get}" else "") +
      (if (request.ip.isDefined) s"&ip=${request.ip.get}" else "")


  private def eventToStr(event: TrackerEvent): String = event match {
    case Started => "started"
    case Stopped => "stopped"
    case Completed => "completed"
  }

}
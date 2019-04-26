package storrent.tracker

sealed trait TrackerResponse

case class SuccessResponse(
  warningMessage: Option[String],
  interval: Int,
  minInterval: Option[Int],
  trackerId: Option[String],
  complete: Int,
  incomplete: Int,
  peers: List[Peer]
) extends TrackerResponse

case class FailureResponse(failureReason: String) extends TrackerResponse

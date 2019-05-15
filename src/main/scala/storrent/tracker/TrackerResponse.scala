package storrent.tracker

import storrent.bencode._

import scala.util.Success

sealed trait TrackerResponse

case class SuccessResponse(
  interval: Long,
  peers: List[PeerInfo],
  warningMessage: Option[String],
  complete: Option[Long],
  incomplete: Option[Long],
  minInterval: Option[Long],
  trackerId: Option[String]
) extends TrackerResponse

case class FailureResponse(failureReason: String) extends TrackerResponse

object TrackerResponse {

  def parse(body: String): TrackerResponse = {
    BencodeParser.parse(body) match {
      case Success(List(BencodeDict(map))) => populateSuccessResponse(map)
      case _ => FailureResponse("Invalid format: " + body)
    }
  }

  private def populateSuccessResponse(dict: Map[BencodeString, BencodeValue]): SuccessResponse =
    SuccessResponse(
      interval = interval(dict),
      peers = peers(dict),
      complete = dict.get(BencodeString("min interval")) match {
        case Some(BencodeInt(value)) => Some(value)
        case _ => None
      },
      incomplete = dict.get(BencodeString("min interval")) match {
        case Some(BencodeInt(value)) => Some(value)
        case _ => None
      },
      warningMessage = dict.get(BencodeString("warning message")) match {
        case Some(BencodeString(value)) => Some(value)
        case _ => None
      },
      minInterval = dict.get(BencodeString("min interval")) match {
        case Some(BencodeInt(value)) => Some(value)
        case _ => None
      },
      trackerId = dict.get(BencodeString("tracker id")) match {
        case Some(BencodeString(value)) => Some(value)
        case _ => None
      }
    )

  private def interval(dict: Map[BencodeString, BencodeValue]): Long =
    dict.get(BencodeString("interval")) match {
      case Some(BencodeInt(value)) => value
      case _ => throw TrackerException("Field 'interval' must be numeric")
    }

  private def peers(dict: Map[BencodeString, BencodeValue]): List[PeerInfo] =
    dict.get(BencodeString("peers")) match {
      case Some(BencodeList(ls)) => PeerInfo.from(ls)
      case Some(BencodeString(str)) => PeerInfo.from(str)
      case None => throw TrackerException("Field 'peers' is required")
      case Some(_) => throw TrackerException("Field 'peers' should be a list or a string")
    }
}
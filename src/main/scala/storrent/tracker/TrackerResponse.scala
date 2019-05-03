package storrent.tracker

import java.net.InetAddress
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import storrent.bencode._

sealed trait TrackerResponse

case class SuccessResponse(
  interval: Long,
  peers: List[Peer],
  warningMessage: Option[String],
  complete: Option[Long],
  incomplete: Option[Long],
  minInterval: Option[Long],
  trackerId: Option[String]
) extends TrackerResponse

case class FailureResponse(failureReason: String) extends TrackerResponse

object TrackerResponse {

  def parse(body: String): TrackerResponse = {
    try {
      BencodeParser.parse(body) match {
        case List(BencodeDict(map)) => populateSuccessResponse(map)
        case _ => FailureResponse("Invalid format: " + body)
      }
    } catch {
      case BencodeParseException(_) => FailureResponse("Request failed: " + body)
    }
  }

  private def populateSuccessResponse(dict: Map[BencodeString, BencodeValue]): SuccessResponse = {
    println("TODO populate response")
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
  }

  private def interval(dict: Map[BencodeString, BencodeValue]): Long =
    dict.get(BencodeString("interval")) match {
      case Some(BencodeInt(value)) => value
      case _ => throw TrackerException("Field 'interval' must be numeric")
    }

  private def peers(dict: Map[BencodeString, BencodeValue]): List[Peer] =
    dict.get(BencodeString("peers")) match {
      case Some(BencodeList(ls)) => peersList(ls)
      case Some(BencodeString(str)) => peersString(str)
      case None => throw TrackerException("Field 'peers' is required")
      case Some(_) => throw TrackerException("Field 'peers' should be a list or a string")
    }

  private def peersString(peers: String): List[Peer] =
    peers.getBytes(StandardCharsets.ISO_8859_1)
      .grouped(6)
      .map(p => Peer(
        ip = InetAddress.getByAddress(p.take(4)).toString.tail,
        port = ByteBuffer.wrap(p.drop(4)).getShort & 0xffff
      )).toList

  private def peersList(peers: List[BencodeValue]): List[Peer] = {
    val res = peers.map {
      case BencodeDict(p) => Peer(ip = "", port = 0)
      case _ => throw TrackerException("Peers list should contain only dictionary values")
    }
    res
  }

}
package storrent

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import akka.actor.{Actor, ActorLogging}
import scalaj.http._
import storrent.metainfo.Torrent
import storrent.tracker._

case class Update(torrent: Torrent, event: TrackerEvent)

class Tracker(localId: String, port: Int) extends Actor with ActorLogging {

  private val charSet = StandardCharsets.ISO_8859_1

  def receive: Receive = {
    case Update(torrent, event) => {
      val request = populateTrackerRequest(torrent, event)
      val query = TrackerRequest.getQueryString(request)
      val response: HttpResponse[Array[Byte]] = Http(query).asBytes
      val resStr = new String(response.body, charSet)

      TrackerResponse.parse(resStr) match {
        case sr:SuccessResponse => sender() ! UpdatePeers(sr.peers)
        case FailureResponse(msg) => log.error("Tracker request failed: " + msg)
      }
    }
  }

  private def populateTrackerRequest(torrent: Torrent, event: TrackerEvent): TrackerRequest =
    TrackerRequest(
      baseUrl = torrent.metaInfo.announceList.head,
      infoHash = URLEncoder.encode(new String(torrent.metaInfo.infoHash, charSet), charSet),
      uploaded = 0,
      downloaded = 0,
      left = torrent.totalLength,
      peerId = URLEncoder.encode(localId, charSet),
      port = port,
      compact = 1,
      event = Some(event),
      numWant = Some(40),
      ip = None
    )
}
package storrent

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import akka.actor.Actor
import scalaj.http._
import storrent.metainfo.MetaInfo
import storrent.tracker._

case class Update(metaInfo: MetaInfo, event: TrackerEvent)

class Tracker(port: Int) extends Actor {

  private val localId = "19510014123456654321"

  def receive: Receive = {
    case Update(metaInfo, event) => {
      val request = populateTrackerRequest(metaInfo, event)
      val query = TrackerRequest.getQueryString(request)
      val response: HttpResponse[Array[Byte]] = Http(query).asBytes
      val resStr = new String(response.body, StandardCharsets.ISO_8859_1)

      println("Received response from tracker")
      TrackerResponse.parse(resStr) match {
        case sr:SuccessResponse => sender() ! UpdatePeers(sr.peers)
        case FailureResponse(msg) => println("Tracker request failed: " + msg)
      }
    }
  }

  private def populateTrackerRequest(metaInfo: MetaInfo, event: TrackerEvent): TrackerRequest =
    TrackerRequest(
      baseUrl = metaInfo.announceList.head,
      infoHash = URLEncoder.encode(new String(metaInfo.infoHash, "ISO-8859-1"), "ISO-8859-1"),
      uploaded = 0,
      downloaded = 0,
      left = metaInfo.totalLength,
      peerId = URLEncoder.encode(localId, "ISO-8859-1"),
      port = port,
      compact = 1,
      event = Some(event),
      numWant = Some(40),
      ip = None
    )
}
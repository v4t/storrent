package storrent.system

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import akka.actor.{Actor, ActorLogging}
import scalaj.http._
import storrent.system.messages.Client.UpdatePeersFromTracker
import storrent.system.messages.Tracker.Update
import storrent.torrent.Torrent
import storrent.trackerprotocol._

class Tracker(localId: String, port: Int) extends Actor with ActorLogging {
  private val charSet = StandardCharsets.ISO_8859_1

  def receive: Receive = {
    case Update(torrent, event) => {
      val request = populateTrackerRequest(torrent, event)
      val query = TrackerRequest.getQueryString(request)
      val response: HttpResponse[Array[Byte]] = Http(query).timeout(connTimeoutMs = 10000, readTimeoutMs = 10000).asBytes
      val responseBody = new String(response.body, charSet)
      if (!event.contains(Stopped)) {
        TrackerResponse.parse(responseBody) match {
          case sr: SuccessResponse => sender() ! UpdatePeersFromTracker(sr.peers, sr.interval)
          case FailureResponse(msg) => {
            log.error("Tracker request failed. Reason: " + msg)
            sender() ! UpdatePeersFromTracker(List(), 30)
          }
        }
      }
    }
  }

  /**
   * Create tracker request payload.
   * @param torrent Torrent data
   * @param event   Tracker event to be sent
   * @return
   */
  private def populateTrackerRequest(torrent: Torrent, event: Option[TrackerEvent]): TrackerRequest =
    // TODO: Implement getting real data for uploaded & downloaded parameters
    TrackerRequest(
      baseUrl = torrent.metaInfo.announceList.head,
      infoHash = URLEncoder.encode(new String(torrent.metaInfo.infoHash, charSet), charSet),
      uploaded = 0,
      downloaded = 0,
      left = torrent.totalLength,
      peerId = URLEncoder.encode(localId, charSet),
      port = port,
      compact = 1,
      event = event,
      numWant = Some(50),
      ip = None
    )
}
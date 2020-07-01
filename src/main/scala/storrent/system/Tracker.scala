package storrent.system

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import akka.actor.{Actor, ActorLogging, ActorRef}
import scalaj.http._
import storrent.system.messages.Client.{StopClient, UpdatePeersFromTracker}
import storrent.system.messages.Tracker.Update
import storrent.torrent.Torrent
import storrent.trackerprotocol._

class Tracker(localId: String, port: Int) extends Actor with ActorLogging {
  private val charSet = StandardCharsets.ISO_8859_1

  def receive: Receive = {
    case Update(torrent, event, downloaded, uploaded) => {
      val request = populateTrackerRequest(torrent, event, downloaded, uploaded)
      val query = TrackerRequest.getQueryString(request)
      val response: HttpResponse[Array[Byte]] = Http(query)
        .option(HttpOptions.followRedirects(true))
        .timeout(connTimeoutMs = 10000, readTimeoutMs = 10000)
        .asBytes
      val responseBody = new String(response.body, charSet)
      if (!event.contains(Stopped)) {
        handleTrackerResponse(responseBody, sender(), event.contains(Started))
      }
    }
  }

  /**
   * Parse tracker request and return the result to client. If request is the first one and fails, stop client.
   *
   * @param body           Tracker response body
   * @param client         Client actor
   * @param initialRequest Boolean flag signifying whether request was the initial tracker request
   */
  private def handleTrackerResponse(body: String, client: ActorRef, initialRequest: Boolean = false) =
    TrackerResponse.parse(body) match {
      case sr: SuccessResponse => sender() ! UpdatePeersFromTracker(sr.peers, sr.interval)
      case FailureResponse(msg) => {
        if (initialRequest) {
          println("Failed to connect to tracker: " + msg)
          client ! StopClient
        } else {
          log.error("Tracker request failed. Reason: " + msg)
          client ! UpdatePeersFromTracker(List(), 30)
        }
      }
    }

  /**
   * Create tracker request payload.
   *
   * @param torrent Torrent data
   * @param event   Tracker event to be sent
   * @return
   */
  private def populateTrackerRequest(
    torrent: Torrent, event: Option[TrackerEvent], downloaded: Long, uploaded: Long
  ): TrackerRequest =
    // TODO: Implement getting real data for uploaded & downloaded parameters
    TrackerRequest(
      baseUrl = torrent.metaInfo.announceList.head,
      infoHash = URLEncoder.encode(new String(torrent.metaInfo.infoHash, charSet), charSet),
      uploaded = uploaded,
      downloaded = downloaded,
      left = torrent.totalLength,
      peerId = URLEncoder.encode(localId, charSet),
      port = port,
      compact = 1,
      event = event,
      numWant = Some(50),
      ip = None
    )
}
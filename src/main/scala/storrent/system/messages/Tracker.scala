package storrent.system.messages

import storrent.torrent.Torrent
import storrent.trackerprotocol.TrackerEvent

object Tracker {

  case class Update(torrent: Torrent, event: Option[TrackerEvent])
}

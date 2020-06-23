package storrent.trackerprotocol

final case class TrackerException(message: String) extends Exception(message)

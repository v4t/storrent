package storrent.trackerprotocol

sealed trait TrackerEvent

case object Started extends TrackerEvent

case object Stopped extends TrackerEvent

case object Completed extends TrackerEvent

package storrent.tracker

sealed trait TrackerEvent

case object Started extends TrackerEvent

case object Stopped extends TrackerEvent

case object Completed extends TrackerEvent

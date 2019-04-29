package storrent.tracker

sealed trait Event

case class Started() extends Event

case class Stopped() extends Event

case class Completed() extends Event

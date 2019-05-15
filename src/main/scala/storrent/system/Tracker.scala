package storrent.system

import akka.actor.Actor
import storrent.bencode.BencodeString

object Tracker {

}

class Tracker extends Actor {

  def receive = {
    case BencodeString => println("foo")
  }

}

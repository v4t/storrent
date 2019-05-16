package storrent.system

import akka.actor.Actor
import storrent.tracker.{FailureResponse, SuccessResponse, TrackerResponse}

object Tracker {

  case class SendTrackerRequest(requestUrl: String)

  case class TrackerRequestFailure(message: String)

  case class TrackerRequestSuccess(response: SuccessResponse)

}

class Tracker extends Actor {

  import Tracker._
  import java.nio.charset.StandardCharsets

  def receive = {
    case SendTrackerRequest(requestUrl) => {
      import scalaj.http._
      val response: HttpResponse[Array[Byte]] = Http(requestUrl).asBytes
      val resStr = new String(response.body, StandardCharsets.ISO_8859_1)

      val res = TrackerResponse.parse(resStr)
      val suc = res match {
        case r@SuccessResponse(_, _, _, _, _, _, _) => println("foo");sender() ! TrackerRequestSuccess(r)
        case FailureResponse(msg) => println("bar");sender() ! TrackerRequestFailure(msg)
      }
      println("got response")
    }
  }

}

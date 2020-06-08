package storrent

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, ActorSystem}
import akka.io.Tcp._
import akka.io.{IO, Tcp}

class ConnectionListener(localAddress: InetSocketAddress, actorSystem: ActorSystem) extends Actor with ActorLogging {

  IO(Tcp)(actorSystem) ! Bind(self, localAddress)

  def receive: Receive = {
    case CommandFailed(_: Bind) =>
      log.debug("Failed to start listening on " + localAddress.getHostName + ":" + localAddress.getPort)
      context stop self
      actorSystem.terminate()

    case Bound(localAddress: InetSocketAddress) =>
      log.debug("Started listening on " + localAddress)

    case Connected(remote, local) =>
      sender ! Register(self)
      log.debug("Received connection from " + remote.toString)

    case Received(data) =>
      log.debug("received data")
      log.debug(data.decodeString("ISO-8859-1"))
  }
}
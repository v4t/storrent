package storrent.system

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, ActorSystem}
import akka.io.Tcp._
import akka.io.{IO, Tcp}

class ConnectionListener(localAddress: InetSocketAddress, actorSystem: ActorSystem) extends Actor with ActorLogging {

  IO(Tcp)(actorSystem) ! Bind(self, localAddress)

  def receive: Receive = {
    case CommandFailed(_: Bind) =>
      log.info("Failed to start listening on " + localAddress.getHostName + ":" + localAddress.getPort)
      context stop self
      actorSystem.terminate()

    case Bound(localAddress: InetSocketAddress) =>
      log.info("Started listening on " + localAddress)

    case Connected(remote, local) =>
      sender ! Register(self)
      log.info("Received connection from " + remote.toString)

    case Received(data) =>
      log.info("received data")
      log.info(data.decodeString("ISO-8859-1"))
  }
}
package storrent

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorSystem}
import akka.io.Tcp._
import akka.io.{IO, Tcp}

class ConnectionListener(localAddress: InetSocketAddress, actorSystem: ActorSystem) extends Actor {

  IO(Tcp)(actorSystem) ! Bind(self, localAddress)

  def receive: Receive = {

    case CommandFailed(_: Bind) =>
      println("Failed to start listening on " + localAddress.getHostName + ":" + localAddress.getPort)
      context stop self
      actorSystem.terminate()

    case Bound(localAddress: InetSocketAddress) =>
      println("Started listening on " + localAddress)

    case Connected(remote, local) =>
      sender ! Register(self)
      println("Received connection from " + remote.toString)

    case Received(data) =>
      println("received data")
      println(data.decodeString("ISO-8859-1"))
  }
}
package storrent

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef}
import akka.io.{IO, Tcp}
import akka.util.ByteString
import storrent.peers.Handshake
import storrent.tracker.PeerInfo

import scala.concurrent.duration._
import scala.util.{Failure, Success}

class Peer(peer: PeerInfo, client: ActorRef) extends Actor {

  import Tcp._
  import context.system

  val remote = new InetSocketAddress("169.254.57.70", 6881)
  //  val remote = new InetSocketAddress(peer.ip, peer.port)

  var handshakeCompleted = false

  IO(Tcp) ! Connect(remote, timeout = Some(30.seconds))


  def receive = {
    case CommandFailed(_: Connect) =>
      client ! "Failed to connect with peer"
      context.stop(self)

    case c@Connected(remote, local) =>
      val connection = sender()
      connection ! Register(self)
      client ! PeerConnected(peer, self)
      println("connected to peer " + peer.ip)

      context.become {
        case hs: Handshake =>
          println("sending handshake to peer " + peer.ip)
          connection ! Write(ByteString(Handshake.serialize(hs)))

        case CommandFailed(w: Write) =>
          client ! "write failed"

        case Received(data) =>
          handleReceivedData(data)

        case "close" =>
          println("close connection")
          connection ! Close

        case _: ConnectionClosed =>
          client ! "connection closed"
          context.stop(self)
      }
  }

  private def handleReceivedData(data: ByteString) = {
    println("received from peer")
    println(data.length)
    println(data.decodeString("UTF-8"))
    if (!handshakeCompleted) {
      handleHandshake(data)
    }
  }

  private def handleHandshake(data: ByteString) = {
    Handshake.deserialize(data.toArray) match {
      case Success(value) =>
        println(value)
        handshakeCompleted = true
      case Failure(exception) =>
        println(exception.getMessage)
        self ! "close"
    }
  }
}
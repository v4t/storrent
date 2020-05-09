package storrent

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef}
import akka.io.{IO, Tcp}
import akka.util.ByteString
import storrent.metainfo.MetaInfo
import storrent.peers._
import storrent.tracker.PeerInfo

import scala.concurrent.duration._

class Peer(peer: PeerInfo, metaInfo: MetaInfo, localId: String, client: ActorRef) extends Actor {

  import Tcp._
  import context.system

  val remote = new InetSocketAddress(peer.ip, peer.port)

  var handshakeCompleted = false

  var choking = true
  var peerChoking = true

  var interested = false
  var peerInterested = false

  var peerBitfield: Array[Boolean] = new Array(metaInfo.pieceCount)

  IO(Tcp) ! Connect(remote, timeout = Some(30.seconds))

  def receive = {
    case CommandFailed(_: Connect) =>
      client ! "Failed to connect with peer"
      context.stop(self)

    case c@Connected(remote, local) =>
      val connection = sender()
      connection ! Register(self)
      client ! PeerConnected(peer, self)
      connection ! Write(ByteString(Handshake.encode(metaInfo.infoHash, localId)))
      println("connected to peer " + peer.ip)
      context.become(active(connection))
  }

  def active(connection: ActorRef): Receive = {
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

  private def handleReceivedData(data: ByteString): Unit = {
    if (!handshakeCompleted) {
      handleHandshake(data)
    } else {
      val x = Message.decode(data.toArray);
      if (x.isEmpty) println("Received unknown message of " + data.length + " bytes." )
      else x.get match {
        case KeepAlive() => println("Received keepalive")
        case Choke() => println("Received choke")
        case Unchoke() => println("Received unchoke")
        case Interested() => println("Received interested")
        case NotInterested() => println("Received notinterested")
        case Have(_) => println("Received have")
        case Bitfield(dp) => println("Received bitfield: " + dp.length)
        case Request(_, _, _) => println("Received request")
        case Piece(_, _, _) => println("Received piece")
        case Cancel(_, _, _) => println("Received cancel")
        case Port(_) => println("Received port")
      }
    }
  }

  private def handleHandshake(data: ByteString): Unit = {
    Handshake.decode(data.toArray) match {
      case Some(value) =>
        println(value)
        handshakeCompleted = true
      case _ =>
        println("failed to decode handshake")
        self ! "close"
    }
  }

  private def handleKeepAlive(): Unit = {

  }

  private def handleChoke(): Unit = {
    peerChoking = true
  }

  private def handleUnchoke(): Unit = {
    peerChoking = false
  }

  private def handleInterested(): Unit = {
    peerInterested = true
  }

  private def handleNotInterested(): Unit = {
    peerInterested = false
  }

  private def handleHave(index: Int): Unit = {
    if (peerBitfield.isDefinedAt(index)) peerBitfield(index) = true
  }

  private def handleBitfield(bitfield: Array[Boolean]): Unit = {
    if (bitfield.length != peerBitfield.length) {
      println("Received bitfield with invalid length")
      self ! "close"
    } else {
      for (i <- bitfield.indices) peerBitfield(i) = bitfield(i)
    }
  }

  private def handleRequest(): Unit = {
    println("Received request")
  }

  private def handlePiece(): Unit = {
    println("Received piece")
  }

  private def handleCancel(): Unit = {
    println("Received cancel")
  }

  private def handlePort(): Unit = {
    println("Received port")
  }
}
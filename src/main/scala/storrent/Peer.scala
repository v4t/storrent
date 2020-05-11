package storrent

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ReceiveTimeout}
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
      connection ! Write(ByteString(Handshake.encode(metaInfo.infoHash, localId)))
      context.become(waitingHandshake(connection))
  }

  def waitingHandshake(connection: ActorRef): Receive = {
    case Received(data) =>
      if(Handshake.decode(data.toArray).isEmpty) {
        println("Failed to decode handshake")
        self ! "close"
      } else {
        client ! PeerConnected(peer, self)
        context.become(active(connection))
      }

    case "close" => connection ! Close

    case _: ConnectionClosed => context.stop(self)
  }

  def active(connection: ActorRef): Receive = {
    case CommandFailed(w: Write) =>
      client ! "write failed"

    case Received(data) =>
      handleReceivedData(data)

    case "request" =>
      val index = 0
      if (!peerChoking && peerBitfield(index)) {
        val bytes = Request.encode(0,0,0)
        connection ! Write(ByteString(bytes))
      }

    case "close" =>
      println("close connection")
      connection ! Close

    case _: ConnectionClosed =>
      client ! PeerDisconnected(peer.peerId)
      context.stop(self)
  }

  private def handleReceivedData(data: ByteString): Unit = {
    val msg = Message.decode(data.toArray);
    if (msg.isEmpty) println("Received unknown message of " + data.length + " bytes.")
    else msg.get match {
      case KeepAlive() => handleKeepAlive()
      case Choke() => handleChoke()
      case Unchoke() => handleUnchoke()
      case Interested() => handleInterested()
      case NotInterested() => handleNotInterested()
      case Have(index) => handleHave(index)
      case Bitfield(dp) => handleBitfield(dp)
      case Request(index, begin, length) => handleRequest(index, begin, length)
      case Piece(index, begin, block) => handlePiece(index, begin, block)
      case Cancel(index, begin, length) => handleCancel(index, begin, length)
      case Port(listenPort) => handlePort(listenPort)
    }
  }

  private def handleKeepAlive(): Unit = {
    println("Received keepalive")
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

  private def handleRequest(index: Int, begin: Int, length: Int): Unit = {
    println("Received request")
  }

  private def handlePiece(index: Int, begin: Int, block: Array[Byte]): Unit = {
    println("Received piece")
  }

  private def handleCancel(index: Int, begin: Int, length: Int): Unit = {
    println("Received cancel")
  }

  private def handlePort(listenPort: Int): Unit = {
    println("Received port")
  }
}
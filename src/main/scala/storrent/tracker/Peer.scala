package storrent.tracker

import java.net.InetAddress
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import storrent.bencode.{BencodeDict, BencodeInt, BencodeString, BencodeValue}

case class Peer(
  ip: String,
  port: Int,
  peerId: Option[String] = None
)

object Peer {
  def from(peers: String): List[Peer] =
    peers.getBytes(StandardCharsets.ISO_8859_1)
      .grouped(6)
      .map(p => Peer(
        ip = InetAddress.getByAddress(p.take(4)).toString.tail,
        port = ByteBuffer.wrap(p.drop(4)).getShort & 0xffff
      )).toList

  def from(peers: List[BencodeValue]): List[Peer] =
    peers.map {
      case BencodeDict(map) => decodePeerDict(map)
      case _ => throw TrackerException("Peers list should contain only dictionary values")
    }

  private def decodePeerDict(map: Map[BencodeString, BencodeValue]): Peer = {
    val ip = map.get(BencodeString("ip")) match {
      case Some(BencodeString(value)) => value
      case _ => throw TrackerException("Field 'ip' should be a string value")
    }
    val port = map.get(BencodeString("port")) match {
      case Some(BencodeInt(value)) => value
      case _ => throw TrackerException("Field 'port' should be an integer value")
    }
    val peerId = map.get(BencodeString("peer id")) match {
      case Some(BencodeString(value)) => Some(value)
      case _ => None
    }
    Peer(
      ip = ip,
      port = port.toInt,
      peerId = peerId
    )
  }
}
package storrent.peers

import java.nio.charset.StandardCharsets

import scala.util.{Failure, Success, Try}

case class Handshake(infoHash: Array[Byte], peerId: String)

object Handshake {

  def serialize(handshake: Handshake): Array[Byte] = {
    val protocolStr = "BitTorrent protocol".getBytes(StandardCharsets.ISO_8859_1)
    val protocolStrLen = Array[Byte](protocolStr.length.toByte)
    val reserved = Array.fill[Byte](8)(0)
    val peerId = handshake.peerId.getBytes(StandardCharsets.ISO_8859_1)
    Array.concat(protocolStrLen, protocolStr, reserved, handshake.infoHash, peerId)
  }

  def deserialize(payload: Array[Byte]): Try[Handshake] = {
    if (payload.length != 68) return Failure(PWPException("Invalid length for PWP handshake"))

    val protocolStrLen = payload(0)
    val protocolStr = new String(payload.slice(1, 1 + protocolStrLen), StandardCharsets.ISO_8859_1)
    if (protocolStr != "BitTorrent protocol") return Failure(PWPException("Invalid protocol for PWP handshake"))

    Success(Handshake(
      infoHash = payload.slice(payload.length - 40, payload.length - 20),
      peerId = new String(payload.slice(payload.length - 20, payload.length), StandardCharsets.ISO_8859_1)
    ))
  }
}

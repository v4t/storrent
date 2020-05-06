package storrent.peers

import java.nio.ByteOrder

import akka.util.ByteStringBuilder

case class Bitfield(downloadedPieces: Array[Boolean]) extends Message

object Bitfield {
  implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN
  val messageId: Byte = MessageId.BITFIELD
  val lengthPrefix: Int = 1

  def encode(downloadedPieces: Array[Boolean]): Array[Byte] = {
    val len = lengthPrefix + downloadedPieces.length
    val bitfieldAsBytes = downloadedPieces.sliding(8, 8).map(i => bitsToByte(i)).toArray
    val bsb = new ByteStringBuilder()
    bsb.putInt(lengthPrefix)
    bsb.putByte(messageId)
    bsb.putBytes(bitfieldAsBytes)
    bsb.result().toArray
  }

  def decode(bytes: Array[Byte]): Option[Bitfield] = {
    if (bytes.length <= 5 || bytes(4) != messageId) None
    else {
      val bitfield = bytes.drop(5).flatMap(byteToBits)
      Some(Bitfield(bitfield))
    }
  }

  private def byteToBits(byte: Byte): Array[Boolean] =
    (7 to 0 by -1).map(bit => ((byte >> bit) & 1) == 1).toArray

  private def bitsToByte(bits: Array[Boolean]): Byte =
    bits.foldLeft(0)((acc, bit) => if (bit) (acc << 1) | 1 else acc << 1).toByte
}
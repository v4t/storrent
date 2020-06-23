package storrent.peerprotocol

import java.nio.ByteOrder

import akka.util.ByteStringBuilder

/**
 * bitfield: <len=0001+X><id=5><bitfield>
 */
case class Bitfield(downloadedPieces: Array[Boolean]) extends Message

object Bitfield {
  implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN
  val messageId: Byte = Message.Id.BITFIELD
  val lengthPrefix: Int = 1

  def encode(downloadedPieces: Array[Boolean]): Array[Byte] = {
    val bitfieldByteCount = (downloadedPieces.length + 7) & (-8) // Round up to nearest multiple of 8
    val len = lengthPrefix + bitfieldByteCount
    val bitfieldAsBytes = downloadedPieces
      .padTo(bitfieldByteCount, false)
      .sliding(8, 8)
      .map(i => bitsToByte(i))
      .toArray
    val bsb = new ByteStringBuilder()
    bsb.putInt(len)
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
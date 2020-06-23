package storrent.peerprotocol

import java.nio.ByteOrder

import akka.util.ByteStringBuilder

/**
 * interested: <len=0001><id=2>
 */
case class Interested() extends Message

object Interested {
  implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN
  val messageId: Byte = Message.Id.INTERESTED
  val lengthPrefix: Int = 1

  def encode(): Array[Byte] = {
    val bsb = new ByteStringBuilder()
    bsb.putInt(lengthPrefix)
    bsb.putByte(messageId)
    bsb.result().toArray
  }

  def decode(bytes: Array[Byte]): Option[Interested] = {
    if (bytes.length != 5 || bytes(4) != messageId || bytes.slice(0, 4).last != lengthPrefix) None
    else Some(Interested())
  }
}
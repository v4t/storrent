package storrent.peers

import java.nio.ByteOrder

import akka.util.ByteStringBuilder

case class NotInterested() extends Message

object NotInterested {
  implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN
  val messageId: Byte = MessageId.NOT_INTERESTED
  val lengthPrefix: Int = 1

  def encode(): Array[Byte] = {
    val bsb = new ByteStringBuilder()
    bsb.putInt(lengthPrefix)
    bsb.putByte(messageId)
    bsb.result().toArray
  }

  def decode(bytes: Array[Byte]): Option[NotInterested] = {
    if (bytes.length != 5 || bytes(4) != messageId || bytes.slice(0, 4)(4) == lengthPrefix) None
    else Some(NotInterested())
  }
}
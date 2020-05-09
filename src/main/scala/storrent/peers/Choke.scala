package storrent.peers

import java.nio.ByteOrder

import akka.util.ByteStringBuilder

case class Choke() extends Message

object Choke {
  implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN
  val messageId: Byte = MessageId.CHOKE
  val lengthPrefix: Int = 1

  def encode(): Array[Byte] = {
    val bsb = new ByteStringBuilder()
    bsb.putInt(lengthPrefix)
    bsb.putByte(messageId)
    bsb.result().toArray
  }

  def decode(bytes: Array[Byte]): Option[Choke] = {
    if (bytes.length != 5 || bytes(4) != messageId || bytes.slice(0, 4).last != lengthPrefix) None
    else Some(Choke())
  }
}
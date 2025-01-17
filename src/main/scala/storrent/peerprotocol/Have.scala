package storrent.peerprotocol

import java.nio.{ByteBuffer, ByteOrder}

import akka.util.ByteStringBuilder

/**
 * have: <len=0005><id=4><piece index>
 */
case class Have(index: Int) extends Message

object Have {
  implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN
  val messageId: Byte = Message.Id.HAVE
  val lengthPrefix: Int = 5

  def encode(index: Int): Array[Byte] = {
    val bsb = new ByteStringBuilder()
    bsb.putInt(lengthPrefix)
    bsb.putByte(messageId)
    bsb.putInt(index)
    bsb.result().toArray
  }

  def decode(bytes: Array[Byte]): Option[Have] = {
    if (bytes.length != 9 || bytes(4) != messageId || bytes.slice(0, 4).last != lengthPrefix) None
    else {
      val index = ByteBuffer.wrap(bytes.slice(5,9)).getInt
      Some(Have(index))
    }
  }
}
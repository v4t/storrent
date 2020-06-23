package storrent.peerprotocol

import java.nio.{ByteBuffer, ByteOrder}

import akka.util.ByteStringBuilder

/**
 * <len=0003><id=9><listen-port>
 */
case class Port(listenPort: Int) extends Message

object Port {
  implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN
  val messageId: Byte = Message.Id.PORT
  val lengthPrefix: Int = 3

  def encode(listenPort: Int): Array[Byte] = {
    val bsb = new ByteStringBuilder()
    bsb.putInt(lengthPrefix)
    bsb.putByte(messageId)
    bsb.putShort(listenPort.toShort)
    bsb.result().toArray
  }

  def decode(bytes: Array[Byte]): Option[Port] = {
    if (bytes.length != 7 || bytes(4) != messageId || bytes.slice(0, 4).last != lengthPrefix) None
    else {
      val listenPort = ByteBuffer.wrap(bytes.slice(5,7)).getShort & 0xffff // Convert to int since shorts are signed
      Some(Port(listenPort))
    }
  }
}
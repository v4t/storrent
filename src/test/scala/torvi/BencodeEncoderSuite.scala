package torvi

import org.scalatest.FunSuite

class BencodeEncoderSuite extends FunSuite {

  test("BencodeValueString can be encoded into string literal") {
    val value = List(BencodeStringValue("test"))
    assert(BencodeEncoder.encode(value) == "4:test")
  }

}


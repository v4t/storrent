package torvi.bencode

import org.scalatest.FunSuite

class BencodeEncoderSuite extends FunSuite {

  test("BencodeStringValue are encoded correctly into bencode string") {
    val value = List(BencodeString("test"))
    assert(BencodeEncoder.encode(value) == "4:test")
  }

  test("Both empty and nonempty BencodeStringValues are encoded correctly into bencode strings") {
    val value = List(BencodeString(""), BencodeString("test"))
    assert(BencodeEncoder.encode(value) == "0:4:test")
  }

  test("BencodeIntValue are encoded correctly into bencode string") {
    val value = List(BencodeInt(2414))
    assert(BencodeEncoder.encode(value) == "i2414e")
  }

  test("Negative BencodeIntValues are encoded correctly into bencode string") {
    val value = List(BencodeInt(-500))
    assert(BencodeEncoder.encode(value) == "i-500e")
  }

  test("BencodeListValue containing just strings are encoded correctly") {
    val value = List(BencodeList(List(BencodeString("foo"), BencodeString("bar"))))
    assert(
      BencodeEncoder.encode(value) == "l3:foo3:bare"
    )
  }

  test("BencodeListValue with mixed strings and integers are encoded correctly") {
    val value = List(BencodeList(List(BencodeString("foo"), BencodeInt(2))))
    assert(
      BencodeEncoder.encode(value) == "l3:fooi2ee"
    )
  }

  test("BencodeListValue containing other lists are encoded correctly") {
    val value = List(BencodeList(List(
      BencodeList(List(BencodeString("foo"))),
      BencodeList(List(BencodeString("bar")))
    )))
    assert(BencodeEncoder.encode(value) == "ll3:fooel3:baree")
  }


  test("Empty BencodeListValue is encoded correctly") {
    val value = List(BencodeList(List()))
    assert(BencodeEncoder.encode(value) == "le")
  }


  test("BencodeDictValue with one elemenent is encoded correctly") {
    val value = List(BencodeDict(Map(BencodeString("foo") -> BencodeString("bar"))))
    assert(
      BencodeEncoder.encode(value) == "d3:foo3:bare"
    )
  }

  test("BencodeDictValue with mixed elements is encoded correctly") {
    val value = List(BencodeDict(Map(
      BencodeString("foo") -> BencodeString("bar"),
      BencodeString("int") -> BencodeInt(45)
    )))
    assert(
      BencodeEncoder.encode(value) == "d3:foo3:bar3:inti45ee"
    )
  }

  test("BencodeDictValue containing other dictionaries is encoded correctly") {
    val value = List(BencodeDict(Map(
      BencodeString("foo") -> BencodeDict(Map(
        BencodeString("bar") -> BencodeInt(99)))
    )))
    assert(
      BencodeEncoder.encode(value) == "d3:food3:bari99eee"
    )
  }

  test("Empty BencodeDictValue is encoded correctly") {
    val value = List(BencodeDict(Map()))
    assert(
      BencodeEncoder.encode(value) == "de"
    )
  }

  test("BencodeDictValueKeys are encoded in correct order") {
    val value = List(BencodeDict(Map(
      BencodeString("c") -> BencodeString("bar"),
      BencodeString("b") -> BencodeInt(45),
      BencodeString("a") -> BencodeString("foo")
    )))
    assert(
      BencodeEncoder.encode(value) == "d1:a3:foo1:bi45e1:c3:bare"
    )
  }
}


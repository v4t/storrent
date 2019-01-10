package torvi.Bencode

import org.scalatest.FunSuite

class BencodeEncoderSuite extends FunSuite {

  test("BencodeStringValue are encoded correctly into bencode string") {
    val value = List(BencodeStringValue("test"))
    assert(BencodeEncoder.encode(value) == "4:test")
  }

  test("Both empty and nonempty BencodeStringValues are encoded correctly into bencode strings") {
    val value = List(BencodeStringValue(""), BencodeStringValue("test"))
    assert(BencodeEncoder.encode(value) == "0:4:test")
  }

  test("BencodeIntValue are encoded correctly into bencode string") {
    val value = List(BencodeIntValue(2414))
    assert(BencodeEncoder.encode(value) == "i2414e")
  }

  test("Negative BencodeIntValues are encoded correctly into bencode string") {
    val value = List(BencodeIntValue(-500))
    assert(BencodeEncoder.encode(value) == "i-500e")
  }

  test("BencodeListValue containing just strings are encoded correctly") {
    val value = List(BencodeListValue(List(BencodeStringValue("foo"), BencodeStringValue("bar"))))
    assert(
      BencodeEncoder.encode(value) == "l3:foo3:bare"
    )
  }

  test("BencodeListValue with mixed strings and integers are encoded correctly") {
    val value = List(BencodeListValue(List(BencodeStringValue("foo"), BencodeIntValue(2))))
    assert(
      BencodeEncoder.encode(value) == "l3:fooi2ee"
    )
  }

  test("BencodeListValue containing other lists are encoded correctly") {
    val value = List(BencodeListValue(List(
      BencodeListValue(List(BencodeStringValue("foo"))),
      BencodeListValue(List(BencodeStringValue("bar")))
    )))
    assert(BencodeEncoder.encode(value) == "ll3:fooel3:baree")
  }


  test("Empty BencodeListValue is encoded correctly") {
    val value = List(BencodeListValue(List()))
    assert(BencodeEncoder.encode(value) == "le")
  }


  test("BencodeDictValue with one elemenent is encoded correctly") {
    val value = List(BencodeDictValue(Map(BencodeStringValue("foo") -> BencodeStringValue("bar"))))
    assert(
      BencodeEncoder.encode(value) == "d3:foo3:bare"
    )
  }

  test("BencodeDictValue with mixed elements is encoded correctly") {
    val value = List(BencodeDictValue(Map(
      BencodeStringValue("foo") -> BencodeStringValue("bar"),
      BencodeStringValue("int") -> BencodeIntValue(45)
    )))
    assert(
      BencodeEncoder.encode(value) == "d3:foo3:bar3:inti45ee"
    )
  }

  test("BencodeDictValue containing other dictionaries is encoded correctly") {
    val value = List(BencodeDictValue(Map(
      BencodeStringValue("foo") -> BencodeDictValue(Map(
        BencodeStringValue("bar") -> BencodeIntValue(99)))
    )))
    assert(
      BencodeEncoder.encode(value) == "d3:food3:bari99eee"
    )
  }

  test("Empty BencodeDictValue is encoded correctly") {
    val value = List(BencodeDictValue(Map()))
    assert(
      BencodeEncoder.encode(value) == "de"
    )
  }

  test("BencodeDictValueKeys are encoded in correct order") {
    val value = List(BencodeDictValue(Map(
      BencodeStringValue("c") -> BencodeStringValue("bar"),
      BencodeStringValue("b") -> BencodeIntValue(45),
      BencodeStringValue("a") -> BencodeStringValue("foo")
    )))
    assert(
      BencodeEncoder.encode(value) == "d1:a3:foo1:bi45e1:c3:bare"
    )
  }
}


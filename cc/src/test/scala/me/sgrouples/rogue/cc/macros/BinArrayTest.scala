package me.sgrouples.rogue.cc.macros
import munit.FunSuite
import me.sgrouples.rogue.cc.*

case class CArrays(bytes: Array[Byte], strings:Array[String])

class BinArrayTest extends FunSuite {
 class CMeta extends MCcMeta[CArrays, CMeta]("cm") {
    val bytes = ArrayField[Byte]("bytes")
    val strings = ArrayField[String]("strings")
  }
  test("Array[Byte] should have a dedicated Format"){
    val cm = new CMeta 
    val in = CArrays("ALAMA kota".getBytes, Array(" OLA"))
    val out = cm.read(cm.write(in))
    assert(cm.reader(cm.bytes).getClass == BinaryMacroBsonFormat.getClass())
    assert(out.strings.toList == in.strings.toList)
    assert(out.bytes.toList == in.bytes.toList)

  }
}

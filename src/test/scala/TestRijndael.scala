import org.mai.crypto_methods.Rijndael
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.{be, convertToAnyShouldWrapper, include}
import spire.math.UByte

class TestRijndael extends AnyFlatSpec with PrivateMethodTester {
  implicit def int2Ubyte(x: Int): UByte = UByte(x)
  def state: Array[UByte]= (0 until 16).map(UByte(_)).toArray
  def rijndael = new Rijndael(128)

  it should "end to end" in {
    val key     = "abcdefghijklmn"
    val message = "abcdefghijklmnopabcdefghijklmn"
    val encoded = rijndael.encode(message, key)
    val decoded = rijndael.decode(encoded, key)
    decoded should include(message)
  }

  it should "end to end 256" in {
    val key     = "abcdefghijklmn"
    val message = "abcdefghijklmnopabcdefghijklmn"
    val r = new Rijndael(256)
    val encoded = r.encode(message, key)
    val decoded = r.decode(encoded, key)
    decoded should include(message)
  }

  it should "encode" in {
    val trueEncoded: Array[UByte] = Array(169, 19, 41, 175, 153, 167, 141, 2, 174, 193, 124, 80, 119, 87, 170, 239)
    val encoded= rijndael.encode("abcdefghijklmnop", "abcdefghijklmnop")
    encoded should be(trueEncoded)
  }

  it should "decode" in {
    val trueEncoded: Array[UByte] = Array(169, 19, 41, 175, 153, 167, 141, 2, 174, 193, 124, 80, 119, 87, 170, 239)
    val decoded = rijndael.decode(trueEncoded, "abcdefghijklmnop")
    decoded should include("abcdefghijklmnop")
  }

  it should "mix column" in {
    val mixColumnState: Array[UByte] = Array(2, 7, 0, 5, 6, 3, 4, 1, 10, 15, 8, 13, 14, 11, 12, 9)
    val mixColumn                    = rijndael.mixColumn(state)
    mixColumn should be(mixColumnState)
  }

  it should "mix column inverse" in {
    val mixColumnState: Array[UByte] = Array(10, 15, 8, 13, 14, 11, 12, 9, 2, 7, 0, 5, 6, 3, 4, 1)
    val mixColumn                    = rijndael.mixColumn(state, inverse = true)
    mixColumn should be(mixColumnState)
  }

  it should "shift rows" in {
    val shiftRowState: Array[UByte] = Array(0, 5, 10, 15, 4, 9, 14, 3, 8, 13, 2, 7, 12, 1, 6, 11)
    val shiftRow                    = rijndael.shiftRow(state)
    shiftRow should be(shiftRowState)
  }

  it should "shift rows inverse" in {
    val shiftRowState: Array[UByte] = Array(0, 13, 10, 7, 4, 1, 14, 11, 8, 5, 2, 15, 12, 9, 6, 3)
    val shiftRow                    = rijndael.shiftRow(state, inverse = true)
    shiftRow should be(shiftRowState)
  }

  it should "byte sub" in {
    val byteSubState: Array[UByte] = Array(99, 124, 119, 123, 242, 107, 111, 197, 48, 1, 103, 43, 254, 215, 171, 118)
    val byteSub                    = rijndael.byteSub(state)
    byteSub should be(byteSubState)
  }

  it should "byte sub inverse" in {
    val byteSubState: Array[UByte] = Array(82, 9, 106, 213, 48, 54, 165, 56, 191, 64, 163, 158, 129, 243, 215, 251)
    val byteSub                    = rijndael.byteSub(state, inverse = true)
    byteSub should be(byteSubState)
  }

  it should "add round key" in {
    val key                       = (0 until 16).map(UByte(_)).toArray
    val resultState: Array[UByte] = Array.fill(key.length)(0)
    val stateKey                  = rijndael.addRoundKey(state, key)
    stateKey should be(resultState)
  }

  it should "key expansion" in {
    val key = "abcdefghijklmnop"
    val trueKey: Array[UByte] = Array(97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 255,
      202, 50, 88, 154, 172, 85, 48, 243, 198, 62, 92, 158, 168, 81, 44, 63, 27, 67, 83, 165, 183, 22, 99, 86, 113, 40,
      63, 200, 217, 121, 19, 14, 173, 62, 187, 171, 26, 40, 216, 253, 107, 0, 231, 53, 178, 121, 244, 49, 27, 129, 45,
      154, 1, 169, 245, 103, 106, 169, 18, 82, 216, 208, 230, 64, 107, 15, 45, 218, 106, 166, 216, 189, 0, 15, 202, 239,
      216, 223, 44, 1, 245, 126, 242, 219, 159, 216, 42, 102, 159, 215, 224, 137, 71, 8, 204, 225, 197, 53, 85, 58, 90,
      237, 127, 92, 197, 58, 159, 213, 130, 50, 83, 114, 230, 216, 86, 72, 188, 53, 41, 20, 121, 15, 182, 193, 251, 61,
      229, 102, 193, 1, 46, 46, 125, 52, 7, 58, 4, 59, 177, 251, 255, 6, 84, 70, 174, 33, 33, 104, 211, 21, 38, 82, 215,
      46, 151, 169, 40, 40, 195)

    val keySchedule = rijndael.keyExpansion(key)
    keySchedule should be(trueKey)
  }

  it should "string to bytes" in {
    val s                       = "abcdefghijklmnop"
    val trueBytes: Array[UByte] = (97 until 97 + s.length).map(UByte(_)).toArray

    val bytes = rijndael.stringToBytes(s)
    bytes should be(trueBytes)
  }

  it should "sbox" in {
    val trueSbox: Array[UByte] = Array(99, 124, 119, 123, 242, 107, 111, 197, 48, 1, 103, 43, 254, 215, 171, 118, 202,
      130, 201, 125, 250, 89, 71, 240, 173, 212, 162, 175, 156, 164, 114, 192, 183, 253, 147, 38, 54, 63, 247, 204, 52,
      165, 229, 241, 113, 216, 49, 21, 4, 199, 35, 195, 24, 150, 5, 154, 7, 18, 128, 226, 235, 39, 178, 117, 9, 131, 44,
      26, 27, 110, 90, 160, 82, 59, 214, 179, 41, 227, 47, 132, 83, 209, 0, 237, 32, 252, 177, 91, 106, 203, 190, 57,
      74, 76, 88, 207, 208, 239, 170, 251, 67, 77, 51, 133, 69, 249, 2, 127, 80, 60, 159, 168, 81, 163, 64, 143, 146,
      157, 56, 245, 188, 182, 218, 33, 16, 255, 243, 210, 205, 12, 19, 236, 95, 151, 68, 23, 196, 167, 126, 61, 100, 93,
      25, 115, 96, 129, 79, 220, 34, 42, 144, 136, 70, 238, 184, 20, 222, 94, 11, 219, 224, 50, 58, 10, 73, 6, 36, 92,
      194, 211, 172, 98, 145, 149, 228, 121, 231, 200, 55, 109, 141, 213, 78, 169, 108, 86, 244, 234, 101, 122, 174, 8,
      186, 120, 37, 46, 28, 166, 180, 198, 232, 221, 116, 31, 75, 189, 139, 138, 112, 62, 181, 102, 72, 3, 246, 14, 97,
      53, 87, 185, 134, 193, 29, 158, 225, 248, 152, 17, 105, 217, 142, 148, 155, 30, 135, 233, 206, 85, 40, 223, 140,
      161, 137, 13, 191, 230, 66, 104, 65, 153, 45, 15, 176, 84, 187, 22)
    val Rijndael = rijndael
    val initSbox = PrivateMethod[Array[UByte]]('initSbox)
    val sbox     = Rijndael.invokePrivate(initSbox())
    sbox should be(trueSbox)
  }

  it should "sbox inv" in {
    val sboxInvTrue: Array[UByte] = Array(82, 9, 106, 213, 48, 54, 165, 56, 191, 64, 163, 158, 129, 243, 215, 251, 124,
      227, 57, 130, 155, 47, 255, 135, 52, 142, 67, 68, 196, 222, 233, 203, 84, 123, 148, 50, 166, 194, 35, 61, 238, 76,
      149, 11, 66, 250, 195, 78, 8, 46, 161, 102, 40, 217, 36, 178, 118, 91, 162, 73, 109, 139, 209, 37, 114, 248, 246,
      100, 134, 104, 152, 22, 212, 164, 92, 204, 93, 101, 182, 146, 108, 112, 72, 80, 253, 237, 185, 218, 94, 21, 70,
      87, 167, 141, 157, 132, 144, 216, 171, 0, 140, 188, 211, 10, 247, 228, 88, 5, 184, 179, 69, 6, 208, 44, 30, 143,
      202, 63, 15, 2, 193, 175, 189, 3, 1, 19, 138, 107, 58, 145, 17, 65, 79, 103, 220, 234, 151, 242, 207, 206, 240,
      180, 230, 115, 150, 172, 116, 34, 231, 173, 53, 133, 226, 249, 55, 232, 28, 117, 223, 110, 71, 241, 26, 113, 29,
      41, 197, 137, 111, 183, 98, 14, 170, 24, 190, 27, 252, 86, 62, 75, 198, 210, 121, 32, 154, 219, 192, 254, 120,
      205, 90, 244, 31, 221, 168, 51, 136, 7, 199, 49, 177, 18, 16, 89, 39, 128, 236, 95, 96, 81, 127, 169, 25, 181, 74,
      13, 45, 229, 122, 159, 147, 201, 156, 239, 160, 224, 59, 77, 174, 42, 245, 176, 200, 235, 187, 60, 131, 83, 153,
      97, 23, 43, 4, 126, 186, 119, 214, 38, 225, 105, 20, 99, 85, 33, 12, 125)

    val Rijndael = rijndael
    val initSbox = PrivateMethod[Array[UByte]]('initSbox)
    val sbox     = Rijndael.invokePrivate(initSbox())

    val initSboxInv = PrivateMethod[Array[UByte]]('initSboxInv)
    val sboxInv     = Rijndael.invokePrivate(initSboxInv(sbox))

    sboxInv should be(sboxInvTrue)
  }

}


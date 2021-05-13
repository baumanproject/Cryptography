package org.mai.crypto_methods

import java.nio.charset.StandardCharsets
import java.security.InvalidParameterException

import BinaryTools.IntToUByte
import Gf256.Multiply
import spire.math.UByte


class Rijndael(val keySize: Int = 128) {
  if (keySize != 128 && keySize != 192 && keySize != 256)
    throw new InvalidParameterException()

  private val numRounds = if (keySize == 128) 11 else if (keySize == 192) 13 else 15
  private val keySizeBytes = keySize / 8 // 16 bytes
  private val numColumns = keySizeBytes / 4 // 4
  private val sbox = initSbox()
  private val sboxInv = initSboxInv(sbox)
  private val rcon = initRcon()

  private val mixColumnMatrix: Array[UByte] = Array(2, 3, 1, 1,
    1, 2, 3, 1,
    1, 1, 2, 3,
    3, 1, 1, 2)

  private val mixColumnInvMatrix: Array[UByte] = Array(14, 11, 13,  9,
    9, 14, 11, 13,
    13,  9, 14, 11,
    11, 13,  9, 14)

  private implicit def int2Ubyte(x: Int): UByte = UByte(x)

  def encode(input: String, key: String): Array[UByte] = {
    val inputBytes = stringToBytes(input)
    val keySchedule = keyExpansion(key)
    inputBytes.sliding(keySizeBytes, step = keySizeBytes)
              .flatMap(x => encodeBatch(x, keySchedule))
              .toArray
  }

  def decode(inputBytes: Array[UByte], key: String): String = {
    val keySchedule = keyExpansion(key)
    val bytes = inputBytes.sliding(keySizeBytes, step = keySizeBytes)
      .flatMap(x => decodeBatch(x, keySchedule))
      .toArray
    val decodedBytes = unpadBytes(bytes)
    bytesToString(decodedBytes).mkString("")
  }

  def encodeBatch(input: Array[UByte], key: Array[UByte]): Array[UByte] = {
    var state = addRoundKey(input, key.slice(0, keySizeBytes))
    for(i <- 1 until numRounds - 1) {
      state = byteSub(state)
      state = shiftRow(state)
      state = mixColumn(state)
      state = addRoundKey(state, key.slice(keySizeBytes * i, keySizeBytes * (i + 1)))
    }
    state = byteSub(state)
    state = shiftRow(state)
    state = addRoundKey(state, key.slice(keySizeBytes * (numRounds - 1), keySizeBytes * numRounds))
    state
  }

  def decodeBatch(input: Array[UByte], key: Array[UByte]): Array[UByte] = {
    var state = addRoundKey(input, key.slice(keySizeBytes * (numRounds - 1), keySizeBytes * numRounds))

    var i = numRounds - 1
    while (i > 1) {
      state = shiftRow(state, inverse = true)
      state = byteSub(state, inverse = true)
      state = addRoundKey(state, key.slice(keySizeBytes * (i - 1), keySizeBytes * i))
      state = mixColumn(state, inverse = true)
      i -= 1
    }

    state = shiftRow(state, inverse = true)
    state = byteSub(state, inverse = true)
    state = addRoundKey(state, key.slice(0, keySizeBytes))
    state
  }

  def addRoundKey(state: Array[UByte], key: Array[UByte]): Array[UByte] = {
    state.zip(key).map ({ case (x, y) => x ^ y })
  }

  def shiftRow(state: Array[UByte], inverse: Boolean = false): Array[UByte] = {

    val tempState = Array.ofDim[UByte](keySizeBytes)
    for (row <- 0 until 4) {
      for (col <- 0 until numColumns) {
        val newIndex = if (!inverse) index(row, pMod(col - row, numColumns)) else index(row, pMod(col + row, numColumns))
        tempState(newIndex) = state(index(row, col))
      }
    }
    tempState
  }

  def byteSub(state: Array[UByte], inverse: Boolean = false): Array[UByte] = {
    val currentSbox = if (inverse) sboxInv else sbox
    state.map(x => currentSbox(x.toInt))
  }

  def mixColumn(state: Array[UByte], inverse: Boolean = false): Array[UByte] = {
    val stateCopy = Array.fill[UByte](state.length)(0)
    val matrix = if (inverse) mixColumnInvMatrix else mixColumnMatrix

    for (r <- 0 until numColumns) {
      for (c <- 0 until 4) {
        for (o <- 0 until 4) {
          stateCopy(index(c, r)) = stateCopy(index(c, r)) ^ Multiply(matrix(c * 4 + o), state(index(o, r)))
        }
      }
    }
    stateCopy
  }

  def keyExpansion(key: String): Array[UByte] = {
    val keySymbols = stringToBytes(key)

    val keySchedule: Array[Array[UByte]] = Array.ofDim[UByte](numRounds * 4, numColumns) // 44 * 4 = 176 for 128
    for (i <- 0 until 4) {
      for (j <- 0 until numColumns) {
        keySchedule(i)(j) = keySymbols(i * 4 + j)
      }
    }

    for (i <- numColumns until 4 * numRounds) { // from 4 to 43
      var temp: Array[UByte] = new Array[UByte](numColumns)
      keySchedule(i - 1).copyToArray(temp)
      if (i % numColumns == 0) {
        temp = xorWord(subWord(rotWord(temp)), rcon(i / numColumns - 1))
      } else if (i % numColumns == 0 && i > 6) {
        temp = subWord(temp)
      }
      keySchedule(i) = xorWord(temp, keySchedule(i - numColumns))
    }

    keySchedule.flatten
  }

  def stringToBytes(input: String): Array[UByte] = {
    val inputBytes = input.getBytes(StandardCharsets.UTF_8).map(UByte(_))
    val paddedLength = (math.ceil((inputBytes.length + 1).toDouble / keySizeBytes) * keySizeBytes).toInt
    val padSize = paddedLength - inputBytes.length - 1
    inputBytes ++ Array.fill[UByte](padSize)(32.b) :+ padSize.b // pad array with 0 to paddedLength
  }

  def bytesToString(input: Array[UByte]): String = {
    new String(input.map(_.toByte), StandardCharsets.US_ASCII)
   }

  def unpadBytes(input: Array[UByte]): Array[UByte] = {
    val paddedLength = input.last
    input.dropRight((paddedLength + 1).toInt)
  }

  private def index(row: Int, col: Int): Int = col * numColumns + row

  private def pMod(num: Int, m: Int): Int = {
    val x = num % m
    if (x < 0) x + m else x
  }


  private def xorWord(word: Array[UByte], other: Array[UByte]): Array[UByte] = {
    val result = new Array[UByte](word.length)
    for (i <- word.indices) {
      result(i) = word(i) ^ other(i)
    }
    result
  }

  private def rotWord(word: Array[UByte]): Array[UByte] = {
    val firstValue = word(0)
    for (i <- 1 until word.length) {
      word(i - 1) = word(i)
    }
    word(word.length - 1) = firstValue
    word
  }

  private def subWord(word: Array[UByte]): Array[UByte] = {
    for (i <- word.indices) {
      word(i) = sbox(word(i).toInt)
    }
    word
  }

  private def rotateLeft8(x: UByte, shift: Int) = (x << shift) | (x >> (8 - shift))

  private def initSbox(): Array[UByte] =  {
    var p = 1.b
    var q = 1.b

    val sbox = new Array[UByte](256)
    /* loop invariant: p * q == 1 in the Galois field */
    do {
      /* multiply p by 3 */
      p = p ^ (p << 1) ^ (if ((p & 0x80.b) != 0.b) 0x11B.b else 0.b)

      /* divide q by 3 (equals multiplication by 0xf6) */
      q ^= q << 1
      q ^= q << 2
      q ^= q << 4
      q = q ^ (if ((q & 0x80.b) != 0.b) 0x09.b else 0.b)

      /* compute the affine transformation */
      val xformed = q ^ rotateLeft8(q, 1) ^ rotateLeft8(q, 2) ^
        rotateLeft8(q, 3) ^ rotateLeft8(q, 4)
      val res = xformed ^ 0x63.b
      sbox(p.toInt) = res
    } while (p != 1.b);

    /* 0 is a special case since it has no inverse */
    sbox(0) = 0x63.b
    sbox
  }

  private def initSboxInv(sbox: Array[UByte]): Array[UByte] = {
//    sbox.zipWithIndex.map ({ case (elem, index) => sbox(index)})
    val sboxInv = new Array[UByte](sbox.length)
    for (i <- sbox.indices) {
      sboxInv(sbox(i).toInt) = i.b
    }
    sboxInv
  }

  private def initRcon(): Array[Array[UByte]] = {
    val rcon = Array.fill[UByte](numRounds - 1, 4)(0)
    val initValues: Array[UByte] = Array(0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36,
      0x6c, 0xd8, 0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef, 0xc5)
    for (i <- 0 until numRounds - 1) {
      rcon(i)(0) = initValues(i)
    }
    rcon
  }

}


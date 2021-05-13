package org.mai.crypto_methods

import spire.math.{UByte, UInt}

import scala.annotation.tailrec

object BinaryTools {
  def getN(a: Int, k: Int): Int = (a >> k) & 1

  def set(a: Int, pos: Int, value: Int): UInt = (-UInt(value) ^ UInt(a)) & (UInt(1) << pos) ^ UInt(a)

  def toggle(a: Int, k: Int): Int = a ^ (1 << k)

  def swap(a: Int, i: Int, j: Int): Int = {
    val x = ((a >> i) ^ (a >> j)) & 1
    a ^ ((x << i) | (x << j))
  }

  def zeroK(a: Int, k: Int): Int = (a >> k) << k

  def mergeBits(a: UInt, i: Int): UInt =
    ((a >> (intSize() - i)) << i) ^ ((a << (intSize() - i)) >> (intSize() - i))

  def getCentral(a: UInt, i: Int): UInt =
    (a << i) >> (i * 2)

  def score(a: UInt): UInt = {
    var i         = intSize() / 2
    var num: UInt = a
    while (i > 0) {
      num = num ^ (num >> i)
      i /= 2
    }
    num & UInt(1)
  }

  def shiftLeft(a: UInt, count: Int): UInt =
    (a << count) | (a >> (intSize() - count))

  def shiftRight(a: UInt, count: Int): UInt =
    (a >> count) | (a << (intSize() - count))

  def intSize(): Int = toBinary(UInt.MaxValue).length // 32

  implicit class IntToBase(val digits: String) extends AnyVal {
    def base(b: Int) = Integer.parseInt(digits, b)

    def b = base(2)

    def o = base(8)

    def x = base(16)
  }

  implicit class IntToUByte(val digits: Int) extends AnyVal {
    def b = UByte(digits)
  }




  @tailrec
  def toBinaryOld(n: UInt, bin: List[UInt] = List.empty[UInt]): String = {
    if (n / UInt(2) == UInt(1)) (1 :: (n % UInt(2)) :: bin).mkString("")
    else {
      val r = n % UInt(2)
      val q = n / UInt(2)
      toBinaryOld(q, r :: bin)
    }
  }

  def toBinary(n: UInt): String = {
    Integer.toBinaryString(n.toInt)
  }

  def fastPow(a: BigInt, b: BigInt, modulo: BigInt): BigInt = {
      var r = BigInt(1)
      var a_ = a
      var b_ = b
      while (b_ > 0) {
        if ((b_ & 1) == 1) {
          r = (r * a_) % modulo
        }
        a_ = (a_ * a_) % modulo
        b_ = b_ >> 1
      }
      r
    }


}

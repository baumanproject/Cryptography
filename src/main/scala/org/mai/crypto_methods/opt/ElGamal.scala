package org.mai.crypto_methods.opt

import org.mai.crypto_methods.BinaryTools.fastPow

import scala.util.Random

case class ElGamalPublicKey(p: BigInt, a: BigInt, y: BigInt)

class ElGamal (val keySize: Int) {
  private val rnd = new Random()

  val (publicKey, privateKey) = getKey(keySize) // увеличить чтобы работало для бОльших чисел

  private def getKey(size: Int) = {
    val p = getP(size)
    val a = getG(p)
    val x = randomBigInt(1, p - 2)
    val y = fastPow(a, x, p)
    (ElGamalPublicKey(p, a, y), x)
  }
  private def getP(size: Int): BigInt = {
    while (true) {
      val p = BigInt.probablePrime(size, rnd) * 2 + 1
      if (p.isProbablePrime(10))
        return p
    }
    0
  }

  private def getG(p: BigInt): BigInt = {
    while (true) {
      val alpha = randomBigInt(2, p - 1)
      if ((p - 1) % alpha != 1) {
        return alpha
      }
    }
    0
  }

  private def randomBigInt(min: BigInt, max: BigInt): BigInt = {
    val bigInteger = max - min
    val res        = BigInt(max.bitLength, rnd)
    if (res.compareTo(min) < 0) return res + min
    if (res.compareTo(bigInteger) >= 0) return (res % bigInteger) + min
    res
  }


  def encrypt(message: BigInt): (BigInt, BigInt) = {
    val k = randomBigInt(1, publicKey.p - 2)
    val c1 = fastPow(publicKey.a, k, publicKey.p)
    val c2 = message * fastPow(publicKey.y, k, publicKey.p)
    (c1, c2)
  }

  def decrypt(c1: BigInt, c2: BigInt): BigInt = {
    (c1.pow((publicKey.p - 1 - privateKey).toInt) * c2) % publicKey.p
  }

}

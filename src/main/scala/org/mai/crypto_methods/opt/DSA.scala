package org.mai.crypto_methods.opt

import org.mai.crypto_methods.BinaryTools.fastPow
//2import com.sun.tools.corba.se.idl.InvalidArgument

import scala.util.Random

class DSA { // (val p: BigInt, val q: BigInt, val g: BigInt, val x: BigInt) // (val p: BigInt, val q: BigInt)
  private val rnd     = new Random()
  private val keySize = 256
  private val l       = 1024

//  val q = BigInt.probablePrime(keySize, rnd)
//  val p = getP(q)
  val (p, q) = getPandQ()
  val g = getG(p, q)             // 60
  val x = randomBigInt(2, q - 1) // 24
  val y = fastPow(g, x, p)       // 158

  def sign(message: String): (BigInt, BigInt) = {
    val messageHash = sha256(message)
//    val messageHash = 41
    var (k, r, s) = (BigInt(0), BigInt(0), BigInt(0))
    do {
      do {
        k = randomBigInt(2, q - 1)
//        k = 15
        r = fastPow(g, k, p) % q
      } while (r == 0)
      s = (k.modInverse(q) * (messageHash + x * r)) % q
    } while (s == 0)
    (r, s)
  }

  def verify(message: String, r: BigInt, s: BigInt): Boolean = {
    if (r < 0 || r > q || s < 0 || s > p) throw new IllegalArgumentException()
    val messageHash = sha256(message)
//    val messageHash = 41
    val w  = s.modInverse(q)
    val u1 = (messageHash * w) % q
    val u2 = (r * w) % q
    val v  = ((fastPow(g, u1, p) * fastPow(y, u2, p)) % p) % q
    v == r
  }

  def sha256Hash(text: String): String = String.format("%064x", sha256(text))

  def sha256(text: String): BigInt =
    new java.math.BigInteger(1,
                             java.security.MessageDigest
                               .getInstance("SHA-256")
                               .digest(text.getBytes("UTF-8")))

  private def getPandQ(): (BigInt, BigInt) = {
    var (q, k, p) = (BigInt(0), BigInt(0), BigInt(0))
    do {
      q = BigInt.probablePrime(keySize, rnd)
      k = randomBigInt(BigInt(2).pow(l - keySize - 1), BigInt(2).pow(l - keySize))
      p = q * k + 1
    } while (!p.isProbablePrime(10) || p.bitLength != l)
    (p, q)
  }

  private def getP(q: BigInt): BigInt = {
    var p = BigInt.probablePrime(l, rnd)
    while ((p - 1) % q != 0) {
      p = BigInt.probablePrime(l, rnd)
    }
    p
  }

  private def getG(p: BigInt, q: BigInt): BigInt = {
    var h = BigInt(2)
    var g = fastPow(h, (p - 1) / q, p)
    while (g == 1) {
      h = randomBigInt(2, p - 1)
      g = fastPow(h, (p - 1) / q, p)
    }
    g
  }

  private def randomBigInt(min: BigInt, max: BigInt): BigInt = {
    val bigInteger = max - min
    val res        = BigInt(max.bitLength, rnd)
    if (res.compareTo(min) < 0) return res + min
    if (res.compareTo(bigInteger) >= 0) return (res % bigInteger) + min
    res
  }

}

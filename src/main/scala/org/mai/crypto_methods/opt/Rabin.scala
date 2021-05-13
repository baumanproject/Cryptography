package org.mai.crypto_methods.opt

import org.mai.crypto_methods.BinaryTools.fastPow
import org.mai.crypto_methods.Euclidean

import scala.annotation.tailrec
import scala.util.Random

case class RabinPrivateKey(p: BigInt, q: BigInt)

class Rabin (val keySize: Int) {
  private val rnd = new Random()

  var (publicKey, privateKey) = getKey(keySize) // увеличить чтобы работало для бОльших чисел

  def encrypt(message: BigInt): BigInt = {
    message.pow(2) % publicKey
  }

  def decrypt(cipher: BigInt): (BigInt, BigInt, BigInt, BigInt) = {
    val mp = fastPow(cipher, (privateKey.p + 1) / 4, privateKey.p)
    val mq = fastPow(cipher, (privateKey.q + 1) / 4, privateKey.q)
    val (_, yp, yq) = Euclidean.gcdexBig(privateKey.p, privateKey.q)
    val r = positiveMod(yp * privateKey.p * mq + yq * privateKey.q * mp, publicKey)
    val s = positiveMod(yp * privateKey.p * mq - yq * privateKey.q * mp, publicKey)
    (r, publicKey - r, s, publicKey - s)
  }

  private def getKey(size: Int) = {
    var p = BigInt.probablePrime(size, rnd)
    var q = findNextPrime(p + 1)
    while (p%4 != 3 || q%4 !=3) {
      p = BigInt.probablePrime(size, rnd)
      q = findNextPrime(p + 1)
    }
    (p * q, RabinPrivateKey(p, q))
  }

  private def findNextPrime(n: BigInt) : BigInt = {
    @tailrec
    def iterate(m: BigInt) : BigInt ={
      if(m.isProbablePrime(10)) m
      else iterate(m + 1)
    }
    iterate(n)
  }

  private def positiveMod(number: BigInt, modulo: BigInt): BigInt = {
    val v = number % modulo
    if (v < 0) return v + modulo
    v
  }

}

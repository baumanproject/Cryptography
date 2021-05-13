package org.mai.crypto_methods

import BinaryTools.fastPow

import scala.BigInt._

case class PublicKey(n: BigInt, e: BigInt)
case class PrivateKey(n: BigInt, d: BigInt)

class RSA() {
  var p: BigInt = generatePrime()
  var q: BigInt = generatePrime()

  while(p==q) {
    p = generatePrime()
    q = generatePrime()
  }

  val phiN: BigInt = (p - BigInt(1)) * (q - BigInt(1))
  val e: BigInt = getExp
  val d: BigInt = e.modInverse(phiN)

  val publicKey: PublicKey = PublicKey(p * q, e)
  val privateKey: PrivateKey  = PrivateKey(p * q, d)


  private def getExp: BigInt = {
    var e = 3
    while (phiN.gcd(e) > 1) e = e + 2
    e
  }

  private def generatePrime(): BigInt = {
    val sieve = new SieveOfEratosthenes(math.pow(2, 15).toInt)
    sieve.getRandomPrime
  }

  def encrypt(message: BigInt, pk: PublicKey) =
    fastPow(message, pk.e, pk.n)

  def decrypt(cypher: BigInt, sk: PrivateKey) =
    fastPow(cypher, sk.d, sk.n)

}

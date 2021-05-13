package org.mai.crypto_methods

import scala.annotation.tailrec

object Euclidean {
  @tailrec
  def gcd(x: Int, y: Int): Int = (x, y) match {//euclid
    case (x, 0) => x
    case (x, y) if y > x => gcd(y, x)
    case _ => gcd(y, x % y)
  }

  def gcdex(a: Int, b: Int): (Int, Int, Int) = {//ax+by = nod(a,b)
    if (b == 0) {
      return (a, 1, 0)
    }
    val (d, x, y) = gcdex(b, a % b)
    (d, y, x - y * (a / b))
  }

  def gcdexBig(a: BigInt, b: BigInt): (BigInt, BigInt, BigInt) = {
    if (b == 0) {
      return (a, 1, 0)
    }
    val (d, x, y) = gcdexBig(b, a % b)
    (d, y, x - y * (a / b))
  }

}

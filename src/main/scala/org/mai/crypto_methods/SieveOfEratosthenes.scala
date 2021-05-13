package org.mai.crypto_methods

import scala.util.Random

class SieveOfEratosthenes(val n: Int) {

  def sieve(stream: LazyList[Int]): LazyList[Int] = stream.head #:: sieve(stream.tail.filter(x => x % stream.head != 0))

  def getPrimeNumbers: Array[Int] =
    sieve(LazyList.from(2)).takeWhile(_ < n).toArray

  def getRandomPrime: Int = {
    val r = new Random()
    /*val odds = LazyList.from(3, 2).takeWhile(_ <= Math.sqrt(n).toInt)
    val composites = odds.flatMap(i => LazyList.from(i * i, 2 * i).takeWhile(_ <= n))
    val primesStream = LazyList.from(3, step=2).takeWhile(_ <= n).diff(composites).dropWhile(_ < from)
    val randomIndex = r.nextInt(primesStream.size)
    primesStream.slice(randomIndex, randomIndex + 1).head
*/

    val arr = getPrimeNumbers
    val randomIndex = r.nextInt(arr.size)
    arr(randomIndex)
  }
}

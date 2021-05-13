package org.mai.crypto_methods.opt

case class Fraction(p: Int, q: Int)

class WienersAttack {
  def attack(e: BigInt, n: BigInt, maxIters: Int = 100): Int = {
    for (frac <- suitableFractions(e.toDouble / n.toDouble).take(maxIters)) {
      if (frac.p != 0) {
        val phiN = (e * frac.q - 1) / frac.p
        val d = sqrt(((n - phiN) + 1).pow(2) - 4 * n)
        val pn = (((n - phiN) + 1) + d) / 2
        val qn = (((n - phiN) + 1) - d) / 2
        if (pn * qn == n) return frac.q
      }
    }
    0
  }

  def getContinuedFractions(a: Double): Stream[Int] = {
    def nextItem(x: Double): (Int, Double) = {
      val a1 = (1/x).toInt
      (a1, 1 / x - a1)
    }
    lazy val numbers: Stream[(Int, Double)] = Stream((a.toInt, a - a.toInt)) #::: numbers.map({ case (a, x) => nextItem(x) })
    numbers.map(_._1)
  }

  def suitableFractions(a: Double): Stream[Fraction] = {
    val continuedFractions = getContinuedFractions(a)//.drop(1) // drop 0
    val init = Stream(Fraction(0, 1), Fraction(1, 0))
    def nextItem(a: Int, fraction: (Fraction, Fraction)): Fraction = {
      val (f_2, f_1) = fraction
      Fraction(a * f_1.p + f_2.p, a * f_1.q + f_2.q)
    }
    lazy val numbers: Stream[Fraction] = init #::: numbers.zip(numbers.tail)
                        .zip(continuedFractions)
                        .map{n => nextItem(n._2, n._1)}
    numbers.drop(2) // drop init values (0,1) and (1,0)
  }

  private def sqrt(number : BigInt) = {
    def next(n : BigInt, i : BigInt) : BigInt = (n + i/n) >> 1

    val one = BigInt(1)

    var n = one
    var n1 = next(n, number)

    while ((n1 - n).abs > one) {
      n = n1
      n1 = next(n, number)
    }

    while (n1 * n1 > number) {
      n1 -= one
    }

    n1
  }


}

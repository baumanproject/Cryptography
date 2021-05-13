package org.mai.crypto_methods

import org.mai.crypto_methods.Polynomial.{extendedEuclideanAlgorithmPolynom, multiplyPolynom, percentagePolynom, subtr}
import spire.math.UByte

object Gf256 {

  private val IrreduciblePolynomial = 0x11b
  /*public GaloisField(uint m = 0x11b)
  {
    IrreduciblePolynomial = m;
  }*/

  def toPolinomial(number: String): String = {
    number.reverse
          .zipWithIndex
          .filter(_._1 == '1')
          .map(x => x._2 match {
            case 0 => "1"
            case 1 => "x"
            case _ => s"x^${x._2}"
          })
          .reverse
          .mkString("", " + ", "")
  }



  def Multiply(a: UByte, b: UByte): UByte = {
    var p: UByte = UByte(0)
    var a_ = a
    var b_ = b
    for (_ <- 0 until 8) {
      if ((b_ & UByte(1)) != UByte(0)) {
        p = p ^ a_
      }
      val hi_bit_set = (a_ & UByte(0x80)) != UByte(0)
      a_ = a_ <<  1
      if (hi_bit_set) {
        a_ = a_ ^ UByte(0x1B)
      } /* x^8 + x^4 + x^3 + x + 1 */
      b_ = b_ >> 1
    }
    p
  }

  def Pow(number: UByte, exponent: Int): UByte = {
    var result = UByte(1)
    var exp_temp = UByte(exponent)
    while (exp_temp != UByte(0)) {
      if ((exp_temp.toInt & 1) == 1){
        result = Multiply(result, number)
      }
      result = Multiply(result, result)
      exp_temp >>= 1
    }
    result
  }


  def MultiplicativeInverseUsingExponentiation(number: UByte):UByte = {Pow(number, 254)}

  def DivideGalua (a: UByte, b: UByte): UByte = {
      val firstNumberPolynomial = a
      val secondNumberPolynomial = b
      val secondNumberPolynomialInverse = MultiplicativeInverseUsingExponentiation(secondNumberPolynomial)

      val multiplicationResult = (multiplyPolynom(firstNumberPolynomial.toInt,secondNumberPolynomialInverse.toInt))
      val multiplicationByModule = percentagePolynom(multiplicationResult.toInt,IrreduciblePolynomial)

      UByte(multiplicationByModule)
    }



  def ExtendedEuclidianAlgorithmGF(a:UByte, b:UByte): (UByte, UByte, UByte) = {
    if (b.toInt == 1)
    {
      return (a, UByte(1), UByte(0))
    }

    var x1 = 1
    var y1 = 0
    var x2 = 0
    var y2 = 1

    var b_t = b
    var a_t = a

    while (b_t.toInt > 0)
    {
      val q = DivideGalua(a_t,b_t)



      var t = b_t
      // b_t = a_t % b_t;
      b_t = UByte(percentagePolynom(a_t.toInt, b_t.toInt))

      a_t = t

      t = UByte(x2)
      x2 = subtr(x1.toInt,Multiply(q,UByte(x2)).toInt)
      x1 = t.toInt

      t = UByte(y2)
      y2 = subtr(y1,Multiply(q, UByte(y2)).toInt)
      y1 = t.toInt
    }

     (a_t, UByte(x1), UByte(y1))
  }

  def MultiplicativeInverse(a :UByte, mode :Int): UByte = {
    if (mode == 0) {
       MultiplicativeInverseUsingExponentiation(a)
    }
    else {
      val (_, inverse,_) = extendedEuclideanAlgorithmPolynom(a.toInt, IrreduciblePolynomial)
      UByte(inverse)
    }
  }
}
//10001
//Result of inverse mode: 10110100


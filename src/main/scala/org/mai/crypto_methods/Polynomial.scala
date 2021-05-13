package org.mai.crypto_methods

import org.mai.crypto_methods.BinaryTools.getN
import spire.math.UByte

object Polynomial {

  def BitLength(a:Int) :Int = {
      var countBits = 0
      var numberCopy = a
      while ( numberCopy > 0) {
        numberCopy >>= 1
        countBits += 1
      }
      countBits
  }

  def subtr(a:Int, b: Int): Int ={
    a^b
  }
  def devidePolinom(polynomial: Int, divisorPolynomial:Int): (Int, Int) = {
    var result = 0
    var number = polynomial
    var divisor = divisorPolynomial

    while(BitLength(number.toInt) >= BitLength(divisor)){
      var shift = BitLength(number.toInt) - BitLength(divisor)
      number ^= divisor << shift
      result ^= 1 << shift
    }
    (result,number)
  }

  def percentagePolynom(polynomial: Int, divisorPolynomial: Int): Int = {
    if (divisorPolynomial == 0)
      throw new ArithmeticException("Divisor polynomial must be not 0");


    (polynomial, divisorPolynomial) match
    {
      case x if(x._1.toInt == 0)=> None
      case x if(x._1.toInt == 1 & x._2.toInt == 1 )=> return 0
      case x if (x._1.toInt) == 1 => return 1
      case _ => None
    }

    val (_, result) = devidePolinom(polynomial, divisorPolynomial)
    return result
  }


  def multiplyPolynom(a :Int, b: Int): Int = {
    var result = 0
    var firstNumberBits = a
    var secondNumberBits = b
    for ( i <- 0 to BitLength(firstNumberBits)){
      for(j <- 0 to BitLength(secondNumberBits)){
        var shift = i+j
        result ^= (getN(firstNumberBits.toInt,i) & getN(secondNumberBits.toInt, j))<< shift
      }
    }
    result
  }

  def extendedEuclideanAlgorithmPolynom(a:Int, b:Int): (Int, Int, Int) = {
    if (b.toInt == 1)
    {
      return (a, 1, 0)
    }

    var x1 = 1
    var y1 = 0
    var x2 = 0
    var y2 = 1

    var b_t = b
    var a_t = a

    while (b_t.toInt > 0)
    {
      val (q,_) = devidePolinom(a_t,b_t)



      var t = b_t
      // b_t = a_t % b_t;
      b_t = percentagePolynom(a_t, b_t)

      a_t = t

      t = x2
      x2 = subtr(x1,multiplyPolynom(q,x2))
      x1 = t

      t = y2
      y2 = subtr(y1,multiplyPolynom(q, y2))
      y1 = t
    }

    (a_t, x1, y1)
  }


}

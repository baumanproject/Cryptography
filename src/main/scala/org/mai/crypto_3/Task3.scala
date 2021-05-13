package org.mai.crypto_3
import org.mai.crypto_methods.BinaryTools.{IntToBase, toBinary}
import org.mai.crypto_methods.ConsoleTools.greeting
import org.mai.crypto_methods.Gf256.{DivideGalua, ExtendedEuclidianAlgorithmGF, MultiplicativeInverse, Multiply, toPolinomial}
import spire.math.{UByte, UInt}

import scala.io.StdIn.{readChar, readLine}


object Task3 {
  def main(args: Array[String]): Unit = {
    val number = greeting()
    number match {
      case 1 => task1()
      case 2 => task2()
      case 3 => task3()
      case 4 => task4()
      case _ => println("Invalid task number")
    }
  }



  def task1(): Unit = {
    println("GF(256)")
    println("Input operation name")
    println("p - get polynomial form of GF(256) element ")
    println("m - multiply two GF(256) polynomials ")
    println("d - divide ")
    val op = readChar()

    if (op == 'p') {
      println("Input GF(256) element in binary format")
      val number = readLine()
      println(toPolinomial(number))
    } else if (op == 'm') {
      println("Input two numbers in binary format")
      val a = readLine().b
      val b = readLine().b
      val result = Multiply(UByte(a), UByte(b))
      println(s"result of multiplication = ${toBinary(UInt(result.toInt))}")
    }
    else if(op == 'd'){
      println("Input two numbers in binary format")
      val a = readLine().b
      val b = readLine().b
      val result = DivideGalua(UByte(a), UByte(b))
      println(s"result of division = ${toBinary(UInt(result.toInt))}")
    }
    else {
      println("Unsupported operation")
    }
    //10001
    //11111
    /*
    10110110 * 1010011 = 110110
    0xb6 * 0x53 = 0x36
     */
  }

  def task2(): Unit ={
    println("Extended Euclidean for GF")
    println("Input two numbers in binary format")
    val a = readLine().b
    val b = readLine().b
    //val c = UByte(b)
    val (a_r, b_r, c_r) = ExtendedEuclidianAlgorithmGF(UByte(a),UByte(b))
    //val m = UInt(b_r.toInt)
    //print(UInt(1) / UInt(2))
    //val r = toBinary(UInt(2))
    //val k = toBinary(UInt(5))

    //println((5))
    //println(k)
    //val t = toBinary(UInt(1))
    //println(s"Result of Euclidean extended for GF")
    //println(s"result a = ${t}")
   // println(s"b = ${toBinary(UInt(b_r.toInt))}")
    //println(s"c = ${toBinary(UInt(c_r.toInt))}")
    println(s"Result of Euclidean extended for GF: a = ${toBinary(UInt(a_r.toInt))}, b = ${toBinary(UInt(b_r.toInt))}, c = ${toBinary(UInt(c_r.toInt))}")
  }

  def task3(): Unit={
    println("Multiplicative inverse")
    println("Input multiplicative mode: i - Inverse, e - Euclidean")
    val op = readChar()

    if (op == 'e') {
      println("Input one number in binary format")
      val a = readLine().b
      val result =  MultiplicativeInverse(UByte(a), 1)
      println(s"Result of euclidean mode: ${toBinary(UInt(result.toInt))} ")
    }
    else if (op == 'i'){
      println("Input one number in binary format")
      val a = readLine().b
      val result =  MultiplicativeInverse(UByte(a), 0)
      println(s"Result of inverse mode: ${toBinary(UInt(result.toInt))} ")
    }
    else {
      println("Wrong argument")
    }

    //1001
    //Result of inverse mode: 1001111
  }

  def task4():Unit={
    println("")
  }
}


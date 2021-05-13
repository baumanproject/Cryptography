package org.mai.crypto_4

import org.mai.crypto_methods.Rijndael
import org.mai.crypto_methods.ConsoleTools.greeting

import scala.io.StdIn.{readLine}

object Task4 {
  def main(args: Array[String]): Unit = {

    val number = greeting()
    number match {
      case 1 => task1()
      case _ => println("Invalid task number")
    }
  }

  def task1(): Unit = {
    println("Rijndael")
    println("Input text")
    val message = readLine()
    println("Input key")
    val key = readLine()
    val aes = new Rijndael()
    val encoded = aes.encode(message, key)
    println(s"Encrypted text = ${aes.bytesToString(encoded)}")
    val decoded = aes.decode(encoded, key)
    println(s"Decrypted text = $decoded")

  }

}


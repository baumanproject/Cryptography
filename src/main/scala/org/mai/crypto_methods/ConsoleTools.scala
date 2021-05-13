package org.mai.crypto_methods

import scala.io.StdIn.readInt

object ConsoleTools {

  def greeting(): Int = {
    println("Input task number:")
    val number = readInt()
    //println("Note: all input numbers must be in decimal notation\n")
    number
  }

}

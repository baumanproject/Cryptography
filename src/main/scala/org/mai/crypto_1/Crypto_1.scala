package org.mai.crypto_1

//import org.mai.crypto.Tasks

object Crypto_1 {

  def main(args:Array[String]):Unit={

    try{
      if(args.length <2){
        throw new Exception("Failed")
        println(args.length)
      }

    }
    catch{
      case _: Exception => println("Invalid argument. need string got empty array")
    }

    //val point = new Tasks(args(1).toInt)

    args(0).toInt match {
      case x if x==1 => println(s"Show ${args(2).toInt} bit of number ${args(1).toInt.toBinaryString} is ${Tasks.GetKBit(args(1).toInt,args(2).toInt)}")
      case x if x==2 =>println(s"Was ${args(1).toInt.toBinaryString} , value: ${args(2).toInt}, position: ${args(3).toInt}, result: ${Tasks.SetKBit(args(1).toInt,args(2).toInt, args(3).toInt).toBinaryString}")
      case x if x==3 => println(s"Swap: (${args(2)}, ${args(3)}) Was :${args(1).toInt.toBinaryString}, result: ${Tasks.SwapIJ(args(1).toInt,args(2).toInt, args(3).toInt).toBinaryString}")
      case x if x==4 => println(s"Was: ${args(1).toInt.toBinaryString}, result: ${Tasks.NullMLitlleBit(args(1).toInt,args(2).toInt).toBinaryString}")
      case x if x==5 => println(s"For permutation ${args(2)},${args(3)},${args(4)},${args(5)} , for number ${args(1).toInt.toBinaryString}, result: ${Tasks.MergeBytes(args(1).toInt,Array[Int](args(2).toInt, args(3).toInt, args(4).toInt, args(5).toInt)).toBinaryString}")
      case x if x==6 =>println(s"Degree for ${args(1).toInt} on condition [2^p, 2^(p+1) = ${Tasks.GetDegreeTwo(args(1).toInt)}")
      case x if x==7=> println(s"Max degree of 2 for division of ${args(1).toInt} is : ${Tasks.FindMaxTwoDegreeOnDivision(args(1).toInt)}")
      case x if x==8=> Tasks.CyclicShift(args(1).toInt,args(2).toInt)
      case _ => println("No method")
    }


  }



}

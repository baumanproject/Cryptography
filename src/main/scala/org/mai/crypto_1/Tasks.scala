package org.mai.crypto_1

//import scala.math.BigInt
import scala.math.log10

object Tasks {

  def GetKBit(value:Int,k:Int):Int = (value >> k)&1

  def SetKBit(value:Int,k:Int, p:Int):Int = (~(1 << k) & value) | p << k

  def SwapIJ(value:Int, i:Int, j:Int):Int= {
    val j_v = GetKBit(value,j)
    val i_v = GetKBit(value,i)
    var value_1 = SetKBit(value,i, j_v)
    value_1 = SetKBit(value_1,j, i_v)
    value_1
  }

  def NullMLitlleBit(value:Int,k:Int):Int = value>>k<<k

  def FindMaxTwoDegreeOnDivision(value:Int):Int={
    var log2 = (x:Double) => log10(x)/log10(2.0)
    log2((~value+1)&value).toInt

  }


  def GetDegreeTwo(value:Int):Int={
    var result = 0
    var copy:Int = value
    while(copy>0){
      copy>>=1
      result+=1
    }
    result-1
  }

  def MergeBytes(value:Int,per:Array[Int]):Int={
    try{
      if (per.length<4){
        throw new Exception("Failed")
      }
    }
    catch{
      case _: Exception => println("Invalid array length")
    }


    val mask:Int = 255

    val result = (value & mask) << per(0) * 8 |
      ((value & mask << 8) >> 8) << per(1) * 8 |
      ((value & mask << 16) >> 16) << per(2) * 8 |
      ((value & mask << 24) >> 24) << per(3) * 8

    println(value.toBinaryString)
    println(result.toBinaryString)
    result

  }

  def CyclicShift(value:Int,n:Int):Unit={
    println(s"For ${value.toBinaryString} ShiftRight: ${ShiftRight(value,n,32).toBinaryString}; ShiftLeft: ${ShiftLeft(value,n,32).toBinaryString}")
  }

  def ShiftLeft(value:Int,n:Int, dc:Int):Int= (value << n) | (value >> (dc - n))

  def ShiftRight(value:Int, n:Int, dc:Int):Int= (value >> n) | (value << (dc - n))


}


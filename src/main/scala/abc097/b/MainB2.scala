package abc097.b

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer


object MainB2 {
  def read() = {
    val sc = new Scanner(System.in)
    val x = sc.nextInt()
    x
  }


  def parfectPowerList(base: Int, upperBound: Int): List[Int] = {
    if (base == 1) {
      return List(1)
    }
    val buf: ArrayBuffer[Int] = ArrayBuffer.empty

    // 2乗から
    var num = base * base
    while (num <= upperBound) {
      buf.append(num)
      num = num * base
    }
    buf.toList
  }

  def main(args: Array[String]): Unit = {
    val x: Int = read()
    val parfectPowers = (1 to x).flatMap(i => parfectPowerList(i, x))
    val result = parfectPowers.max
    println(result)
  }
}
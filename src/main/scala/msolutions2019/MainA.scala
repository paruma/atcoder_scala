package msolutions2019

import java.util.Scanner


object MainA {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()

    val result = (n-2) * 180

    println(result)
  }
}
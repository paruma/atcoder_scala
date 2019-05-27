package tenka1_2019

import java.util.Scanner

object MainA {
  def read() = {
    val sc = new Scanner(System.in)
    val a, b, c = sc.nextInt()

    //val h = for (_ <- 0 until n) yield sc.nextInt()
    (a, b, c)
  }

  def solve(n: Int): Int = {
    0
  }


  def main(args: Array[String]): Unit = {
    val (a, b, c) = read()
    if ((a < c && c < b) ||(b < c && c < a)) {
      println("Yes")
    } else {
      println("No")
    }
  }

}
package agc033

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer


object MainB {

  def read() = {
    val sc = new Scanner(System.in)

    val n = sc.nextInt()
    val b = for (i <- 0 until n) yield sc.nextInt()
    (n, b)
  }


  def solve(n: Int, b: IndexedSeq[Int]): Int = {
    0
  }

  def main(args: Array[String]): Unit = {
    val (n, b) = read()
    println(solve(n, b))
  }
}
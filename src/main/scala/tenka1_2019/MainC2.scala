package tenka1_2019

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer

object MainC2 {
  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val s = sc.next()
    //val h = for (_ <- 0 until n) yield sc.nextInt()
    (n, s)
  }

  def solve(n: Int, s: String): Int = {
    val cumSumB = s.map(c => if (c == '#') 1 else 0).scanLeft(0)(_ + _)
    val cumSumW = s.map(c => if (c == '.') 1 else 0).scanLeft(0)(_ + _)
    case class Range(begin: Int, end: Int)
    def countB(range: Range) = cumSumB(range.end) - cumSumB(range.begin)

    def countW(range: Range) = cumSumW(range.end) - cumSumW(range.begin)

    // 白にする区間: [0, i) (i=0,...,n)
    (0 to n).map { i =>
      val whiteRange = Range(0, i)
      val blackRange = Range(i, n)
      countB(whiteRange) + countW(blackRange) // 異なる数
    }.min
  }


  def main(args: Array[String]): Unit = {
    val (n, s) = read()
    println(solve(n, s))
  }

}
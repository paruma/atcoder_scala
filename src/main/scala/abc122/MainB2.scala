package abc122

import java.util.Scanner

// O(n)解法
object MainB2 {
  def read() = {
    val sc = new Scanner(System.in)
    val s = sc.next()
    s
  }

  def isATGCChar(c: Char): Boolean = "ATGC".contains(c)

  def solve(s: String): Int = {
    s.foldLeft((0, 0)) {
      case ((maxCount, currentCount), ch) =>
        if (isATGCChar(ch)) {
          val nextCount = currentCount + 1
          (Math.max(maxCount, nextCount), nextCount)
        }
        else (maxCount, 0)
    }._1
  }

  def solve2(s: String): Int = {
    s.scanLeft(0)((cnt, ch) => if (isATGCChar(ch)) cnt + 1 else 0).max
  }


  def main(args: Array[String]): Unit = {
    val s = read()
    println(solve(s))
  }
}
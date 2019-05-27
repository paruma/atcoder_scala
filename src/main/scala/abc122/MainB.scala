package abc122

import java.util.Scanner



object MainB {
  def read() = {
    val sc = new Scanner(System.in)
    val s = sc.next()
    s
  }

  def isATGCStr(s: String): Boolean = s.forall(c => "ATGC".contains(c))

  def solve(s: String): Int = {
    val subStrs = for (
      i <- 0 to s.length;
      j <- i to s.length) yield s.substring(i, j)
    subStrs.filter(isATGCStr).map(_.length).max
  }

  def isATGCChar(c: Char): Boolean = "ATGC".contains(c)

  // https://atcoder.jp/contests/abc122/submissions/4688907
  def solve2(s: String): Int = {
    s.tails.map(l => l.takeWhile(isATGCChar).length).max
  }

  def main(args: Array[String]): Unit = {
    val s = read()
    println(solve(s))
  }
}
package abc122

import java.util.Scanner


object MainC {

  case class MyRange(left: Int, right: Int)

  def read() = {
    val sc = new Scanner(System.in)
    val n, q = sc.nextInt()
    val s = sc.next()
    val ranges = for (i <- 0 until q) yield MyRange(sc.nextInt() - 1, sc.nextInt() - 1) // 0-origin化
    (n, q, s, ranges)
  }

  def solve(n: Int, q: Int, s: String, ranges: IndexedSeq[MyRange]): IndexedSeq[Int] = {
    // hasAC(i): s(i)='A'かつs(i+1)='C'
    val hasAC = for (i <- 0 until n - 1) yield {
      s(i) == 'A' && s(i + 1) == 'C'
    }
    // countAC(i) = #{j| j<i, hasAC(i)==True}
    val countAC = hasAC.map(p => if (p) 1 else 0).scanLeft(0)(_ + _)
    for (range <- ranges) yield {
      // [range.left, range.right-1]の範囲でhasACがTrueの数を調べる
      countAC(range.right) - countAC(range.left)
    }
  }


  def main(args: Array[String]): Unit = {
    val (n, q, s, ranges) = read()
    solve(n, q, s, ranges).foreach(println)
  }
}
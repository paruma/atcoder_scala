package exa2019

import java.util.Scanner

// 剰余取る前
object MainD {


  def read() = {
    val sc = new Scanner(System.in)
    val n, x = sc.nextInt()
    val s = for (_ <- 0 until n) yield sc.nextInt()
    (n, x, s.sorted)
  }


  def solve2(n: Int, x: Int, s: IndexedSeq[Int]): Long = {
    val ss = s.sorted
    val maxY = Math.max(x, ss.max)
    // dp: [0, n) * [0, maxY] => [0, infty)
    // dp(i, y) = (0 to i).map(ss).permutations.map(t => t.foldLeft(y)(_%_)).sum
    // dp(0, y) = y mod ss(0)
    // dp(i, y) = (i - 1) * dp(i - 1, y) + dp(i - 1, y mod ss(i))

    val initDP = IndexedSeq.tabulate(maxY + 1)(y => (y % ss(0)).toLong)
    val resultDP = (1 until n).foldLeft(initDP)((dp, i) =>
      IndexedSeq.tabulate(dp.length)(y => i * dp(y) + dp(y % ss(i)))
    )
    resultDP(x)
  }


  def main(args: Array[String]): Unit = {
    val (n, x, s) = read()
    println(s"ans:${solve2(n, x, s)}")
  }


  // ---以下実験用---

  // O(n!)愚直解
  def solve(n: Int, x: Int, s: IndexedSeq[Int]): Long = {
    s.permutations.map(t => t.foldLeft(x)(_ % _)).sum
  }

  def experiment(n: Int, x: Int, s: IndexedSeq[Int]): Unit = {
    s.permutations.map(t => (t, t.scanLeft(x)(_ % _))).foreach(println)
    s.permutations.map(t => (t, t.foldLeft(x)(_ % _))).foreach(println)
    s.permutations.map(t => t.foldLeft(x)(_ % _)).toIndexedSeq.groupBy(identity).map { case (e, xs) => (e, xs.length) }.foreach(println)
  }

  def experiment2(): Unit = {
    val n = 5
    val s = IndexedSeq(22, 11, 6, 5, 13)
    (0 until 200).foreach { x =>
      println(s"x=$x")
      s.permutations.map(t => t.foldLeft(x)(_ % _)).toIndexedSeq.groupBy(identity).map { case (e, xs) => (e, xs.length) }.foreach(println)
      println()
    }
  }

  def experiment3(): Unit = {
    val n = 5
    val s = IndexedSeq(22, 11, 6, 5, 13)
    (0 until 200).foreach { x =>
      println(s"x=$x, ans: ${solve(n, x, s)}")
    }
  }
}
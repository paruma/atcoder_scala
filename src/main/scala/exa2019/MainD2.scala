package exa2019

import java.util.Scanner

//TLE
object MainD2 {


  case class RR(representative: Long) extends AnyVal {
    def +(that: RR): RR = RR((this.representative + that.representative) % RR.mod)

    def -(that: RR): RR = RR((this.representative - that.representative + RR.mod) % RR.mod)

    def *(that: RR): RR = RR((this.representative * that.representative) % RR.mod)

    def unary_- : RR = RR((-this.representative + RR.mod) % RR.mod)
  }


  object RR {
    val mod: Long = (1e9+7).toLong

    val _0: RR = RR(0)
    val _1: RR = RR(1)
  }

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

    val initDP = IndexedSeq.tabulate(maxY + 1)(y => RR(y % ss(0)))
    val resultDP = (1 until n).foldLeft(initDP)((dp, i) =>
      IndexedSeq.tabulate(dp.length)(y => RR(i) * dp(y) + dp(y % ss(i)))
    )
    resultDP(x).representative
  }


  def main(args: Array[String]): Unit = {
    val (n, x, s) = read()
    println(solve2(n, x, s))
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
package abc125

import java.util.Scanner


object MainD {
  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val a = for (_ <- 0 until n) yield sc.nextLong()
    (n, a)
  }

  def solve(n: Int, a: IndexedSeq[Long]): Long = {
    val minAbs = a.map(Math.abs).min
    val cntMinus = a.count(n => n < 0)
    if (cntMinus % 2 == 0) a.map(Math.abs).sum
    else a.map(Math.abs).sum - 2 * minAbs
  }

  // DP解法
  def solve2(n: Int, a: IndexedSeq[Long]): Long = {
    // .tail.init:headとlastを除去
    val (resultDPPlus, resultDPMinus) = a.tail.init.foldLeft((a(0), -a(0))) {
      case ((dpPlus, dpMinus), ae) =>
        val nextDPPlus = Math.max(dpPlus + ae, dpMinus - ae)
        val nextDPMinus = Math.max(dpPlus - ae, dpMinus + ae)
        (nextDPPlus, nextDPMinus)
    }

    Math.max(resultDPPlus + a.last, resultDPMinus - a.last)
  }

  def main(args: Array[String]): Unit = {
    val (n, a) = read()
    println(solve2(n, a))
  }
}
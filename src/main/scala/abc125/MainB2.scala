package abc125

import java.util.Scanner


object MainB2 {

  def solve2(n: Int, v: IndexedSeq[Int], c: IndexedSeq[Int]): Int = {
    val benefits = for (pattern <- 0 until 1 << n) yield {
      val patternList = (0 until n).map(i => (pattern >> i) & 1)
      val totalValue = (v, patternList).zipped.map(_ * _).sum
      val totalCost = (c, patternList).zipped.map(_ * _).sum
      totalValue - totalCost
    }
    benefits.max
  }

  def solve3(n: Int, v: IndexedSeq[Int], c: IndexedSeq[Int]): Int = {
    // v.zip(c)の部分列全生成
    val vcSubSeqs = for (pattern <- 0 until 1 << n) yield {
      val patternList = (0 until n).map(i => (pattern >> i) & 1)
      v.zip(c).zip(patternList).filter(_._2 == 1).map(_._1)
    }
    vcSubSeqs.map(subSeq => subSeq.map { case (ve, ce) => ve - ce }.sum).max
  }



  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val v = for (_ <- 0 until n) yield sc.nextInt()
    val c = for (_ <- 0 until n) yield sc.nextInt()

    println(solve3(n, v, c))
  }
}
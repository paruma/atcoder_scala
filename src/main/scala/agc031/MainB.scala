package agc031

import java.util.Scanner

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object MainB {

  case class ResidueRing(private val input: Long) {
    val representative: Long = ((input % ResidueRing.mod) + ResidueRing.mod) % ResidueRing.mod

    def +(that: ResidueRing): ResidueRing = ResidueRing(this.representative + that.representative)

    def -(that: ResidueRing): ResidueRing = ResidueRing(this.representative - that.representative)

    def *(that: ResidueRing): ResidueRing = ResidueRing(this.representative * that.representative)

    def unary_- : ResidueRing = ResidueRing(-this.representative)

  }

  object ResidueRing {
    val mod: Long = (1e9 + 7).toLong
    val zero: ResidueRing = ResidueRing(0)
    val one: ResidueRing = ResidueRing(1)
  }

  type Stone = Int


  def read(): (Int, IndexedSeq[Stone]) = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val stones = for (i <- 0 until n) yield sc.nextInt()
    (n, stones)
  }

  def solve(n: Int, stones: IndexedSeq[Stone]): Long = {
    val edges: Array[mutable.Set[Int]] = Array.fill(n)(mutable.Set.empty)
    val maxColor = 500000

    // indexを0から走査したとき、stoneを通った一番最近のindex
    val history: Array[Option[Stone]] = Array.fill(maxColor)(None)

    for ((stone, i) <- stones.zipWithIndex) {
      if (i >= 1) edges(i).add(i - 1) // 後ろへ辺を追加
      history(stone).foreach { oldI => //stones(oldI) == stones(i) = stone
        edges(i).add(oldI)
      }
      history(stone) = Some(i)
    }

    val dp: Array[ResidueRing] = Array.fill(n)(ResidueRing.zero)
    dp(0) = ResidueRing.one
    for (i <- 1 until n) {
      dp(i) = edges(i).toList.map(oldI => dp(oldI)).foldLeft(ResidueRing.zero)(_ + _)
    }
    dp(n - 1).representative
  }

  def main(args: Array[String]): Unit = {
    val (n, stones) = read()
    println(solve(n, stones))
  }

}
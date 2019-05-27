package abc123

import java.util.Scanner

import scala.annotation.tailrec

// [(a,b,c) | abc<=k]をソート (リファクタリング)
object MainD8 {
  def read(): (Int, Int, Int, Int, IndexedSeq[Long], IndexedSeq[Long], IndexedSeq[Long]) = {
    val sc = new Scanner(System.in)
    val n1, n2, n3, k = sc.nextInt()
    val l1 = for (_ <- 0 until n1) yield sc.nextLong()
    val l2 = for (_ <- 0 until n2) yield sc.nextLong()
    val l3 = for (_ <- 0 until n3) yield sc.nextLong()
    (n1, n2, n3, k, l1, l2, l3)
  }


  def solve(n1: Int, n2: Int, n3: Int, k: Int, l1: IndexedSeq[Long], l2: IndexedSeq[Long], l3: IndexedSeq[Long]): IndexedSeq[Long] = {

    // breakを使わない実装
    // (i1, i2) => [i3 | i3<- l3.indices, (i1+1)*(i2+1)*(i3+1)<=k]を求める。
    // 愚直にfilterすると遅いので、i3 => (i1+1)*(i2+1)*(i3+1)の単調性を利用する
    def i3List(i1: Int, i2: Int): List[Int] = {
      @tailrec // 末尾再帰でなくてもどうにかなる(たかだか1000回再帰なので)
      def sub(i3: Int, acc: List[Int]): List[Int] = {
        if (!l3.isDefinedAt(i3)) acc
        else if ((i1 + 1) * (i2 + 1) * (i3 + 1) > k) acc
        else sub(i3 + 1, i3 :: acc)
      }

      sub(i3 = 0, Nil)
    }

    val indexListRequirementOfTopk: IndexedSeq[(Int, Int, Int)] =
      for (i1 <- l1.indices;
           i2 <- l2.indices;
           i3 <- i3List(i1, i2)
      ) yield (i1, i2, i3)

    val sortedL1 = l1.sorted.reverse
    val sortedL2 = l2.sorted.reverse
    val sortedL3 = l3.sorted.reverse
    indexListRequirementOfTopk.map { case (i1, i2, i3) => sortedL1(i1) + sortedL2(i2) + sortedL3(i3) }.sorted.reverse.take(k)
  }

  def solve2(n1: Int, n2: Int, n3: Int, k: Int, l1: IndexedSeq[Long], l2: IndexedSeq[Long], l3: IndexedSeq[Long]): IndexedSeq[Long] = {

    val indexListRequirementOfTopk: IndexedSeq[(Int, Int, Int)] =
    //((i1+1)*(i2+1)*(i3+1)<=k
      for (i1 <- l1.indices;
           i2 <- l2.indices;
           i3 <- 0 to Math.min(l3.length - 1, k / ((i1 + 1) * (i2 + 1)) - 1)
      ) yield (i1, i2, i3)

    val sortedL1 = l1.sorted.reverse
    val sortedL2 = l2.sorted.reverse
    val sortedL3 = l3.sorted.reverse
    indexListRequirementOfTopk.map { case (i1, i2, i3) => sortedL1(i1) + sortedL2(i2) + sortedL3(i3) }.sorted.reverse.take(k)
  }


  def printResult(result: IndexedSeq[Long]) = {
    result.foreach(println)
  }


  def main(args: Array[String]): Unit = {
    val (n1, n2, n3, k, l1, l2, l3) = read()
    printResult(solve2(n1, n2, n3, k, l1, l2, l3))
  }
}

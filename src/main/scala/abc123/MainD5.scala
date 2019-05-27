package abc123

import java.util.Scanner

// [x1 + x2 | x1<- l1, x2<- l2]をソート (TLE)
object MainD5 {
  def read(): (Int, Int, Int, Int, IndexedSeq[Long], IndexedSeq[Long], IndexedSeq[Long]) = {
    val sc = new Scanner(System.in)
    val n1, n2, n3, k = sc.nextInt()
    val l1 = for (_ <- 0 until n1) yield sc.nextLong()
    val l2 = for (_ <- 0 until n2) yield sc.nextLong()
    val l3 = for (_ <- 0 until n3) yield sc.nextLong()
    (n1, n2, n3, k, l1, l2, l3)
  }


  def solve(n1: Int, n2: Int, n3: Int, k: Int, l1: IndexedSeq[Long], l2: IndexedSeq[Long], l3: IndexedSeq[Long]): IndexedSeq[Long] = {
    val l1_plus_l2 = for (x1 <- l1; x2 <- l2) yield x1 + x2
    // O(n1*n2)
    // l1+l2の時点でtop kにないものはl3を足してもtop kにならない。
    val sorted_l1_plus_l2_top_k = l1_plus_l2.sorted.reverse.take(k)
    //O((n1*n2)log(n1*n2))
    // k個取れない場合もエラーは出ない。取れるだけ取る
    val sorted_l1_plus_l2_top_k_plus_l3 = for (x12 <- sorted_l1_plus_l2_top_k; x3 <- l3) yield x12 + x3
    // O(k*n3)
    val sorted_sorted_l1_plus_l2_top_k_plus_l3_topk = sorted_l1_plus_l2_top_k_plus_l3.sorted.reverse.take(k) //O((k*n3)log(k*n3))
    sorted_sorted_l1_plus_l2_top_k_plus_l3_topk
  }

  def printResult(result: IndexedSeq[Long]) = {
    result.foreach(println)
  }


  def main(args: Array[String]): Unit = {
    val (n1, n2, n3, k, l1, l2, l3) = read()
    printResult(solve(n1, n2, n3, k, l1, l2, l3))
  }
}
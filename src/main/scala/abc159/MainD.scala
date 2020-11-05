package abc159

import java.util.Scanner

object MainD {

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val a = IndexedSeq.fill(n)(sc.nextInt())
    (n, a)
  }

  def output(result: IndexedSeq[Long]) = {
    val out = new java.io.PrintWriter(System.out)
    result.foreach(out.println)
    out.flush()
  }

  def solve(n: Int, lst: IndexedSeq[Int]): IndexedSeq[Long] = {
    def comb2(x: Int): Long = x.toLong * (x.toLong - 1) / 2

    val pool = {
      val tmp = Array.fill(n + 1)(0)
      for (e <- lst) {
        tmp(e) += 1
      }
      tmp.toIndexedSeq
    }
    val poolCmb = pool.map(comb2)
    val sumAll = poolCmb.sum

    for (k <- 0 until n) yield {
      sumAll - poolCmb(lst(k)) + comb2(pool(lst(k))-1)
    }
  }

  def main(args: Array[String]): Unit = {
    val (n, a) = read()
    output(solve(n, a))
  }
}
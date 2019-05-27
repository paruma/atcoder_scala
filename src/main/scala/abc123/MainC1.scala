package abc123

import java.util.Scanner


object MainC1 {

  def solve(n: Long, capaList: IndexedSeq[Long]): Long = {
    val minCapa = capaList.min
    capaList.length + (n-1)/minCapa
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val n = sc.nextLong()
    val capaList = for (_ <- 0 until 5) yield sc.nextLong()
    println(solve(n, capaList))
  }
}
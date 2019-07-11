package abc130

import java.util.Scanner


object MainD {


  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val k = sc.nextLong()
    val a = IndexedSeq.fill(n)(sc.nextLong())
    (n, k, a)
  }

  def solve(n: Int, k: Long, a: IndexedSeq[Long]): Long = {
    val cumSum = a.scanLeft(0L)(_ + _)

    def sumA(begin: Int, end: Int) = cumSum(end) - cumSum(begin)

    //しゃくとり
    var end = 0
    val cnt = 0L
    var sum = 0L
    for (i <- 0 until n) {
      //[i, end)で和がkを超えるまで進める
      while (end < n && sumA(i, end) < k) {
        end += 1
      }
      if (sumA(i, end) >= k) {
        sum += (n - end + 1)
      }
    }
    sum
  }

  def main(args: Array[String]): Unit = {
    val (n, k, a) = read()
    println(solve(n, k, a))
  }
}
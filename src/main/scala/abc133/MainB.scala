package abc133

import java.util.Scanner


object MainB {


  def read() = {
    val sc = new Scanner(System.in)
    val n, d = sc.nextInt()
    val x = IndexedSeq.fill(n, d)(sc.nextInt())
    (n, d, x)
  }

  def solve(n: Int, d: Int, x: IndexedSeq[IndexedSeq[Int]]): Long = {
    def dist(y:IndexedSeq[Int], z:IndexedSeq[Int]):Double ={
      math.sqrt((y,z).zipped.map((yi,zi)=> (yi-zi)*(yi-zi)).sum)
    }
    val eps = 1e-6
    (for (i1 <- 0 until n; i2 <- i1 + 1 until n) yield {
      val y = x(i1)
      val z = x(i2)
      val d = dist(y,z)
        d.%(1) <= eps || d.%(1) >= 1 - eps
    }).count(identity)
  }

  def main(args: Array[String]): Unit = {
    val (n, d, x) = read()
    println(solve(n, d, x))
  }
}
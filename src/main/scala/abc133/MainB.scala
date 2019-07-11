package abc132

import java.util.Scanner


object MainB {


  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val p = IndexedSeq.fill(n)(sc.nextInt())
    (n, p)
  }

  def solve(n:Int, p:IndexedSeq[Int]): Long = {
    (1 to n-2).count { i =>
      val t = IndexedSeq(p(i - 1), p(i), p(i + 1)).sorted
      t(1) == p(i)
    }
  }

  def main(args: Array[String]): Unit = {
    val (n, p) = read()
    println(solve(n, p))
  }
}
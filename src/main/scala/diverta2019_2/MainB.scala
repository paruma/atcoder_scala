package abc129

import java.util.Scanner


object MainB {


  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val w = IndexedSeq.fill(n)(sc.nextInt())
    (n, w)
  }

  def solve(n: Int, w: IndexedSeq[Int]): Long = {
    // [0, i), [i, n)
    (1 until n).map { i =>
      val s1 = (0 until i).map(w).sum
      val s2 = (i until n).map(w).sum
      math.abs(s1 - s2)
    }.min
  }

  def main(args: Array[String]): Unit = {
    val (n, w) = read()
    println(solve(n, w))
  }
}
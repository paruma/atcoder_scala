package abc124

import java.util.Scanner

// O(n^2)解法
object MainB {
  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val h = for (_ <- 0 until n) yield sc.nextInt()
    (n, h)
  }

  //O(n^2)解法
  def solve(n: Int, h: IndexedSeq[Int]): Int = {
    // until と to 間違えて時間かかった(5分30秒)
    def canView(i: Int): Boolean = (0 until i).forall(h(_) <= h(i))

    (0 until n).count(canView)
  }


  //O(n^2)解法(tailsを利用)
  def solve4(n: Int, h: IndexedSeq[Int]): Int =
    h.reverse.tails
      .filter(l => l.nonEmpty)
      .count(l => l.tail.forall(x => x <= l.head))


  def main(args: Array[String]): Unit = {
    val (n, h) = read()
    println(solve(n, h))
  }

}
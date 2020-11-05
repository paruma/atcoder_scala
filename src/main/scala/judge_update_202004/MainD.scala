package judge_update_202004

import java.util.Scanner

import scala.annotation.tailrec

object MainD {

  def read() = {
    val sc = new Scanner(System.in)

    val n, q = sc.nextInt()
    val a = IndexedSeq.fill(n)(sc.nextInt())
    val s = IndexedSeq.fill(q)(sc.nextInt())
    (n, q, a, s)
  }

  @tailrec
  def gcd(x: Int, y: Int): Int = {
    assert(x >= 0 && y >= 0)
    if (y == 0) x else gcd(y, x % y)
  }

  // binarysearch一般化したい
  def binarySearch(lst: IndexedSeq[Int], query: Int): Option[Int] = {
    def sub(begin: Int, end: Int): Option[Int] = {
      if(begin == end) return None
      val mid = (begin + end) / 2
      if (gcd(lst(mid - 1), query) != 1 && gcd(lst(mid), query) == 1) {
        Some(mid)
      } else if (gcd(lst(mid - 1), query) != 1 && gcd(lst(mid), query) != 1) {
        sub(mid + 1, end)
      } else {
        sub(begin, mid)
      }
    }

    sub(1, lst.length)
  }

  def solve(n: Int, q: Int, a: IndexedSeq[Int], s: IndexedSeq[Int]): IndexedSeq[Int] = {
    val cumGCD = a.scanLeft(0)(gcd)
    // q(i)を二分探索
    s.map(x => {
      val result = binarySearch(cumGCD, x)
      if(result.nonEmpty) result.get
      else gcd(cumGCD.last, x)
    })
  }

  def main(args: Array[String]): Unit = {
    val (n, q, a, s) = read()
    solve(n, q, a, s).foreach(println)
  }
}
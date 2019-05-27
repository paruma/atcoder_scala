package abc123

import java.util.Scanner


object MainB1 {

  // 切り上げる
  def roundUp10(x: Int): Int = {
    if (x % 10 == 0) x
    else (x / 10 + 1) * 10
  }

  def roundUp10Lack(x: Int): Int = roundUp10(x) - x

  def leaveOneOut(xs: IndexedSeq[Int]): IndexedSeq[(Int, IndexedSeq[Int])] = {
    val n = xs.length
    (0 until n).map(i => (xs(i), IndexedSeq.concat(0 until i, i + 1 until n).map(xs)))
  }

  // 提出解
  // nlogn
  def solve1(xs: IndexedSeq[Int]): Int = {
    val sortedXs = xs.sortBy(n => if (n % 10 == 0) 10 else n % 10)
    sortedXs.tail.map(roundUp10).sum + sortedXs.head
  }

  // n!
  def solve2(xs: IndexedSeq[Int]): Int =
    xs.permutations.map(l => l.init.map(roundUp10).sum + l.last).min

  // n
  def solve3(xs: IndexedSeq[Int]): Int = {
    xs.map(roundUp10).sum - xs.map(roundUp10Lack).max
  }

  // n^2
  def solve4(xs: IndexedSeq[Int]): Int = {
    leaveOneOut(xs).map { case (y, ys) => ys.map(roundUp10).sum + y }.min
  }


  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val xs = for (_ <- 0 until 5) yield sc.nextInt
    println(solve3(xs))
  }
}
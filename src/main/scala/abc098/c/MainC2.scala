package abc098.c

import java.util.Scanner


object MainC2 {
  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val row = sc.next()
    (n, row)
  }


  case class SeqForCumulative[T](list: IndexedSeq[T])(implicit num: Numeric[T]) {
    // ↓defだと複数回計算してしまうぽい
    val cumulative: IndexedSeq[T] = list.scanLeft(num.zero)(num.plus)

    def partial(start: Int, end: Int): T = num.minus(cumulative(end), cumulative(start))
  }

  def solve(n: Int, row: String): Int = {
    val indicatorE = row.map(elem => if (elem == 'E') 1 else 0)
    val indicatorW = row.map(elem => if (elem == 'W') 1 else 0)

    val cumulativeE = SeqForCumulative(indicatorE)
    val cumulativeW = SeqForCumulative(indicatorW)

    // リーダーがlearder番目のとき、リーダーの方向を向いていない人数
    val numNotFaceLeader = (leader: Int) => cumulativeW.partial(0, leader) + cumulativeE.partial(leader + 1, n)

    (0 until n).map(numNotFaceLeader).min
  }


  def main(args: Array[String]): Unit = {
    val (n, row) = read()
    println(solve(n, row))
  }
}
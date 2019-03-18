package sandbox

import scala.annotation.tailrec


object RecurrenceRelation {
  // 漸化式のテストプログラム

  // 1項間漸化式: a(n) = f(a(n-1))
  // 2項間漸化式: a(n) = f(a(n-1), a(n-2))

  def geometricSequence(): Unit = {
    val n = 100

    //無限リスト
    /*
        seq1
        1 : seq1.map(_*2)
        1 : 2 : seq1.tail.map(_*2)
        1 : 2 : 4 : seq1.tail.tail.map(_*2)
     */
    lazy val seq1: Stream[Int] = 1 #:: seq1.map(_ * 2)
    println(seq1.take(10).toList)

    // 有限 (Streamなしで)
    // 項数指定(take)

    // 一度に数列を定義するのが難しい
    // ListBufferを作って各要素を逐一計算していくことならできる。

    // まずは再帰
    @tailrec
    def rec(n: Int, acc: List[Int]): List[Int] = if (n == 0) acc else rec(n - 1, acc.head * 2 :: acc)

    val req2 = rec(n, Nil)


    // 条件指定 (takeWhile)

  }

  def main(args: Array[String]): Unit = {
    geometricSequence()
  }
}
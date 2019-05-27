package abc127

import java.util.Scanner


object MainD {

  case class Ope(b: Int, c: Long)

  def read() = {
    val sc = new Scanner(System.in)
    val n, m = sc.nextInt()
    val a = IndexedSeq.fill(n)(sc.nextLong())
    val opes = IndexedSeq.fill(m)(Ope(sc.nextInt(), sc.nextLong()))
    (n, m, a, opes)
  }

  //aソートしておく？
  //(b,c)ソートしておく？ ソートしたほうが得。1変換で終わる

  def solve(n: Int, m: Int, a: IndexedSeq[Long], opes: IndexedSeq[Ope]): Long = {
    val sortedOpes = opes.sortBy(ope => ope.c).reverse // 降順
    val sortedA = a.sorted.toArray

    def opeAll(): Unit = {
      var i = 0
      for (ope <- sortedOpes) {

        for (_ <- 0 until ope.b) {
          if (sortedA(i) < ope.c) {
            sortedA(i) = ope.c
          } else {
            return
          }
          i += 1
          if (i >= n) {
            return
          }
        }
      }
    }

    opeAll()

    sortedA.sum
  }

  def main(args: Array[String]): Unit = {
    val (n, m, a, opes) = read()
    println(solve(n, m, a, opes))
  }
}
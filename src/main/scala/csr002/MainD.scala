package csr002

import java.util.Scanner


object MainD {

  case class Pair(a: Long, b: Long)

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val lst = IndexedSeq.fill(n)(Pair(sc.nextLong(), sc.nextLong()))
    (n, lst)
  }

  def solve(n: Int, lst: IndexedSeq[Pair]): Long = {
    lst.map(x=>math.max(x.a, x.b)).sum
  }

  def main(args: Array[String]): Unit = {
    val (n, lst) = read()
    println(solve(n, lst))
  }
}
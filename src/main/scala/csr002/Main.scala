package csr002

import java.util.Scanner


object Main {

  case class Pair(a: Long, b: Long)

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val lst = IndexedSeq.fill(n)(Pair(sc.nextLong(), sc.nextLong()))
    (n, lst)
  }

  def solve(n: Int, lst: IndexedSeq[Pair]): Long = {
    0
  }

  def main(args: Array[String]): Unit = {
    val (n, lst) = read()
    println(solve(n, lst))
  }
}
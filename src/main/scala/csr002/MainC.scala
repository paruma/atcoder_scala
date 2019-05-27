package csr002

import java.util.Scanner


object MainC {

  case class Pair(a: Int, b: Int)

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val lst = IndexedSeq.fill(n)(Pair(sc.nextInt(), sc.nextInt()))
    (n, lst)
  }

  def solve(n: Int, lst: IndexedSeq[Pair]): Int = {
    lst.map(x=> x.a + x.b).max
  }

  def main(args: Array[String]): Unit = {
    val (n, lst) = read()
    println(solve(n, lst))
  }
}
package csr002

import java.util.Scanner


object MainF {

  case class Pair(a: Long, b: Long)

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val lst = IndexedSeq.fill(n)(Pair(sc.nextLong(), sc.nextLong()))
    (n, lst)
  }

  def solve(n: Int, lst: IndexedSeq[Pair]): Long = {
    val lst2 = lst.map(x=>
      if(x.a >x.b) Pair(x.b, x.a)
      else x
    )
    lst2.toSet.size
  }

  def main(args: Array[String]): Unit = {
    val (n, lst) = read()
    println(solve(n, lst))
  }
}
package csr002

import java.util.Scanner

import scala.annotation.tailrec


object MainG {

  case class Pair(a: Long, b: Long)

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val lst = IndexedSeq.fill(n)(Pair(sc.nextLong(), sc.nextLong()))
    (n, lst)
  }

  @tailrec
  def gcd(x: Long, y: Long): Long = {
    assert(x >= 0 && y >= 0)
    if (y == 0) x else gcd(y, x % y)
  }

  def solve(n: Int, lst: IndexedSeq[Pair]): IndexedSeq[Long] = {
    lst.map(x => gcd(x.a, x.b))
  }

  def output(result: IndexedSeq[Long]) = {
    val out = new java.io.PrintWriter(System.out)
    result.foreach(out.println)
    out.flush()
  }

  def main(args: Array[String]): Unit = {
    val (n, lst) = read()
    output(solve(n, lst))
  }
}
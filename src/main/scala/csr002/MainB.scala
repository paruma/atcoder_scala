package csr002

import java.util.Scanner


object MainB {

  case class Pair(a: Int, b: Int)

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val lst = IndexedSeq.fill(n)(Pair(sc.nextInt(), sc.nextInt()))
    (n, lst)
  }

  def solve(n: Int, lst: IndexedSeq[Pair]): IndexedSeq[Int] = {
    lst.map(x => x.a % x.b)
  }

  def output(result: IndexedSeq[Int]) = {
    val out = new java.io.PrintWriter(System.out)
    result.foreach(out.println)
    out.flush()
  }

  def main(args: Array[String]): Unit = {
    val (n, lst) = read()
    output(solve(n, lst))
  }
}
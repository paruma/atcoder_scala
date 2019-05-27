package csr002

import java.util.Scanner


object MainH2 {

  case class Pair(a: Long, b: Long)

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val lst = IndexedSeq.fill(n)(Pair(sc.nextLong(), sc.nextLong()))
    (n, lst)
  }

  def read2() = {
    val input = io.Source.stdin.getLines()
    val n = input.next().toInt
    val lst = IndexedSeq.fill(n) {
      val strs = input.next().split(" ")
      Pair(strs(0).toInt, strs(1).toInt)
    }
    (n, lst)
  }

  def solve(n: Int, lst: IndexedSeq[Pair]): IndexedSeq[Long] = {
    lst.map(x => {
      if (x.a == x.b) -1
      else math.abs(x.a - x.b)
    })
  }

  def output(result: IndexedSeq[Long]) = {
    val out = new java.io.PrintWriter(System.out)
    result.foreach(println)
    out.flush()
  }


  def main(args: Array[String]): Unit = {
    val (n, lst) = read2()
    output(solve(n, lst))
  }
}
package csr002

import java.util.Scanner

import scala.annotation.tailrec


object MainA {

  case class Pair(a: Long, b: Long)

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val lst = IndexedSeq.fill(n)(Pair(sc.nextLong(), sc.nextLong()))
    (n, lst)
  }

  def solve(n: Int, lst: IndexedSeq[Pair]): IndexedSeq[Long]= {
    lst.map(x => x.a * x.b)
  }

  def output1(result :IndexedSeq[Long]) = {
    result.foreach(println)
  }

  def output2(result :IndexedSeq[Long]) = {
    result.foreach(System.out.println)
  }
  def output3(result: IndexedSeq[Long]) = {
    val out = new java.io.PrintWriter(System.out)
    result.foreach(out.println)
    out.flush()
  }

  def main(args: Array[String]): Unit = {
    val (n, lst) = read()
    output3(solve(n, lst))
  }
}
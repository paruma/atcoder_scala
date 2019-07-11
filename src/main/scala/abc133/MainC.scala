package abc132

import java.util.Scanner

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


object MainC {
  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val d = IndexedSeq.fill(n)(sc.nextInt())
    (n, d)
  }

  def solve(n:Int, d:IndexedSeq[Int]): Long = {
    val sd = d.sorted
    sd(n/2) - sd(n/2-1)
  }

  def main(args: Array[String]): Unit = {
    val (n, d) = read()
    println(solve(n,d))
  }
}
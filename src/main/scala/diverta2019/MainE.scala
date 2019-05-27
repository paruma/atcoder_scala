package diverta2019

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer


object MainE {
  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val a = for (_ <- 0 until n) yield sc.nextLong()

    (n, a)
  }

  def solve(n:Int, a: IndexedSeq[Long]): Long={
    9
  }



  def main(args: Array[String]): Unit = {
    //experiment2(200)
    val (n,a) = read()
    println(solve(n, a))
  }
}
package abc124

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer


object MainD {
  def read(): (Int, Int, String) = {
    val sc = new Scanner(System.in)
    val n, k = sc.nextInt()
    val s = sc.next()
    (n, k, s)
  }

  // あとで保存
  def compressRunLength(s: String): IndexedSeq[(Int, Char)] = {

    var currentChar = s(0)
    var currentCount = 1
    val buf = ArrayBuffer.empty[(Int, Char)]
    for (i <- 1 until s.length) {
      if (currentChar == s(i)) {
        currentCount += 1
      } else {
        buf.append((currentCount, currentChar))
        currentChar = s(i)
        currentCount = 1
      }
    }
    buf.append((currentCount, currentChar))
    buf.toIndexedSeq
  }


  def solve(n: Int, k: Int, s: String): Int = {
    // s.length == n
    val runLength = compressRunLength(s)
    val cumSum = runLength.map(_._1).scanLeft(0)(_ + _)

    // (i, ..., j-1)の和: cumSum(j)-cumSum(i)
    def sum(min: Int, max: Int): Int = cumSum(max + 1) - cumSum(min)

    //clamp
    def cut(i: Int) = Math.min(i, runLength.length - 1)

    // i番目が0: i, i+1,...,i+2k-1
    // i番目が1: i, i+1,...,i+2k
    runLength.indices
      .map(i =>
        if (runLength(i)._2 == '0') (i, cut(i + 2 * k - 1))
        else (i, cut(i + 2 * k)))
      .map { case (min, max) =>
        sum(min, max)
      }.max
  }


  def main(args: Array[String]): Unit = {
    val (n, k, s) = read()
    println(solve(n, k, s))
  }
}
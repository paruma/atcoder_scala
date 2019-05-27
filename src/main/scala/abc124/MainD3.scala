package abc124

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer

// 累積和使わない場合(TLEするはず)
object MainD3 {
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


    //clamp
    def cut(i: Int) = Math.min(i, runLength.length - 1)

    // i番目が0: i, i+1,...,i+2k-1
    // i番目が1: i, i+1,...,i+2k
    runLength.indices
      .map(i =>
        if (runLength(i)._2 == '0') (i, cut(i + 2 * k - 1))
        else (i, cut(i + 2 * k)))
      .map { case (min, max) =>
        (min to max).map(i=>runLength(i)._1).sum
      }.max
  }


  def main(args: Array[String]): Unit = {
    val (n, k, s) = read()
    println(solve(n, k, s))
  }
}
package abc098.d

import java.util.Scanner

import scala.collection.mutable.ListBuffer


object MainD {
  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val seqBuf = ListBuffer.empty[Int]
    for (i <- 0 until n) {
      val elem = sc.nextInt()
      seqBuf.append(elem)
    }
    (n, seqBuf.toList)
  }

  def main(args: Array[String]): Unit = {
    val (n, seq) = read()
    val cumulativeSum = seq.scanLeft(0)(_ + _)
    val cumulativeXor = seq.scanLeft(0)(_ ^ _)
    val arr = Array.fill[Long](n)(0)
    for (i <- 0 until n) {
      var k: Long = 0
      for (j <- 1 to 20) {
        //配列外参照に注意
        if (i + j <= n) {
          if (cumulativeSum(i + j) - cumulativeSum(i) == (cumulativeXor(i + j) ^ cumulativeXor(i))) {
            k = k + 1
          }
        }
      }
      arr(i) = k
    }
    val result = arr.sum
    println(result)
  }
}
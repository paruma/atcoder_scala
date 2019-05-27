package abc128

import java.util.Scanner

import scala.util.Random


object MainD {

  def read() = {
    val sc = new Scanner(System.in)
    val n, k = sc.nextInt()
    val v = IndexedSeq.fill(n)(sc.nextInt())
    (n, k, v)
  }

  def solve(n: Int, k: Int, v: IndexedSeq[Int]): Int = {

    (for (deqTimes <- 0 to math.min(k, n); deqRightTimes <- 0 to deqTimes) yield {
      // 右側から何回撮るか
      val deqLeftTimes = deqTimes - deqRightTimes
      // [0, l), [n-r, n)
      val deqLeft = (0 until deqLeftTimes).map(v)
      val deqRight = (n - deqRightTimes until n).map(v)
      val deq = IndexedSeq.concat(deqLeft, deqRight).sorted

      val enqTimes = k - deqTimes
      // 負のやつ捨てる
      val result = deq.zipWithIndex.map {
        case (v, i) => if (i < enqTimes && v < 0) 0 else v
      }.sum
      result
    }).max
  }

  def readRandom() = {
    val n = 50
    val k = 100
    val v = IndexedSeq.fill(n)(Random.nextInt(20000000) - 10000000)
    println(s"$n $k")
    println(v.mkString(" "))
    (n, k, v)
  }

  def main(args: Array[String]): Unit = {
    val (n, k, v) = read()
    println(solve(n, k, v))
  }
}
package abc084.d

import java.util.Scanner

case class Range(l: Int, r: Int) {
  val toList: scala.Range = l until r + 1
}


object Main {
  def read() = {
    val sc = new Scanner(System.in)
    val q = sc.nextInt()
    val ranges = for (_ <- 0 until q) yield Range(sc.nextInt(), sc.nextInt())
    (q, ranges)
  }

  def isPrimeAsTable: Vector[Boolean] = {
    val n = 100001
    val buf = Array.fill(n)(true)
    buf(0) = false
    buf(1) = false
    for (i <- 2 until math.sqrt(n).toInt) {
      for (j <- 2 * i until n by i) {
        buf(j) = false
      }
    }
    buf.toVector
  }

  def isPrime(n: Int): Boolean = {
    if (n <= 1) return false
    if (n == 2 || n == 3) return true
    val notPrime = (2 until math.sqrt(n).toInt + 1).exists(i => n % i == 0)
    return !notPrime
  }

  def like2017(n: Int): Boolean = isPrimeAsTable(n) && isPrimeAsTable((n + 1) / 2)

  def isOdd(n: Int) = n % 2 == 1

  def main(args: Array[String]): Unit = {
    val (q, ranges) = read()
    for (range <- ranges) {
      val result = range.toList.filter(isOdd).count(like2017)
      println(result)
    }
  }
}
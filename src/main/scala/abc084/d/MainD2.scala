package abc084.d

import java.util.Scanner

import scala.annotation.tailrec

// 失敗作 (素数列が上手くいかない)
/*
テーマ
素数判定(エラトステネスのふるい)
aからbまでの○○の数→ [a,b] /\ N→{0,1}を作ってそれぞれの和を計算
区間和→累積和を使う
 */


object MainD2 {
  def read() = {
    val sc = new Scanner(System.in)
    val q = sc.nextInt()
    val ranges = for (_ <- 0 until q) yield Range(sc.nextInt(), sc.nextInt())
    (q, ranges)
  }

  case class Range(l: Int, r: Int) {

    val toList: scala.Range = l until r + 1
  }

  // 10000くらい使おうとするとStackOverFlowError
  val primes: Stream[Int] = {
    def sieve(s: Stream[Int]): Stream[Int] = {
      // s.headは素数を想定
      // 素数で割り切れるものを除いていく
      s.head #:: sieve(s.tail.filterNot(x => x % s.head == 0))
    }

    sieve(Stream.from(2))
  }

  val isPrimeAsTable: Vector[Boolean] = {
    // gen({2,3,5,7...}) = {true, true, false, true,false, true,...}
    val n = 101
    val primeUntilN = primes.takeWhile(x => x < n)

    val primeBitmap = Array.fill(n)(false)
    primeUntilN.foreach(x => primeBitmap(x) = true)

    primeBitmap.toVector
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
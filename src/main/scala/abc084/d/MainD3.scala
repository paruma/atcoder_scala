package abc084.d

/*
テーマ
素数判定(エラトステネスのふるい)
aからbまでの○○の数→ [a,b] /\ N→{0,1}を作ってそれぞれの和を計算
区間和→累積和を使う
 */


import java.util.Scanner

object MainD3 {
  def read() = {
    val sc = new Scanner(System.in)
    val q = sc.nextInt()
    val ranges = for (_ <- 0 until q) yield Range(sc.nextInt(), sc.nextInt())
    (q, ranges)
  }

  case class Range(min: Int, max: Int) {
  }

  val numMax = 100001
  val isPrimeAsTable: Vector[Boolean] = {
    val n = numMax
    val primeBitmap = Array.fill(n)(true)
    primeBitmap(0) = false
    primeBitmap(1) = false

    for (i <- 2 until Math.sqrt(n).toInt) {
      if (primeBitmap(i)) {
        // i is prime
        val multipleOfI = (2 * i until n).filter(j => j % i == 0)
        multipleOfI.foreach(j => primeBitmap(j) = false)
      }
    }
    primeBitmap.toVector
  }

  def isOdd(n: Int): Boolean = n % 2 == 1

  def like2017(n: Int): Boolean = isOdd(n) && isPrimeAsTable(n) && isPrimeAsTable((n + 1) / 2)

  val isLike2017AsTable: Vector[Boolean] = {
    val n = numMax
    (0 until n).map(like2017).toVector
  }

  // cumulativeSum(Vector(a_0,a_1,a_2)) =
  // Vector
  // (0,
  // a_0,
  // a_0 + a_1,
  // a_0 + a_1 + a_2)
  def cumulativeSum[T](a: Vector[T])(implicit num: Numeric[T]): Vector[T] = {
    a.scanLeft(num.zero)(num.plus)
  }

  // countLike2017Until(n) = #{k | k<-{0,1,2...,n-1}, kはlike2017}
  val countLike2017Until: Vector[Int] = cumulativeSum(isLike2017AsTable.map(if (_) 1 else 0))

  def countLike2017(range: Range) = {
    countLike2017Until(range.max + 1) - countLike2017Until(range.min)
  }

  def main(args: Array[String]): Unit = {
    val (_, ranges) = read()
    for (range <- ranges) {
      val result = countLike2017(range)
      println(result)
    }
  }
}
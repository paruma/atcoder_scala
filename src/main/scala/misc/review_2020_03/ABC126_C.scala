package misc.review_2020_03

import java.util.Scanner

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

// 12:27~13:02
object ABC126_C {


  def read() = {
    val sc = new Scanner(System.in)
    val n, k = sc.nextInt()
    (n, k)
  }


  def solve(n: Int, k: Int): Double = {
    val nCoins = for (initPoint <- (1 to n)) yield {
      // initPoint: サイコロの目
      // nCoin: min [x | p * 2^x>= k]
      //

      @tailrec
      def calcNumCoin(point: Int, nCoin: Int): Int = {
        assert(point >= 1)
        assert(nCoin >= 0)
        if (point >= k) nCoin
        else calcNumCoin(point * 2, nCoin + 1)
      }

      calcNumCoin(initPoint, 0)
    }
    nCoins.map(k => math.pow(0.5, k)).sum/n
  }

  def main(args: Array[String]): Unit = {
    val (n, k) = read()
    val result = solve(n, k)
    println(result)
  }
}
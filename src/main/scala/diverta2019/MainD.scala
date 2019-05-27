package diverta2019

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer


object MainD {
  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextLong()
    n
  }


  def solve(n: Long): Long = {
    def isFavorite(k: Long): Boolean = k > 0 && n % k == n / k
    divisor(n).map(_ - 1).filter(isFavorite).sum
  }

  def experiment(n: Long): Unit = {
    def isFavorite(k: Int): Boolean = n % k == n / k
    for(k<- 1 to n.toInt){
      println(s"k=$k, (${n/k}, ${n%k}), ${isFavorite(k)}")
    }
  }

  def divisor(n: Long): IndexedSeq[Long] = {
    val buf = ArrayBuffer.empty[Long]
    for (i <- 1 to Math.sqrt(n).toInt) {
      if (n % i == 0) {
        buf.append(i)
        if (i * i != n) {
          buf.append(n / i)
        }
      }
    }
    buf.toIndexedSeq
  }

  def experiment2(n: Long): Unit = {
    def isFavorite(k: Int): Boolean = n % k == n / k
    def printInfo(k:Int):Unit = {
      println(s"k=$k, (${n/k}, ${n%k}), ${isFavorite(k)}")
    }
    (1 to n.toInt).filter(isFavorite).foreach(printInfo)

    println("divisor")
    divisor(n).sorted.foreach(println)
  }

  def main(args: Array[String]): Unit = {
    //experiment2(200)
    val n = read()
    println(solve(n))
  }
}
package abc125

import java.util.Scanner

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


object MainC {
  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val a = for (_ <- 0 until n) yield sc.nextLong()
    (n, a)
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

  def solve(n: Int, a: IndexedSeq[Long]): Long = {
    val cand = IndexedSeq.concat(divisor(a(0)), divisor(a(1))).distinct.sorted

    def satysfies(x: Long): Boolean = {
      //a.count(ae => ae % x == 0) >= a.length - 1
      a.count(_ % x == 0) >= a.length - 1
    }

    cand.filter(satysfies).max
  }

  @tailrec
  def gcd(x: Long, y: Long): Long = {
    assert(x >= 0 && y >= 0)
    if (y == 0) x else gcd(y, x % y)
  }

  def solve2(n: Int, a: IndexedSeq[Long]): Long = {
    val cumGCDLeft = a.scanLeft(0L)(gcd)
    val cumGCDRight = a.reverse.scanLeft(0L)(gcd).reverse// a.scanRight(0L)(gcd)

    // [0, end)
    def subCumGCDLeft(end: Int) = cumGCDLeft(end)

    // [start, n)
    def subCumGCDRight(start: Int) = cumGCDRight(start)

    // [0, i) + [i+1, n)
    (0 until n).map(i => gcd(subCumGCDLeft(i), subCumGCDRight(i + 1))).max
  }

  // O(n^2)愚直解
  def solve3(n: Int, a: IndexedSeq[Long]): Long = {
    // [0, end)
    def subCumGCDLeft(end: Int) = (0 until end).map(a).foldLeft(0L)(gcd)

    // [start, n)
    def subCumGCDRight(start: Int) = (start until n).map(a).foldLeft(0L)(gcd)

    // [0, i) + [i+1, n)
    (0 until n).map(i => gcd(subCumGCDLeft(i), subCumGCDRight(i + 1))).max
  }


  def main(args: Array[String]): Unit = {
    val (n, a) = read()
    println(solve2(n, a))
  }
}
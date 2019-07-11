package abc131

import java.util.Scanner

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


object MainC {


  def read() = {
    val sc = new Scanner(System.in)
    val a, b, c, d = sc.nextLong()
    (a, b, c, d)
  }

  @tailrec
  def gcd(x: Long, y: Long): Long = {
    assert(x >= 0 && y >= 0)
    if (y == 0) x else gcd(y, x % y)
  }

  def divCeil(a: Long, b: Long): Long = (a - 1) / b + 1

  def divFloor(a: Long, b: Long): Long = a / b


  def solve(a: Long, b: Long, c: Long, d: Long): Long = {

    def nDiv(x: Long) = divFloor(b, x) - divCeil(a, x) + 1


    val lcmCD = c / gcd(c, d) * d

    val nDivCorD = nDiv(c) + nDiv(d) - nDiv(lcmCD)
    (b - a + 1) - nDivCorD

  }

  def main(args: Array[String]): Unit = {
    val (a, b, c, d) = read()
    println(solve(a, b, c, d))
  }
}
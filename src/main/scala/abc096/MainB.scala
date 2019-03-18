package abc096

import java.util.Scanner

import scala.collection.mutable.ArrayBuffer


object MainB {
  def read() = {
    val sc = new Scanner(System.in)
    val a = sc.nextInt()
    val b = sc.nextInt()
    val c = sc.nextInt()
    val k = sc.nextInt()
    (a, b, c, k)
  }

  def max(a: Int, b: Int, c: Int): Int = Math.max(Math.max(a, b), c)

  def pow(x: Int, k: Int): Int = {
    if (k == 1) x
    else x * pow(x, k - 1)
  }

  def main(args: Array[String]): Unit = {
    val (a, b, c, k) = read()
    val maxvalue = max(a, b, c)
    // 黒板の書き換えで maxvalue * 2^k - maxvalue 増える
    val result = a + b + c + maxvalue * pow(2, k) - maxvalue
    println(result)
  }
}
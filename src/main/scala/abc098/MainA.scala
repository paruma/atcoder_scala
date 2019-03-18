package abc098

import java.util.Scanner


object MainA {
  def read() = {
    val sc = new Scanner(System.in)
    val a, b = sc.nextInt()
    (a, b)
  }

  def max(a: Int, b: Int, c: Int): Int = Math.max(Math.max(a, b), c)


  def main(args: Array[String]): Unit = {
    val (a, b) = read()
    val add = a + b
    val sub = a - b
    val mul = a * b
    val result = max(add, sub, mul)
    println(result)
  }
}
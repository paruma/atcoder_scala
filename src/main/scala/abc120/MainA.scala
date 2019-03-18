package abc120

import java.util.Scanner


object MainA {
  def read() = {
    val sc = new Scanner(System.in)
    val a = sc.nextInt()
    val b = sc.nextInt()
    val c = sc.nextInt()
    (a, b, c)
  }


  def main(args: Array[String]): Unit = {
    val (a, b, c) = read()
    println(Math.min(b / a, c))
  }
}
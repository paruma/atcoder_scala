package abc159

import java.util.Scanner

object MainC {

  def read() = {
    val sc = new Scanner(System.in)
    val l = sc.nextInt()
    l
  }

  def solve(l:Int): Double = {
    val edge = l.toDouble/3
    edge * edge * edge
  }

  def main(args: Array[String]): Unit = {
    val l = read()
    val result = solve(l)
    println(result)
    printf("%f", result)
    println()
  }
}
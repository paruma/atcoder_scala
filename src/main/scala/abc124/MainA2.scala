package abc124

import java.util.Scanner


object MainA2 {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val a, b = sc.nextInt()
    // a2回, aとb, b2回
    println(List(a + a - 1, a + b, b + b -1).max)
  }
}
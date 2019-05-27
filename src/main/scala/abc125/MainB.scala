package abc125

import java.util.Scanner


object MainB {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val v = for (_ <- 0 until n) yield sc.nextInt()
    val c = for (_ <- 0 until n) yield sc.nextInt()
    val result = (v, c).zipped.map(_ - _).filter(_ > 0).sum

    println(result)
  }
}
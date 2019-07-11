package abc128

import java.util.Scanner


object MainA {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val a, p = sc.nextInt()

    val pp = 3 * a + p
    val result = pp/2

    println(result)
  }
}
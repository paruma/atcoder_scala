package abc129

import java.util.Scanner


object MainA {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val p, q, r = sc.nextInt()
    val result = p + q + r - Seq(p, q, r).max

    println(result)
  }
}
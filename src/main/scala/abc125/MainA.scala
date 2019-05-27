package abc125

import java.util.Scanner


object MainA {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val a,b,t = sc.nextInt()

    println((t/a) * b)
  }
}
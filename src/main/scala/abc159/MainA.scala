package abc159

import java.util.Scanner


object MainA {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val n, m = sc.nextInt()
    val result = n * (n-1) /2 + m * (m-1)/2
    println(result)
  }
}
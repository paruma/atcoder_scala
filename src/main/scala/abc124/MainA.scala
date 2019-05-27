package abc124

import java.util.Scanner


object MainA {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val a,b = sc.nextInt()
    val big = Math.max(a,b)
    val small = Math.min(a,b)
    val result =
      if(big >= small + 1) big + (big -1)
      else big + small
    println(result)

  }
}
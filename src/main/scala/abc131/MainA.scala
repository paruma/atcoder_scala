package abc130

import java.util.Scanner


object MainA {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val x,a = sc.nextInt()
    val result = if(x < a) 0 else 10

    println(result)
  }
}
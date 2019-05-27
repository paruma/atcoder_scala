package abc127

import java.util.Scanner


object MainA {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val a, b = sc.nextInt()
    val result =
      if(0<= a && a <= 5) 0
      else if(6<= a && a<= 12) b/2
      else b
    println(result)
  }
}
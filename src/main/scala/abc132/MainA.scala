package abc131

import java.util.Scanner


object MainA {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val x = sc.next()
    val isGood = x(0) != x(1) && x(1) != x(2) && x(2) != x(3)

    println(if(isGood) "Good" else "Bad")
  }
}
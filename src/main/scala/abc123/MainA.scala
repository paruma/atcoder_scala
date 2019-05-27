package abc123

import java.util.Scanner


object MainA {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val posList = for (_ <- 0 until 5) yield sc.nextInt()
    val k = sc.nextInt()
    for (x1 <- posList) {
      for (x2 <- posList) {
        if (Math.abs(x1 - x2) > k) {
          println(":(")
          return
        }
      }
    }
    println("Yay!")
  }
}
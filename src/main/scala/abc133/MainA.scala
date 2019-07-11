package abc133

import java.util.Scanner


object MainA {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val n,a,b = sc.nextInt()
    val result = math.min(n*a,b)
    println(result)
  }
}
package diverta2019

import java.util.Scanner


object MainA {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val n,k = sc.nextInt()

    println(n-k+1)
  }
}
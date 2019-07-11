package diverta2019_2

import java.util.Scanner


object MainA {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val n, k = sc.nextInt()
    val result = if(k == 1) 0 else n-k

    println(result)
  }
}
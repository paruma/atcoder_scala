package abc131

import java.util.Scanner


object MainE {


  def read() = {
    val sc = new Scanner(System.in)
    val n, k = sc.nextInt()
    (n, k)
  }
  // 出力工夫

  def solve(n: Int, k: Int): Long = {
    0
  }

  def main(args: Array[String]): Unit = {
    val (n, k) = read()
    println(solve(n, k))
  }
}
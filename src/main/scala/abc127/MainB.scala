package abc127

import java.util.Scanner


object MainB {

  def read() = {
    val sc = new Scanner(System.in)
    val r, d, x = sc.nextInt()
    (r, d, x)
  }

  def solve(r: Int, d: Int, x: Long): IndexedSeq[Long] = {
    (0 until 10).scanLeft(x)((acc, i) => r * acc - d).tail
  }

  def main(args: Array[String]): Unit = {
    val (r, d, x) = read()
    solve(r, d, x).foreach(println)
  }
}
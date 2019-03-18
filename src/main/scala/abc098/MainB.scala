package abc098

import java.util.Scanner


object MainB {
  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val str = sc.next()
    (n, str)
  }

  // Stringに限らずOK (もう少し一般化できる)
  case class PairString(x: String, y: String) {
    def numContainedBoth(): Int = x.distinct.intersect(y.distinct).length
  }

  def main(args: Array[String]): Unit = {
    val (n, str) = read()
    val result =
      (1 until n) //Yの始まるindex
        .map(i => PairString(str.substring(0, i), str.substring(i, n))) // (X,Y)の全体
        .map(pair => pair.numContainedBoth())
        .max

    println(result)
  }
}

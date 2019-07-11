package abc131

import java.util.Scanner


object MainB {


  def read() = {
    val sc = new Scanner(System.in)
    val n, l = sc.nextInt()
    (n, l)
  }

  def solve(n: Int, l: Int): Long = {
    def aji(i: Int) = l + i - 1

    val sum = (1 to n).map(aji).sum
    (1 to n).map { i =>
      // i: 1つだけ食べるりんご
      val others = sum - aji(i)
      (math.abs(others - sum), others)
    }.minBy(_._1)._2
  }

  def main(args: Array[String]): Unit = {
    val (n, l) = read()
    println(solve(n, l))
  }
}
package diverta2019

import java.util.Scanner


object MainB {

  def solve(r: Int, g: Int, b: Int, n: Int) = {
    var cnt = 0
    for (nr <- 0 to n) {
      for (ng <- 0 to n) {
        val x = r * nr + g * ng
        val nb = (n - x) / b
        val remain = (n - x) % b
        if (nb >= 0 && remain == 0) {
          cnt += 1
        }
      }
    }
    cnt
  }

  def solve2(nBallR: Int, nBallG: Int, nBallB: Int, nBallNeed: Int) = {
    def divCeil(x: Int, y: Int) = (x - 1) / y + 1

    val maxNBoxR = divCeil(nBallNeed, nBallR)// これ普通に切り捨て割り算でいい。
    val maxNBoxG = divCeil(nBallNeed, nBallG)
    for (nBoxR <- 0 to maxNBoxR; nBoxG <- 0 to maxNBoxG) yield {

      val nBoxB = nBallNeed - nBoxR * nBallR + nBoxG * nBallG

    }
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val r, g, b, n = sc.nextInt()
    println(solve(r, g, b, n))
  }
}
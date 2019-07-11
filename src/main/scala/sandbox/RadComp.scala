package sandbox

import java.util.Scanner

object RadComp {


  def read() = {
    val sc = new Scanner(System.in)
    val m = sc.nextInt()
    m
  }

  def solve(m: Int): Double = {
    def hypo(begin: Int, end: Int, x: Int) = if (begin <= x && x < end) 1 else -1

    (for (pattern <- 0 until 1 << m) yield {
      val sigmas = (0 until m).map(i => (pattern >> i) & 1).map(x => if (x == 1) 1 else -1)
      // 区間: [begin, end) (begin <- [0, m], end <- [begin, m]
      (for (begin <- 0 to m; end <- begin to m) yield
        // [begin, end)
        sigmas.zipWithIndex.map { case (sigma, i) => sigma * hypo(begin, end, i) }.sum
        ).max
    }).sum.toDouble/m/math.pow(2, m)

  }


  def main(args: Array[String]): Unit = {
    //val m = read()
    for(m <- 1 to 17) {
      println(solve(m))
    }
  }

}
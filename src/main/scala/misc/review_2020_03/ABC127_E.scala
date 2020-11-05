package misc.review_2020_03

import java.util.Scanner

import scala.annotation.tailrec

// 16:13~
object ABC127_E {

  case class GF(representative: Long) extends AnyVal {
    def +(that: GF): GF = GF((this.representative + that.representative) % GF.mod)

    def -(that: GF): GF = GF((this.representative - that.representative + GF.mod) % GF.mod)

    def *(that: GF): GF = GF((this.representative * that.representative) % GF.mod)

    def /(that: GF): GF = this * that.inv

    def unary_- : GF = GF((-this.representative + GF.mod) % GF.mod)

    def inv: GF = GF.powGF(this, GF.mod - 2)
  }

  object GF {
    val mod: Long = (1e9 + 7).toLong

    val _0: GF = GF(0)
    val _1: GF = GF(1)

    def powGF(x: GF, t: Long): GF = {
      if (t == 0) {
        return GF(1)
      }
      val y = powGF(x, t / 2)
      if (t % 2 == 0) {
        y * y
      } else {
        x * y * y
      }
    }
  }

  val fracGF = (1 to 300000).map(GF(_)).scanLeft(GF(1))(_ * _)

  def combGF(n: Int, r: Int): GF = fracGF(n) / (fracGF(r) * fracGF(n - r))


  def read() = {
    val sc = new Scanner(System.in)
    val n, m, k = sc.nextInt()
    (n, m, k)
  }


  def solve(n: Int, m: Int, k: Int): Long = {
    // 0+1+...+p
    def sum(p: Int) = GF(p) * (GF(p) + GF(1)) / GF(2)

    (combGF(n * m - 2, k - 2) / GF(2) *
      (for (y <- 0 until n; x <- 0 until m) yield {
        GF(n) * (sum(x) + sum(m - x - 1)) + GF(m) * (sum(y) + sum(n - y - 1))
      }).foldLeft(GF._0)(_ + _)).representative
  }


  def main(args: Array[String]): Unit = {
    val (n, m, k) = read()
    val result = solve(n, m, k)
    println(result)
  }
}
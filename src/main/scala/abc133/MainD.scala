package abc132

import java.util.Scanner


object MainD {

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

  def read() = {
    val sc = new Scanner(System.in)
    val n, k = sc.nextInt()
    (n, k)
  }

  def solve(n: Int, k: Int): Unit = {


    val fracGF = (1 to n).map(GF(_)).scanLeft(GF(1))(_ * _)

    def combGF(n: Int, r: Int): GF = {
      if (n < r) GF(0)
      else fracGF(n) / (fracGF(r) * fracGF(n - r))
    }

    for (i <- 1 to k) {
      val result = combGF(k - 1, i - 1) * combGF(n - k + 1, i)
      println(result.representative)
    }

  }

  def main(args: Array[String]): Unit = {
    val (n, k) = read()
    solve(n, k)
    //println(solve(n,k))
  }
}
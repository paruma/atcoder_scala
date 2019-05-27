package abc127

import java.util.Scanner

// GF
object MainE2 {


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
    val n, m, k = sc.nextInt()
    (n, m, k)
  }

  def fracGF(n: Int): GF = (1 to n).map(GF(_)).foldLeft(GF(1))(_ * _)

  def combGF(n: Int, r: Int): GF = fracGF(n) / (fracGF(r) * fracGF(n - r))

  def solve2(h: Int, w: Int, k: Int): Long = {
    // 1 + 2 + ... + tを求める
    def sum(t: Long) = t * (t + 1) / 2

    // (x,y)から[0,w)*[0,h)の各点へのマンハッタン距離の総和
    def sumDist(x: Long, y: Long): GF = {
      // 0-origin
      // xより左側: [0,x) (xマス)
      // xより右側: [x+1,w) (w-x-1マス)
      // yより上側: [0,y) (yマス)
      // yより下側: [y+1, h) (h-y-1マス)
      GF(sum(x) + sum(w - x - 1)) * GF(h) + GF(sum(y) + sum(h - y - 1)) * GF(w)
    }

    val nComb = combGF(w * h - 2, k - 2)
    val distAll = (for (y <- 0 until h; x <- 0 until w) yield sumDist(x, y) / GF(2)).foldLeft(GF(0))(_ + _)
    val result = nComb * distAll
    result.representative
  }

  def main(args: Array[String]): Unit = {
    val (n, m, k) = read()
    println(solve2(n, m, k))
  }
}
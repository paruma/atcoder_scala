package abc127

import java.util.Scanner


object MainE {

  case class RR(representative: Long) extends AnyVal {
    def +(that: RR): RR = RR((this.representative + that.representative) % RR.mod)

    def -(that: RR): RR = RR((this.representative - that.representative + RR.mod) % RR.mod)

    def *(that: RR): RR = RR((this.representative * that.representative) % RR.mod)

    def unary_- : RR = RR((-this.representative + RR.mod) % RR.mod)
  }


  object RR {
    val mod: Long = (1e9 + 7).toLong

    val _0: RR = RR(0)
    val _1: RR = RR(1)
  }




  def read() = {
    val sc = new Scanner(System.in)
    val n, m, k = sc.nextInt()
    (n, m, k)
  }

  def comb(a: Int, b: Int): Int = {
    if (a < b) 0
    else if (b == 0) 1
    else comb(a - 1, b - 1) + comb(a - 1, b)
  }

  def fracRR(n: Int): RR = (1 to n).map(RR(_)).foldLeft(RR(1))(_ * _)


  def combRR(n: Int, r: Int): RR = {
    // comb(n,r) = n!/(r! (n-r)!)
    val x = fracRR(n) // n!
    val y = fracRR(r) // r!
    val z = fracRR(n - r) // (n-r)!

    val yz = y * z
    x * rrInv(yz)
  }

  def mypow(x: Long, t: Long): Long = {
    if (t == 0) {
      return 1
    }
    val y = mypow(x, t / 2)
    if (t % 2 == 0) {
      y * y
    } else {
      x * y * y
    }
  }

  def mypowRR(x: RR, t: Long): RR = {
    if (t == 0) {
      return RR(1)
    }
    val y = mypowRR(x, t / 2)
    if (t % 2 == 0) {
      y * y
    } else {
      x * y * y
    }
  }

  def rrInv(x: RR): RR = mypowRR(x, RR.mod - 2)

  def solve(h: Int, w: Int, k: Int): Int = {
    // 1 + 2 + ... + tを求める
    def sum(t: Int) = t * (t + 1) / 2

    def sumDist(x: Int, y: Int): Int = {
      // 0-origin
      // xより左側: [0,x) (xマス)
      // xより右側: [x+1,w) (w-x-1マス)
      // yより上側: [0,y) (yマス)
      // yより下側: [y+1, h) (h-y-1マス)
      (sum(x) + sum(w - x - 1)) * h + (sum(y) + sum(h - y - 1)) * w
    }

    comb(w * h - 2, k - 2) * (for (y <- 0 until h; x <- 0 until w) yield sumDist(x, y) / 2).sum
  }

  def solve2(h: Int, w: Int, k: Int): Long = {
    // 1 + 2 + ... + tを求める
    def sum(t: Long) = t * (t + 1) / 2

    def sumDist(x: Long, y: Long): RR = {
      // 0-origin
      // xより左側: [0,x) (xマス)
      // xより右側: [x+1,w) (w-x-1マス)
      // yより上側: [0,y) (yマス)
      // yより下側: [y+1, h) (h-y-1マス)
      RR(sum(x) + sum(w - x - 1)) * RR(h) + RR(sum(y) + sum(h - y - 1)) * RR(w)
    }

    val nComb = combRR(w * h - 2, k - 2)
    val distAll = (for (y <- 0 until h; x <- 0 until w) yield sumDist(x, y) * rrInv(RR(2))).foldLeft(RR(0))(_ + _)
    val result = nComb * distAll
    result.representative
  }

  def main(args: Array[String]): Unit = {
    val (n, m, k) = read()
    println(solve2(n, m, k))
  }
}
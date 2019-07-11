package msolutions2019

import java.util.Scanner


object MainC {


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
    val n, a, b, c = sc.nextInt()
    (n, a, b, c)
  }


  //def combGF(n: Int, r: Int): GF = fracGF(n) / (fracGF(r) * fracGF(n - r))

  def solve2(n: Int, a: Int, b: Int, c: Int): Long = {
    val p = GF(a) / GF(a + b)
    val q = GF(b) / GF(a + b)
    val fracGF = (1 to 2 * n).map(GF(_)).scanLeft(GF(1))(_ * _)

    def combGF(n: Int, r: Int): GF = fracGF(n) / (fracGF(r) * fracGF(n - r))

    //def fracGF(n: Int): GF = (1 to n).map(GF(_)).foldLeft(GF(1))(_ * _)
    val powQ = (0 to n).scanLeft(GF(1))((x, _) => x * q)
    val powP = (0 to n).scanLeft(GF(1))((x, _) => x * p)
    val powPN = GF.powGF(p, n)
    val powQN = GF.powGF(q, n)
    val result0 = (0 until n).map(i =>
      combGF(n + i - 1, i) * (powPN * powQ(i) + powQN * powP(i)) * GF(n + i)
    ).foldLeft(GF(0))(_ + _)
    val result1 = result0 * GF(100) / (GF(100) - GF(c))
    result1.representative
  }


  def solve22(n: Int, a: Int, b: Int, c: Int): Long = {
    val p = GF(a) / (GF(a) + GF(b))
    val q = GF(b) / (GF(a) + GF(b))

    //↓ 0 to 2*nにしがち
    val fracGF = (1 to 2 * n).map(GF(_)).scanLeft(GF(1))(_ * _)

    def combGF(n: Int, r: Int): GF = fracGF(n) / (fracGF(r) * fracGF(n - r))

    val powP = (0 to n).scanLeft(GF(1))((acc, _) => acc * p)
    val powQ = (0 to n).scanLeft(GF(1))((acc, _) => acc * q)

    val result0 = (0 until n).map(i =>
      combGF(n + i - 1, i) * (powP(n) * powQ(i) + powP(i) * powQ(n)) * GF(n + i)
    ).foldLeft(GF(0))(_ + _)
    val result1 = result0 / (GF(1) - GF(c) / GF(100))
    result1.representative
  }


  def solve3(n: Int, a: Int, b: Int, c: Int): Float = {
    val p = a.toFloat / (a + b)
    val q = b.toFloat / (a + b)
    val fracGF = (1 to 2 * n).map(_.toFloat).scanLeft(1F)(_ * _)

    def combGF(n: Int, r: Int): Float = fracGF(n) / (fracGF(r) * fracGF(n - r))

    //def fracGF(n: Int): GF = (1 to n).map(GF(_)).foldLeft(GF(1))(_ * _)
    val powQ = (0 to n).scanLeft(1F)((x, _) => x * q)
    val powP = (0 to n).scanLeft(1F)((x, _) => x * p)
    val powPN = math.pow(p, n).toFloat
    val powQN = math.pow(q, n).toFloat
    val result0 = (0 until n).map { i =>
      combGF(n + i - 1, i) * (powPN * powQ(i) + powQN * powP(i)) * (n + i)
    }.foldLeft(0F)(_ + _)
    val result1 = result0 * 100F / (100F - c)
    result1
  }

  def main(args: Array[String]): Unit = {
    val (n, a, b, c) = read()
    println(solve22(n, a, b, c))
  }
}
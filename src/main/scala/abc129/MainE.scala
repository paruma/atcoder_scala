package abc129

import java.util.Scanner


object MainE {


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

  def fracGF(n: Int): GF = (1 to n).map(GF(_)).foldLeft(GF(1))(_ * _)

  def combGF(n: Int, r: Int): GF = fracGF(n) / (fracGF(r) * fracGF(n - r))


  def read() = {
    val sc = new Scanner(System.in)
    val l = sc.next()
    l
  }

  def solve(l: String): Long = {
    val n = l.length
    val dp = Array.fill(n + 1)(GF(0))
    dp(0) = GF(1)
    for (i <- 1 to n) {
      if (l(i - 1) == '1') {
        dp(i) = GF(3) * dp(i - 1)
      } else {
        dp(i) = GF(3) * dp(i - 1) - (GF.powGF(GF(2), i)-GF(1))
      }
    }
    dp(n).representative
  }

  def solve2(l: String): Long = {
    val x = l.foldLeft(0)((acc, c) => if (c == '0') acc * 2 else acc * 2 + 1)
    for (x0 <- 0 to x) {
      println(s"x=$x0")
      var cnt = 0
      for (a <- 0 to x0; b <- 0 to x0) {
        if (a + b == x0 && a + b == (a ^ b)) {
          println(s"$a, $b")
          cnt += 1
        } else {
        }
      }
      println(s"cnt=$cnt")
    }
    (for (a <- 0 to x; b <- 0 to x; if a + b <= x) yield {
      if (a + b == (a ^ b)) 1 else 0
    }).sum
  }

  def main(args: Array[String]): Unit = {
    val l = read()
    println(solve(l))
  }
}
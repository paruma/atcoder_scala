package agc031

import java.util.Scanner


object MainA {

  case class ResidueRing(private val input: Long) {
    val representative: Long = ((input % ResidueRing.mod) + ResidueRing.mod) % ResidueRing.mod

    def +(that: ResidueRing): ResidueRing = ResidueRing(this.representative + that.representative)

    def -(that: ResidueRing): ResidueRing = ResidueRing(this.representative - that.representative)

    def *(that: ResidueRing): ResidueRing = ResidueRing(this.representative * that.representative)

    def unary_- : ResidueRing = ResidueRing(-this.representative)

  }

  object ResidueRing {
    val mod: Long = (1e9 + 7).toLong
    val zero: ResidueRing = ResidueRing(0)
    val one: ResidueRing = ResidueRing(1)
  }

  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val s = sc.next()
    (n, s)
  }

  def solve(n: Int, s: String): Long = {
    // 1 <= n <= 100,000
    val si = s.map(c => c - 'a')
    // 'a', 'b', ..., 'z' => 0, 1, ..., 25
    val nAlphabets = 26
    val countArray: Array[Int] = Array.fill(nAlphabets)(0)
    for (i <- si) {
      countArray(i) += 1
    }
    val result = countArray.map(i => ResidueRing(i)).map(r => r + ResidueRing.one).foldLeft(ResidueRing.one)(_ * _) - ResidueRing.one
    result.representative
  }

  def main(args: Array[String]): Unit = {
    val (n, s) = read()
    println(solve(n, s))
  }
}
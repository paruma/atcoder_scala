package agc031

import java.util.Scanner


object MainA2 {

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
    val result = s.groupBy(identity).map(_._2.length).map(i => ResidueRing(i)).map(r => r + ResidueRing.one).foldLeft(ResidueRing.one)(_ * _) - ResidueRing.one
    result.representative
  }

  def main(args: Array[String]): Unit = {
    val (n, s) = read()
    println(solve(n, s))
  }
}
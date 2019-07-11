package abc129

import java.util.Scanner


object MainC {

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
    val n, m = sc.nextInt()
    val a = IndexedSeq.fill(m)(sc.nextInt())

    (n, m, a)
  }

  def solve(n: Int, m: Int, a: IndexedSeq[Int]): Long = {
    val canUse = Array.fill(n + 1)(true)
    for (ai <- a) {
      canUse(ai) = false
    }

    val dp = Array.fill(n + 1)(RR(0))
    dp(0) = RR(1)
    dp(1) = if (canUse(1)) RR(1) else RR(0)
    for (i <- 2 to n) {
      if(canUse(i)){
        dp(i) = dp(i-2) + dp(i-1)
      }
    }
    dp(n).representative
  }

  def main(args: Array[String]): Unit = {
    val (n, m, a) = read()
    println(solve(n, m, a))
  }
}
package abc122

import java.util.Scanner

import scala.collection.mutable


object MainD2 {

  case class ResidueRing(private val input: Long) {
    val representative: Long = ((input % ResidueRing.mod) + ResidueRing.mod) % ResidueRing.mod

    def +(that: ResidueRing): ResidueRing = ResidueRing(this.representative + that.representative)

    def -(that: ResidueRing): ResidueRing = ResidueRing(this.representative - that.representative)

    def *(that: ResidueRing): ResidueRing = ResidueRing(this.representative * that.representative)

    def unary_- : ResidueRing = ResidueRing(-this.representative)

  }

  object ResidueRing {
    val mod: Long = (1e9 + 7).toLong
    val _0: ResidueRing = ResidueRing(0)
    val _1: ResidueRing = ResidueRing(1)
  }


  def read() = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    n
  }


  def solve(n: Int): Long = {
    // 過去3文字分記憶しておけばいい
    val alphabets = "ATGC"
    val stateList: IndexedSeq[String] =
      for (c1 <- alphabets;
           c2 <- alphabets;
           c3 <- alphabets) yield IndexedSeq(c1, c2, c3).mkString


    val regex = """.AGC|.GAC|.ACG|A.GC|AG.C""".r // コンパイルされる。
    def transition(state: String, alphabet: Char): Option[String] = {
      /*
      (state(0), state(1), state(2), alphabet) match {
        case (_, 'A', 'G', 'C')
             | (_, 'G', 'A', 'C')
             | (_, 'A', 'C', 'G')
             | ('A', _, 'G', 'C')
             | ('A', 'G', _, 'C') => None
        case _ => Some(IndexedSeq(state(1), state(2), alphabet).mkString)
      }
      */
      if (regex.findFirstIn(state + alphabet).nonEmpty) None
      else Some(state.tail + alphabet)
    }

    val mapAll0: mutable.Map[String, ResidueRing] = mutable.Map.empty
    for (state <- stateList) {
      mapAll0.+=((state, ResidueRing._0))
    }
    val dp: Array[mutable.Map[String, ResidueRing]] = Array.fill(n + 1)(mapAll0.clone)

    dp(0)("TTT") += ResidueRing._1
    for (i <- 0 until n) {
      for (state <- stateList) {
        for (alphabet <- alphabets) {
          val next = transition(state, alphabet)
          next match {
            case Some(nextState) => dp(i + 1)(nextState) += dp(i)(state)
            case None =>
          }

        }
      }
    }
    val result = dp.last.values.foldLeft(ResidueRing._0)(_ + _)
    result.representative
  }


  def main(args: Array[String]): Unit = {
    val n = read()
    println(solve(n))
  }
}
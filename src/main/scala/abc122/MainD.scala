package abc122

import java.util.Scanner

import scala.collection.mutable


object MainD {

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
    sealed trait State
    final case object S_A extends State
    final case object S_AG extends State
    final case object S_AGT extends State
    final case object S_AGG extends State
    final case object S_AT extends State
    final case object S_ATG extends State
    final case object S_AC extends State
    final case object S_G extends State
    final case object S_GA extends State
    final case object S_TorC extends State

    sealed trait Alphabet
    final case object A extends Alphabet
    final case object T extends Alphabet
    final case object G extends Alphabet
    final case object C extends Alphabet

    def transition(state: State, alphabet: Alphabet): Option[State] = {
      state match {
        case S_A => alphabet match {
          case A => Some(S_A)
          case T => Some(S_AT)
          case G => Some(S_AG)
          case C => Some(S_AC)
        }
        case S_AG => alphabet match {
          case A => Some(S_GA)
          case T => Some(S_AGT)
          case G => Some(S_AGG)
          case C => None
        }
        case S_AGT => alphabet match {
          case A => Some(S_A)
          case T => Some(S_TorC)
          case G => Some(S_G)
          case C => None
        }
        case S_AGG => alphabet match {
          case A => Some(S_GA)
          case T => Some(S_TorC)
          case G => Some(S_G)
          case C => None
        }
        case S_AT => alphabet match {
          case A => Some(S_A)
          case T => Some(S_TorC)
          case G => Some(S_ATG)
          case C => Some(S_TorC)
        }
        case S_ATG => alphabet match {
          case A => Some(S_GA)
          case T => Some(S_TorC)
          case G => Some(S_G)
          case C => None
        }
        case S_AC => alphabet match {
          case A => Some(S_A)
          case T => Some(S_TorC)
          case G => None
          case C => Some(S_TorC)
        }
        case S_G => alphabet match {
          case A => Some(S_GA)
          case T => Some(S_TorC)
          case G => Some(S_G)
          case C => Some(S_TorC)
        }
        case S_GA => alphabet match {
          case A => Some(S_A)
          case T => Some(S_AT) //ここS_TorCにしていた(ミス)
          case G => Some(S_AG)
          case C => None
        }
        case S_TorC => alphabet match {
          case A => Some(S_A)
          case T => Some(S_TorC)
          case G => Some(S_G)
          case C => Some(S_TorC)
        }
      }
    }

    val stateList = List(S_A, S_AG, S_AGT, S_AGG, S_AT, S_ATG, S_AC, S_G, S_GA, S_TorC)
    val alphabetList = List(A, T, G, C)

    val mapAll0: mutable.Map[State, ResidueRing] = mutable.Map.empty
    for (state <- stateList) {
      mapAll0.+=((state, ResidueRing._0))
    }
    val dp: Array[mutable.Map[State, ResidueRing]] = Array.fill(n + 1)(mapAll0.clone)

    dp(0)(S_TorC) += ResidueRing._1
    for (i <- 0 until n) {
      for (state <- stateList) {
        for (alphabet <- alphabetList) {
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
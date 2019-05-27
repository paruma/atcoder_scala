package abc122

import java.util.Scanner

import scala.collection.mutable


object MainDEasy {

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

  def power(b: Int, n: Int): ResidueRing = List.fill(n)(ResidueRing(b)).foldLeft(ResidueRing._1)(_ * _)

  def solve(n: Int): Int = {
    0
  }

  //全状態受理・決定性有限オートマトン
  case class Automaton[State, Alphabet](transition: (State, Alphabet) => Option[State], initialState: State) {
    def accepts(str: List[Alphabet]): Boolean = {
      def acceptsSub(str: List[Alphabet], state: State): Boolean = {
        if (str.isEmpty) return true

        val next = transition(state, str.head)
        next match {
          case Some(nextState) => acceptsSub(str.tail, nextState)
          case None => false
        }
      }

      acceptsSub(str, initialState)
    }
  }

  def solveEasy(n: Int): Int = {
    sealed trait State

    final case object S_A extends State
    final case object S_AG extends State
    final case object S_TorGorC extends State

    sealed trait Alphabet

    final case object A extends Alphabet
    final case object T extends Alphabet
    final case object G extends Alphabet
    final case object C extends Alphabet

    def transition(state: State, alphabet: Alphabet): Option[State] = {
      state match {
        case S_A => alphabet match {
          case A => Some(S_A)
          case G => Some(S_AG)
          case T | C => Some(S_TorGorC)
        }
        case S_AG => alphabet match {
          case A => Some(S_A)
          case T | G => Some(S_TorGorC)
          case C => None
        }
        case S_TorGorC => alphabet match {
          case A => Some(S_A)
          case T | G | C => Some(S_TorGorC)
        }
      }
    }

    val automaton = Automaton(transition, initialState = S_TorGorC)

    def charToAlphabet(c: Char): Option[Alphabet] = {
      c match {
        case 'A' => Some(A)
        case 'T' => Some(T)
        case 'G' => Some(G)
        case 'C' => Some(C)
        case _ => None
      }
    }

    //気持ち悪い(getあたりが特に)
    def stringToAlphabetList(str: String): Option[List[Alphabet]] = {
      val alphabetList: List[Option[Alphabet]] = str.toList.map(charToAlphabet)
      if (alphabetList.contains(None)) None
      else Some(alphabetList.map(x => x.get))
    }

    val stateList = List(S_A, S_AG, S_TorGorC)
    val alphabetList = List(A, T, G, C)

    val mapAll0: mutable.Map[State, Int] = mutable.Map.empty
    for (state <- stateList) {
      mapAll0.+=((state, 0))
    }
    val dp: Array[mutable.Map[State, Int]] = Array.fill(n + 1)(mapAll0.clone)

    dp(0)(S_TorGorC) += 1
    for (i <- 0 until n) {
      for (state <- stateList) {
        for (alphabet <- alphabetList) {
          val next = transition(state, alphabet)
          next match {
            case Some(nextState) => dp(i + 1)(nextState) += dp(i)(state)
            case None => //これないと""実行時エラー""
          }

        }
      }
    }
    dp.last.values.sum
  }


  def main(args: Array[String]): Unit = {
    val n = read()
    //println(solve(n))
    for(i<- 1 until 10){
      println(solveEasy(i))

    }
    //println("AGCC".contains("AGC"))
  }
}
package abc126

import java.util.Scanner


object MainB {
  def charToInt(c: Char) = c - '0'

  def solve(s: String): String = {

    def isYYMM(a: Int, b: Int) = (0 <= a && a <= 99) && (1 <= b && b <= 12)

    def isMMYY(a: Int, b: Int) = isYYMM(b, a)

    val a = charToInt(s(0)) * 10 + charToInt(s(1))
    val b = charToInt(s(2)) * 10 + charToInt(s(3))
    (isYYMM(a, b), isMMYY(a, b)) match {
      case (true, true) => "AMBIGUOUS"
      case (true, false) => "YYMM"
      case (false, true) => "MMYY"
      case (false, false) => "NA"
    }
  }

  def solve2(s: String): String = {

    def isYY(x: Int) = (0 to 99).contains(x)

    def isMM(x: Int) = (1 to 12).contains(x)

    def isYYMM(a: Int, b: Int) = isYY(a) && isMM(b)

    def isMMYY(a: Int, b: Int) = isMM(a) && isYY(b)

    val a = charToInt(s(0)) * 10 + charToInt(s(1))
    val b = charToInt(s(2)) * 10 + charToInt(s(3))
    (isYYMM(a, b), isMMYY(a, b)) match {
      case (true, true) => "AMBIGUOUS"
      case (true, false) => "YYMM"
      case (false, true) => "MMYY"
      case (false, false) => "NA"
    }
  }


  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val s = sc.next()

    println(solve(s))
  }
}
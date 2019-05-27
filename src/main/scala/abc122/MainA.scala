package abc122

import java.util.Scanner


object MainA {
  def read() = {
    val sc = new Scanner(System.in)
    val c = sc.next()(0)
    c
  }

  def solve(c:Char): Char = {
    c match{
      case 'A' => 'T'
      case 'T' => 'A'
      case 'G' => 'C'
      case 'C' => 'G'
      case _ => 'X'
    }
  }


  def main(args: Array[String]): Unit = {
    val c = read()
    println(solve(c))
  }
}